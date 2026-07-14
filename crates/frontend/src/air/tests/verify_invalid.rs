use verify::{
    BadCall, BadConst, BadContract, BadExtern, BadFunction, BadModule, BadPlace, BadRValue,
    BadReference, BadStatement, BadType, ModuleItem, PrimitiveKind, VerifyError,
    VerifyErrorKind as EK, VerifySite,
};

use super::{super::verify::verify_structured_body, *};
use crate::{
    air::{
        AirPatternAlternative, AirPatternArm, AirPatternBinding, AirPatternBindingMode,
        AirPatternMatch, AirPatternPath, AirPatternPathStep, AirPatternTest, ContractParamDecl,
        ContractReceiver, ContractReturnDecl, ContractSlotDecl, ContractSlotId,
        ContractSurfaceDecl, ContractSurfaceId, ContractWeakeningDecl, ContractWitnessDecl,
        ContractWitnessKey, ContractWitnessSlotDecl, ContractWitnessTarget, ExternBindingDecl,
        ExternStaticDecl, ExternTypeId, FunctionSpecialization, FunctionValueCapability,
        GlobalInitEffect, Module,
    },
    ast::Ident,
};

fn field(name: &str, ty: TypeId) -> FieldDecl {
    FieldDecl {
        name: Ident::new(name),
        ty,
    }
}

fn verify_void_entry(
    mut builder: ProgramBuilder,
    name: &str,
    module: ModuleId,
    void_ty: TypeId,
    build: impl FnOnce(&mut FunctionBuilder, BlockId),
) -> Vec<VerifyError> {
    let mut fb = FunctionBuilder::new(name, module, FunctionKind::Normal, void_ty);
    let bb0 = fb.push_block(term_return_void());
    build(&mut fb, bb0);
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    verify(&builder.finish()).unwrap_err()
}

#[test]
fn entry_function_out_of_range() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("f", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_return_void());
    builder.alloc_function(fb.finish());
    builder.set_entry(FunctionId::from_index(999));

    assert!(verify(&builder.finish()).is_err());
}

#[test]
fn structured_if_not_bool() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad", module, FunctionKind::Normal, void_ty);
    let cond = fb.push_param("cond", int_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::If(AirIf {
                cond: op_place(cond, int_ty),
                then_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(None),
                },
                else_block: None,
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::IfCondMustBeBool(t)) if t == int_ty
    )));
}

#[test]
fn structured_branch_result_missing_on_one_path() {
    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.bool_ty();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("missing", module, FunctionKind::Normal, int_ty);
    let cond = fb.push_param("cond", bool_ty, ParamRole::Normal);
    let out = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_return(op_const(one)));
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::If(AirIf {
                cond: op_place(cond, bool_ty),
                then_block: AirBlock {
                    stmts: vec![AirStmt::Init {
                        local: out,
                        value: RValue::Use(op_const(one)),
                    }],
                    tail: AirTail::None,
                },
                else_block: None,
            })],
            tail: AirTail::Return(Some(op_place(out, int_ty))),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(local)) if local == out
    )));
}

#[test]
fn lambda_callee_read_must_be_initialized() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();
    let lambda_ty = builder.alloc_type(TypeData::Function(SignatureType {
        params: vec![],
        ret: ReturnMode::Value(void_ty),
    }));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_lambda", module, FunctionKind::Normal, void_ty);
    let lambda = fb.push_local(None, lambda_ty, Mutability::Immutable, LocalKind::User);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::Eval(RValue::Call {
                callee: Callee::Lambda(op_place(lambda, lambda_ty)),
                args: vec![],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(local)) if local == lambda
    )));
}

#[test]
fn structured_rejects_init_and_assign_errors() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("bad_init", module, FunctionKind::Normal, void_ty);
    let local = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let uninit = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::Temp);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![
                AirStmt::Init {
                    local,
                    value: RValue::Use(op_const(one)),
                },
                AirStmt::Init {
                    local,
                    value: RValue::Use(op_const(one)),
                },
                AirStmt::Assign {
                    dst: place(uninit, int_ty),
                    value: RValue::Use(op_const(one)),
                },
            ],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::InitImmutableLocalTwice(found)) if found == local
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::AssignUninitializedLocal(found)) if found == uninit
    )));
}

#[test]
fn structured_break_continue_outside_loop() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_tail", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_return_void());
    let break_id = builder.alloc_function(fb.finish());
    let break_body = AirBody {
        block: AirBlock {
            stmts: vec![],
            tail: AirTail::Break(AirLoopId::from_index(0)),
        },
    };
    let continue_body = AirBody {
        block: AirBlock {
            stmts: vec![],
            tail: AirTail::Continue(AirLoopId::from_index(0)),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, break_id, &break_body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::BreakOutsideLoop(AirLoopId(0)))
    )));
    let errors = verify_structured_body(&program, break_id, &continue_body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ContinueOutsideLoop(AirLoopId(0)))
    )));
}

#[test]
fn structured_return_and_match_errors() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let module = test_module(&mut builder);
    let yes = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let mut fb = FunctionBuilder::new("bad_return", module, FunctionKind::Normal, int_ty);
    fb.push_block(term_return(op_const(yes)));
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![],
            tail: AirTail::Return(Some(op_const(yes))),
        },
    };
    let program = builder.finish();
    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ReturnedTypeMismatch { expected, found })
            if expected == int_ty && found == bool_ty
    )));

    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.bool_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let yes = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let mut fb = FunctionBuilder::new("bad_void", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![],
            tail: AirTail::Return(Some(op_const(yes))),
        },
    };
    let program = builder.finish();
    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::VoidFunctionMustReturnNone)
    )));
}

#[test]
fn structured_slice_bounds_must_be_initialized() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_slice", module, FunctionKind::Normal, void_ty);
    let list = fb.push_param("list", list_ty, ParamRole::Normal);
    let start = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let end = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::Eval(RValue::RangeListCopy {
                source: place(list, list_ty),
                start,
                end,
                inclusive: false,
                ty: list_ty,
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(found)) if found == start
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(found)) if found == end
    )));
}

#[test]
fn return_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_return", module, FunctionKind::Normal, int_ty);
    let local_b = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::Return);
    fb.push_block(term_return(op_place(local_b, bool_ty)));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ReturnedTypeMismatch { expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn place_return_must_return_place() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);
    let value = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });

    let func = Function {
        name: Ident::new("bad_place_return"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::with_return_mode(vec![], ReturnMode::Place(int_ty)),
        locals: vec![],
        body: body_from_block(AirBlock {
            stmts: vec![],
            tail: term_return(op_const(value)),
        }),
    };
    let fid = builder.alloc_function(func);
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::PlaceReturnMustReturnPlace)
    )));
}

#[test]
fn void_fn_returns_value() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("void_bad", module, FunctionKind::Normal, void_ty);
    let local_i = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    fb.push_block(term_return(op_place(local_i, int_ty)));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::VoidFunctionMustReturnNone)
    )));
}

#[test]
fn nonvoid_fn_returns_none() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("non_void_bad", module, FunctionKind::Normal, int_ty);
    fb.push_block(term_return_void());
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::NonVoidFunctionMustReturnValue(ret)) if ret == int_ty
    )));
}

#[test]
fn fn_bad_type() {
    let mut builder = ProgramBuilder::default();
    builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let func = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], TypeId::from_index(999)),
        locals: vec![],
        body: body_from_block(AirBlock {
            stmts: vec![],
            tail: AirTail::Return(None),
        }),
    };
    let fid = builder.alloc_function(func);
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(TypeId(id))) if id == 999
    )));
}

#[test]
fn function_specialization_type_must_exist() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let func = Function {
        name: Ident::new("f"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: Some(FunctionSpecialization {
            type_args: vec![TypeId::from_index(999)],
            const_args: vec![],
        }),
        signature: Signature::new(vec![], void_ty),
        locals: vec![],
        body: body_from_block(AirBlock {
            stmts: vec![],
            tail: AirTail::Return(None),
        }),
    };
    builder.alloc_function(func);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(TypeId(id))) if id == 999
    )));
}

#[test]
fn aggregate_bad_field_type() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);

    builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("BadAgg"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![field("f", TypeId::from_index(999))],
        cycle_capable: false,
        stringify_override: None,
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(TypeId(id))) if id == 999
    )));
}

#[test]
fn enum_bad_variant_type() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);

    builder.alloc_enum(EnumDecl {
        name: Ident::new("BadEnum"),
        module,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![VariantDecl {
            name: Ident::new("V"),
            shape: VariantShape::Tuple(vec![TypeId::from_index(888)]),
            raw_value: None,
        }],
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(TypeId(id))) if id == 888
    )));
}

#[test]
fn tuple_bad_nested_type_does_not_panic() {
    let mut builder = ProgramBuilder::default();
    builder.alloc_type(TypeData::Tuple(vec![TypeId::from_index(999)]));

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(TypeId(id))) if id == 999
    )));
}

#[test]
fn function_call_arg_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let mut callee = FunctionBuilder::new("takes_int", module, FunctionKind::Normal, void_ty);
    callee.push_param("value", int_ty, ParamRole::Normal);
    callee.push_block(term_return_void());
    let callee_id = builder.alloc_function(callee.finish());

    let mut caller = FunctionBuilder::new("bad_call", module, FunctionKind::Normal, void_ty);
    let p_arg = caller.push_param("arg", bool_ty, ParamRole::Normal);
    let bb0 = caller.push_block(term_return_void());
    caller.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee_id),
            args: vec![CallArg::Value(op_place(p_arg, bool_ty))],
        }),
    );
    let caller_id = builder.alloc_function(caller.finish());
    builder.set_entry(caller_id);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgTypeMismatch { index: 0, expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn extern_member_receiver_mode_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let ext_ty = builder.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Handle"),
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
    let receiver_ty = builder.alloc_type(TypeData::Extern(ext_ty));
    builder.alloc_extern(ExternDecl {
        name: Ident::new("get_x"),
        module,
        member: ExternMember::FieldGetter {
            owner: ext_ty,
            receiver: ExternReceiverDecl {
                ty: receiver_ty,
                mode: ParamMode::MutBorrow,
            },
            computed: false,
        },
        params: vec![],
        return_type: int_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });
    let errors = verify_void_entry(builder, "main", module, void_ty, |_, _| {});
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, EK::BadExtern(BadExtern::ReceiverModeMismatch)))
    );
}

#[test]
fn extern_rejects_return_only_abi_in_param() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    builder.alloc_extern(ExternDecl {
        name: Ident::new("bad"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: int_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: void_ty,
        abi: crate::air::ExternAbi {
            params: vec![anvyx_externs::ExternTypeExpr::Void],
            ret: anvyx_externs::ExternTypeExpr::Void,
        },
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let errors = verify_void_entry(builder, "main", module, void_ty, |_, _| {});
    assert!(errors.iter().any(|e| {
        matches!(
            e.kind,
            EK::BadExtern(BadExtern::InvalidAbi {
                reason: anvyx_externs::AbiTypeError::VoidOutsideReturn,
                ..
            })
        )
    }));
}

#[test]
fn extern_binding_must_match_decl_identity() {
    use anvyx_externs::{
        ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternFunctionKey, ModulePath,
        ProviderId,
    };

    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    builder.alloc_extern(ExternDecl {
        name: Ident::new("host_log"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: Some(ExternBindingDecl {
            package: crate::resolve::PackageId::synthetic_root(),
            provider: ProviderId {
                name: "host".to_string(),
            },
            key: ExternBindingKey {
                target: ExternBindingTarget::Function(ExternFunctionKey {
                    module: ModulePath {
                        segments: vec!["host".to_string()],
                    },
                    name: "other".to_string(),
                }),
                operation: ExternBindingOp::Call,
            },
        }),
        effects: anvyx_externs::ExternEffects::default(),
    });

    let errors = verify_void_entry(builder, "main", module, void_ty, |_, _| {});
    assert!(
        errors
            .iter()
            .any(|e| { matches!(e.kind, EK::BadExtern(BadExtern::BindingMismatch)) })
    );
}

#[test]
fn extern_binding_with_invalid_owner_does_not_panic() {
    use anvyx_externs::{
        ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternMemberKey,
        ExternMemberSelector, ExternTypeKey, ModulePath, ProviderId,
    };

    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let receiver_ty = builder.alloc_type(TypeData::Extern(ExternTypeId::from_index(99)));
    builder.alloc_extern(ExternDecl {
        name: Ident::new("x"),
        module,
        member: ExternMember::FieldGetter {
            owner: ExternTypeId::from_index(99),
            receiver: ExternReceiverDecl {
                ty: receiver_ty,
                mode: ParamMode::SharedBorrow,
            },
            computed: false,
        },
        params: vec![],
        return_type: int_ty,
        abi: crate::air::ExternAbi::default(),
        binding: Some(ExternBindingDecl {
            package: crate::resolve::PackageId::synthetic_root(),
            provider: ProviderId {
                name: "host".to_string(),
            },
            key: ExternBindingKey {
                target: ExternBindingTarget::Member(ExternMemberKey {
                    owner: ExternTypeKey {
                        module: ModulePath {
                            segments: vec!["host".to_string()],
                        },
                        name: "Handle".to_string(),
                    },
                    selector: ExternMemberSelector::Field("x".to_string()),
                }),
                operation: ExternBindingOp::Get,
            },
        }),
        effects: anvyx_externs::ExternEffects::default(),
    });

    let errors = verify_void_entry(builder, "main", module, void_ty, |_, _| {});
    assert!(errors.iter().any(|e| {
        matches!(
            e.kind,
            EK::BadReference(BadReference::InvalidExternType(id)) if id == ExternTypeId::from_index(99)
        )
    }));
}

#[test]
fn escaping_extern_param_must_be_function() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    builder.alloc_extern(ExternDecl {
        name: Ident::new("retain"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: int_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::Escaping,
        }],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let errors = verify_void_entry(builder, "main", module, void_ty, |_, _| {});
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadExtern(BadExtern::EscapingParamMustBeFunction(0))
    )));
}

#[test]
fn escaping_extern_param_must_be_by_value() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let callback_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![],
        ReturnMode::Value(void_ty),
    )));
    let module = test_module(&mut builder);

    builder.alloc_extern(ExternDecl {
        name: Ident::new("retain"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: callback_ty,
            mode: ParamMode::SharedBorrow,
            escape: ParamEscape::Escaping,
        }],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let errors = verify_void_entry(builder, "main", module, void_ty, |_, _| {});
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadExtern(BadExtern::EscapingParamMustBeValue(0))
    )));
}

#[test]
fn escaping_extern_type_params_use_same_invariants() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let callback_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![],
        ReturnMode::Value(void_ty),
    )));
    let module = test_module(&mut builder);

    builder.alloc_extern_type(ExternTypeDecl {
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
        statics: vec![ExternStaticDecl {
            name: Ident::new("retain"),
            params: vec![
                ExternParamDecl {
                    ty: int_ty,
                    mode: ParamMode::Value,
                    escape: ParamEscape::Escaping,
                },
                ExternParamDecl {
                    ty: callback_ty,
                    mode: ParamMode::SharedBorrow,
                    escape: ParamEscape::Escaping,
                },
            ],
            return_type: void_ty,
            abi: crate::air::ExternAbi::default(),
        }],
        operators: vec![],
    });

    let errors = verify_void_entry(builder, "main", module, void_ty, |_, _| {});
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadExtern(BadExtern::EscapingParamMustBeFunction(0))
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadExtern(BadExtern::EscapingParamMustBeValue(1))
    )));
}

#[test]
fn call_arity_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("ext_add"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![
            ExternParamDecl {
                ty: int_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
            },
            ExternParamDecl {
                ty: int_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
            },
        ],
        return_type: int_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let mut fb = FunctionBuilder::new("arity_bad", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![CallArg::Value(op_place(p_n, int_ty))],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArityMismatch {
            expected: 2,
            found: 1
        })
    )));
}

#[test]
fn call_value_arg_invalid_type_does_not_panic() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();
    let invalid_ty = TypeId::from_index(999);
    let module = test_module(&mut builder);

    let mut callee = FunctionBuilder::new("takes_invalid", module, FunctionKind::Normal, void_ty);
    callee.push_param("value", invalid_ty, ParamRole::Normal);
    callee.push_block(term_return_void());
    let callee_id = builder.alloc_function(callee.finish());

    let mut caller = FunctionBuilder::new("bad_call", module, FunctionKind::Normal, void_ty);
    let arg = caller.push_local(None, invalid_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = caller.push_block(term_return_void());
    caller.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee_id),
            args: vec![CallArg::Value(op_place(arg, invalid_ty))],
        }),
    );
    let fid = builder.alloc_function(caller.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(ty)) if ty == invalid_ty
    )));
}

#[test]
fn shared_string_const_invalid_id_does_not_panic() {
    let mut builder = ProgramBuilder::default();
    let string_ty = builder.string_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("print"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: string_ty,
            mode: ParamMode::SharedBorrow,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let mut fb = FunctionBuilder::new("bad_call", module, FunctionKind::Normal, void_ty);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![CallArg::SharedStringConst(ConstId::from_index(999))],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidConst(ConstId(id))) if id == 999
    )));
}

#[test]
fn call_arg_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("ext_add"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![
            ExternParamDecl {
                ty: int_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
            },
            ExternParamDecl {
                ty: int_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
            },
        ],
        return_type: int_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let mut fb = FunctionBuilder::new("arg_type_bad", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let p_b = fb.push_param("b", bool_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![
                CallArg::Value(op_place(p_n, int_ty)),
                CallArg::Value(op_place(p_b, bool_ty)),
            ],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgTypeMismatch { index: 1, expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn call_arg_function_escape_mismatch() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let callback_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![],
        ReturnMode::Value(void_ty),
    )));
    let non_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![ParamType {
            ty: callback_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        ReturnMode::Value(void_ty),
    )));
    let esc_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![ParamType {
            ty: callback_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::Escaping,
        }],
        ReturnMode::Value(void_ty),
    )));
    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("accept"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: esc_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let mut fb = FunctionBuilder::new("arg_escape_bad", module, FunctionKind::Normal, void_ty);
    let f = fb.push_param("f", non_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![CallArg::Value(op_place(f, non_ty))],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgTypeMismatch { index: 0, expected, found })
            if expected == esc_ty && found == non_ty
    )));
}

fn invalid_root_errors(root: PlaceRoot) -> Vec<VerifyError> {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_root", module, FunctionKind::Normal, int_ty);
    fb.push_block(term_return(Operand::Place(Place {
        root,
        projection: vec![],
        ty: int_ty,
    })));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    verify(&builder.finish()).unwrap_err()
}

#[test]
fn non_local_call_arg_alias_is_conservative() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("takes_borrows"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![
            ExternParamDecl {
                ty: int_ty,
                mode: ParamMode::SharedBorrow,
                escape: ParamEscape::NonEscaping,
            },
            ExternParamDecl {
                ty: int_ty,
                mode: ParamMode::MutBorrow,
                escape: ParamEscape::NonEscaping,
            },
        ],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let mut fb = FunctionBuilder::new("alias", module, FunctionKind::Normal, void_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![
                CallArg::SharedBorrow(Place {
                    root: PlaceRoot::ScopedBorrow(scoped),
                    projection: vec![],
                    ty: int_ty,
                }),
                CallArg::MutBorrow(place(local, int_ty)),
            ],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgAliasConflict {
            first: 0,
            second: 1
        })
    )));
}

#[test]
fn scoped_borrow_source_local_read_bypasses_promoted_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));

    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, binding);
    fb.push_block(term_return(op_place(local, int_ty)));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::PromotedBindingBypassesScopedBorrow { binding: found, local: found_local, .. })
            if found == binding && found_local == local
    )));
}

#[test]
fn scoped_borrow_source_local_write_bypasses_promoted_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));

    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, void_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, binding);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_assign(place(local, int_ty), RValue::Use(op_const(one))),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::PromotedBindingBypassesScopedBorrow { binding: found, local: found_local, .. })
            if found == binding && found_local == local
    )));
}

#[test]
fn same_scoped_borrow_passed_twice_conflicts() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);

    let mut callee = FunctionBuilder::new("both", module, FunctionKind::Normal, void_ty);
    callee.push_param_with_mode("a", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_param_with_mode("b", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    let both = builder.alloc_function(callee.finish());

    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(1),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, void_ty);
    let local = owner.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(local, binding);
    let bb0 = owner.push_block(term_return_void());
    let place = Place {
        root: PlaceRoot::ScopedBorrow(scoped),
        projection: vec![],
        ty: int_ty,
    };
    owner.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(both),
            args: vec![CallArg::MutBorrow(place.clone()), CallArg::MutBorrow(place)],
        }),
    );
    builder.alloc_function(owner.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgAliasConflict {
            first: 0,
            second: 1
        })
    )));
}

#[test]
fn same_lambda_scoped_borrow_slot_passed_twice_conflicts() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);

    let mut callee = FunctionBuilder::new("both", module, FunctionKind::Normal, void_ty);
    callee.push_param_with_mode("a", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_param_with_mode("b", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    let both = builder.alloc_function(callee.finish());

    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(2),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(1);
    builder.alloc_lambda(LambdaDecl {
        source: crate::ast::ExprId(0),
        module,
        owner: FunctionId::from_index(2),
        body,
        signature: SignatureType::new(vec![], ReturnMode::Value(void_ty)),
        escape: LambdaEscape::NonEscaping,
        captures: vec![LambdaCaptureDecl::ScopedBorrow {
            binding,
            borrow: scoped,
            ty: int_ty,
            mutability: Mutability::Mutable,
        }],
    });
    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), void_ty);
    let bb0 = lambda_body.push_block(term_return_void());
    let place = Place {
        root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
        projection: vec![],
        ty: int_ty,
    };
    lambda_body.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(both),
            args: vec![CallArg::MutBorrow(place.clone()), CallArg::MutBorrow(place)],
        }),
    );
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);
    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, void_ty);
    let local = owner.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(local, binding);
    owner.push_block(term_return_void());
    builder.alloc_function(owner.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgAliasConflict {
            first: 0,
            second: 1
        })
    )));
}

#[test]
fn immutable_non_local_mut_borrow_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Immutable,
    ));
    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("take_mut"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: int_ty,
            mode: ParamMode::MutBorrow,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let mut fb = FunctionBuilder::new("imm_root", module, FunctionKind::Normal, void_ty);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![CallArg::MutBorrow(Place {
                root: PlaceRoot::ScopedBorrow(scoped),
                projection: vec![],
                ty: int_ty,
            })],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::ImmutableRoot(PlaceRoot::ScopedBorrow(id))) if id == scoped
    )));
}

#[test]
fn invalid_unused_root_decls_are_rejected() {
    let mut builder = ProgramBuilder::default();
    let invalid_ty = TypeId::from_index(99);
    let invalid_module = ModuleId::from_index(99);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        invalid_ty,
        Mutability::Immutable,
    ));
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner: FunctionId::from_index(0),
        source_local: LocalId::from_index(0),
        ty: invalid_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let global = builder.alloc_global_raw(GlobalDecl {
        name: Ident::new("g"),
        module: invalid_module,
        ty: invalid_ty,
        mutability: Mutability::Immutable,
        init: FunctionId::from_index(99),
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.site,
        VerifySite::ScopedBorrow(id) if id == scoped
    ) && matches!(e.kind, EK::BadReference(BadReference::InvalidType(ty)) if ty == invalid_ty)));
    assert!(errors.iter().any(|e| matches!(
        e.site,
        VerifySite::CaptureCell(id) if id == cell
    ) && matches!(e.kind, EK::BadReference(BadReference::InvalidType(ty)) if ty == invalid_ty)));
    assert!(errors.iter().any(|e| matches!(
        e.site,
        VerifySite::Global(id) if id == global
    ) && matches!(e.kind, EK::BadReference(BadReference::InvalidModule(module)) if module == invalid_module)));
}

#[test]
fn scoped_borrow_source_owner_must_exist() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(99),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidFunction(owner)) if owner == FunctionId::from_index(99)
    ) && matches!(e.site, VerifySite::ScopedBorrow(id) if id == scoped)));
}

#[test]
fn scoped_borrow_source_local_must_exist() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(3),
        int_ty,
        Mutability::Mutable,
    ));
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, BindingId::from_index(0));
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ScopedBorrowSourceLocalMismatch { borrow, owner, local })
            if borrow == scoped && owner == FunctionId::from_index(0) && local == LocalId::from_index(3)
    )));
}

#[test]
fn scoped_borrow_source_local_type_must_match() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        bool_ty,
        Mutability::Mutable,
    ));
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, BindingId::from_index(0));
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ScopedBorrowSourceLocalTypeMismatch { borrow, expected, found })
            if borrow == scoped && expected == bool_ty && found == int_ty
    )));
}

#[test]
fn scoped_borrow_source_local_binding_must_match() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(1),
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, BindingId::from_index(0));
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ScopedBorrowSourceLocalBindingMismatch { borrow, expected, found })
            if borrow == scoped && expected == BindingId::from_index(1) && found == Some(BindingId::from_index(0))
    )));
}

#[test]
fn scoped_borrow_source_local_must_be_mut_param() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::Value, ParamRole::Normal);
    fb.bind_local(local, BindingId::from_index(0));
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ScopedBorrowSourceLocalMustBeMutParam { borrow, local })
            if borrow == scoped && local == LocalId::from_index(0)
    )));
}

#[test]
fn scoped_borrow_source_must_be_mutable() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Immutable,
    ));
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, BindingId::from_index(0));
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ScopedBorrowSourceMustBeMutable { borrow })
            if borrow == scoped
    )));
}

#[test]
fn duplicate_scoped_borrow_source_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let first = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let second = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, binding);
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateScopedBorrow { first: found_first, second: found_second, .. })
            if found_first == first && found_second == second
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateScopedBorrowSource { first: found_first, second: found_second, .. })
            if found_first == first && found_second == second
    )));
}

#[test]
fn scoped_borrow_source_rejects_receiver_param() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let mut fb = FunctionBuilder::new("method", module, FunctionKind::Method, int_ty);
    let local = fb.push_param_with_mode("self", int_ty, ParamMode::MutBorrow, ParamRole::Receiver);
    fb.bind_local(local, BindingId::from_index(0));
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ScopedBorrowSourceLocalMustBeMutParam { borrow, local })
            if borrow == scoped && local == LocalId::from_index(0)
    )));
}

#[test]
fn ref_self_scoped_borrow_rejects_normal_param() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(ScopedBorrowDecl {
        owner: FunctionId::from_index(0),
        binding: BindingId::from_index(0),
        source: ScopedBorrowSource::RefSelf {
            local: LocalId::from_index(0),
        },
        ty: int_ty,
        mutability: Mutability::Mutable,
    });
    let mut fb = FunctionBuilder::new("f", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, BindingId::from_index(0));
    fb.push_block(term_return(op_place(local, int_ty)));
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ScopedBorrowSourceLocalMustBeMutParam { borrow, local })
            if borrow == scoped && local == LocalId::from_index(0)
    )));
}

#[test]
fn scoped_borrow_root_in_unrelated_function_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = owner.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(local, binding);
    owner.push_block(term_return(op_place(local, int_ty)));
    assert_eq!(
        builder.alloc_function(owner.finish()),
        FunctionId::from_index(0)
    );

    let mut other = FunctionBuilder::new("other", module, FunctionKind::Normal, int_ty);
    other.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::ScopedBorrow(scoped),
        projection: vec![],
        ty: int_ty,
    })));
    builder.alloc_function(other.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::ScopedBorrowNotAccessible { borrow, function })
            if borrow == scoped && function == FunctionId::from_index(1)
    )));
}

#[test]
fn lambda_decl_inaccessible_scoped_borrow_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = owner.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(local, binding);
    owner.push_block(term_return(op_place(local, int_ty)));
    assert_eq!(
        builder.alloc_function(owner.finish()),
        FunctionId::from_index(0)
    );

    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(1);
    builder.alloc_lambda(LambdaDecl {
        source: crate::ast::ExprId(0),
        module,
        owner: FunctionId::from_index(2),
        body,
        signature: SignatureType::new(vec![], ReturnMode::Value(int_ty)),
        escape: LambdaEscape::NonEscaping,
        captures: vec![LambdaCaptureDecl::ScopedBorrow {
            binding,
            borrow: scoped,
            ty: int_ty,
            mutability: Mutability::Mutable,
        }],
    });
    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    lambda_body.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);
    let mut other = FunctionBuilder::new("other", module, FunctionKind::Normal, int_ty);
    other.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(
        builder.alloc_function(other.finish()),
        FunctionId::from_index(2)
    );

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::LambdaScopedBorrowNotAccessible { lambda: found_lambda, owner, borrow })
            if found_lambda == lambda && owner == FunctionId::from_index(2) && borrow == scoped
    )));
}

#[test]
fn invalid_scoped_borrow_root() {
    let errors = invalid_root_errors(PlaceRoot::ScopedBorrow(ScopedBorrowId::from_index(0)));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidScopedBorrow(id)) if id == ScopedBorrowId::from_index(0)
    )));
}

#[test]
fn invalid_capture_cell_root() {
    let errors = invalid_root_errors(PlaceRoot::CaptureCell(CaptureCellId::from_index(0)));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidCaptureCell(id)) if id == CaptureCellId::from_index(0)
    )));
}

#[test]
fn invalid_global_root() {
    let errors = invalid_root_errors(PlaceRoot::Global(GlobalId::from_index(0)));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidGlobal(id)) if id == GlobalId::from_index(0)
    )));
}

#[test]
fn field_proj_non_aggregate() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("field_on_int", module, FunctionKind::Normal, void_ty);
    let local_int = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    let bad_place = Place {
        root: PlaceRoot::Local(local_int),
        projection: vec![Projection::Field(FieldId::from_index(0))],
        ty: int_ty,
    };
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(bb0, stmt_eval(RValue::Use(Operand::Place(bad_place))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::FieldProjectionOnNonAggregate(t)) if t == int_ty
    )));
}

#[test]
fn tuple_field_out_of_range() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let tuple_ty = builder.alloc_type(TypeData::Tuple(vec![int_ty, int_ty]));

    let mut fb = FunctionBuilder::new("tuple_oob", module, FunctionKind::Normal, void_ty);
    let local_t = fb.push_local(None, tuple_ty, Mutability::Immutable, LocalKind::User);
    let bad_place = Place {
        root: PlaceRoot::Local(local_t),
        projection: vec![Projection::TupleField(99)],
        ty: int_ty,
    };
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(bb0, stmt_eval(RValue::Use(Operand::Place(bad_place))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::TupleFieldOutOfRange { ty, index: 99, len: 2 }) if ty == tuple_ty
    )));
}

#[test]
fn index_proj_non_indexable() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("index_bad", module, FunctionKind::Normal, void_ty);
    let local_int = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    let idx_local = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    let bad_place = Place {
        root: PlaceRoot::Local(local_int),
        projection: vec![Projection::Index(idx_local)],
        ty: int_ty,
    };
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(bb0, stmt_eval(RValue::Use(Operand::Place(bad_place))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::IndexProjectionOnNonIndexable(t)) if t == int_ty
    )));
}

#[test]
fn lambda_bad_id() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);

    let sig_type = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![ParamType {
            ty: int_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        ReturnMode::Value(int_ty),
    )));

    let mut fb = FunctionBuilder::new("bad_lambda", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda: LambdaId::from_index(999),
            captures: vec![],
            ty: sig_type,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidLambda(LambdaId(id))) if id == 999
    )));
}

#[test]
fn entry_must_not_be_lambda_body() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (_, body, _) = simple_lambda_program(&mut builder, module, int_ty, vec![]);
    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    owner.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    builder.alloc_function(owner.finish());
    builder.set_entry(body);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::EntryMustBeNamed(id)) if id == body
    )));
}

#[test]
fn lambda_decl_body_kind_and_capture_are_checked() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda = LambdaId::from_index(0);
    let body_id = FunctionId::from_index(0);
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body: body_id,
            owner: FunctionId::from_index(0),
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![LambdaCaptureDecl::CaptureCell {
                binding: BindingId::from_index(0),
                cell: CaptureCellId::from_index(99),
                ty: int_ty,
            }],
        }),
        lambda
    );
    let mut fb = FunctionBuilder::new("bad_lambda_body", module, FunctionKind::Normal, int_ty);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), body_id);
    builder.set_entry(body_id);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::LambdaBodyKindMismatch { lambda: id, body })
            if id == lambda && body == body_id
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidCaptureCell(id)) if id == CaptureCellId::from_index(99)
    )));
}

fn simple_lambda_program(
    builder: &mut ProgramBuilder,
    module: ModuleId,
    int_ty: TypeId,
    captures: Vec<LambdaCaptureDecl>,
) -> (LambdaId, FunctionId, TypeId) {
    let lambda = LambdaId::from_index(builder.lambda_count());
    let body = FunctionId::from_index(builder.function_count());
    let owner = FunctionId::from_index(body.index() + 1);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner,
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures,
        }),
        lambda
    );
    let mut fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), body);
    (lambda, body, lambda_ty)
}

#[test]
fn lambda_body_module_must_match_decl() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module_a = test_module(&mut builder);
    let module_b = builder.alloc_module(empty_module("other"));
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module: module_a,
            owner: FunctionId::from_index(0),
            body,
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![],
        }),
        lambda
    );
    let mut fb = FunctionBuilder::new("lambda", module_b, FunctionKind::Lambda(lambda), int_ty);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), body);
    builder.set_entry(body);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::LambdaBodyModuleMismatch {
            lambda: id,
            expected,
            found,
        }) if id == lambda && expected == module_a && found == module_b
    )));
}

#[test]
fn function_value_ty_must_be_function() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let mut target = FunctionBuilder::new("target", module, FunctionKind::Normal, int_ty);
    target.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let target = builder.alloc_function(target.finish());

    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::FunctionRef {
            function: target,
            ty: int_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::FunctionValueMustBeFunction(ty)) if ty == int_ty
    )));
}

#[test]
fn make_lambda_ty_must_be_function() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (lambda, _, _) = simple_lambda_program(&mut builder, module, int_ty, vec![]);
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![],
            ty: int_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::FunctionValueMustBeFunction(ty)) if ty == int_ty
    )));
}

#[test]
fn direct_function_call_rejects_lambda_body() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (_, body, _) = simple_lambda_program(&mut builder, module, int_ty, vec![]);
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(body),
            args: vec![],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::FunctionCalleeMustBeNamed(id)) if id == body
    )));
}

#[test]
fn function_ref_rejects_lambda_body() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (_, body, lambda_ty) = simple_lambda_program(&mut builder, module, int_ty, vec![]);
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::FunctionRef {
            function: body,
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::FunctionRefMustBeNamed(id)) if id == body
    )));
}

#[test]
fn make_lambda_captures_must_match_decl() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::NoRuntime {
            binding: BindingId::from_index(0),
            ty: int_ty,
        }],
    );
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::LambdaCaptureMismatch { index: 0 })
    )));
}

#[test]
fn make_lambda_must_be_created_by_decl_owner() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let wrong_owner = FunctionId::from_index(2);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            owner,
            body,
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![],
        }),
        lambda
    );
    let mut body_fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(body_fb.finish()), body);
    let mut owner_fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    owner_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(owner_fb.finish()), owner);
    let mut wrong_fb = FunctionBuilder::new("wrong", module, FunctionKind::Normal, int_ty);
    let bb0 = wrong_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    wrong_fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![],
            ty: lambda_ty,
        }),
    );
    assert_eq!(builder.alloc_function(wrong_fb.finish()), wrong_owner);
    builder.set_entry(wrong_owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::MakeLambdaOwnerMismatch { lambda: lambda_found, expected, found: creator })
            if lambda_found == lambda && expected == owner && creator == wrong_owner
    )));
}

#[test]
fn no_runtime_capture_slot_cannot_be_used_as_place_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            owner,
            body,
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![LambdaCaptureDecl::NoRuntime {
                binding: BindingId::from_index(0),
                ty: int_ty,
            }],
        }),
        lambda
    );
    let mut body_fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body_fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(body_fb.finish()), body);
    let mut owner_fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    owner_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(owner_fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::NoRuntimeLambdaCaptureRoot(slot))
            if slot == LambdaCaptureSlotId::from_index(0)
    )));
}

#[test]
fn make_lambda_capture_value_type_must_match_decl() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ReadonlyLocal {
            binding,
            ty: int_ty,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let bad = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: op_const(bad),
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::LambdaCaptureMismatch { index: 0 })
    )));
}

#[test]
fn escaping_lambda_cannot_have_scoped_capture_decl() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner: FunctionId::from_index(0),
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![LambdaCaptureDecl::ScopedLocal {
                binding: BindingId::from_index(0),
                ty: int_ty,
                mutability: Mutability::Immutable,
                source: CaptureLocalSource {
                    owner: FunctionId::from_index(0),
                    local: LocalId::from_index(0),
                },
            }],
        }),
        lambda
    );
    let mut fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), body);
    builder.set_entry(FunctionId::from_index(1));
    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(
        builder.alloc_function(main.finish()),
        FunctionId::from_index(1)
    );

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::EscapingLambdaScopedCapture { lambda: id }) if id == lambda
    )));
}

#[test]
fn escaping_lambda_body_cannot_use_scoped_borrow_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let owner = FunctionId::from_index(1);
    let binding = BindingId::from_index(0);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        owner,
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Immutable,
    ));
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner,
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![LambdaCaptureDecl::ScopedBorrow {
                binding,
                borrow: scoped,
                ty: int_ty,
                mutability: Mutability::Mutable,
            }],
        }),
        lambda
    );
    let mut fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::ScopedBorrow(scoped),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(fb.finish()), body);
    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    main.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let main = builder.alloc_function(main.finish());
    assert_eq!(main, owner);
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::EscapingLambdaScopedBorrowRoot { lambda: id, root })
            if id == lambda && root == scoped
    )));
}

#[test]
fn escaping_readonly_capture_must_not_use_scoped_borrow_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Immutable,
    ));
    let binding = BindingId::from_index(0);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner: FunctionId::from_index(1),
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![LambdaCaptureDecl::ReadonlyLocal {
                binding,
                ty: int_ty,
                source: CaptureLocalSource {
                    owner: FunctionId::from_index(1),
                    local: LocalId::from_index(0),
                },
            }],
        }),
        lambda
    );
    let mut body_fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(body_fb.finish()), body);

    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: Operand::Place(Place {
                    root: PlaceRoot::ScopedBorrow(scoped),
                    projection: vec![],
                    ty: int_ty,
                }),
            }],
            ty: lambda_ty,
        }),
    );
    let main = builder.alloc_function(fb.finish());
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::ReadonlyCaptureMustBeImmutableOwned { index: 0 })
    )));
}

#[test]
fn escaping_readonly_capture_must_not_use_borrow_param() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner: FunctionId::from_index(1),
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![LambdaCaptureDecl::ReadonlyLocal {
                binding,
                ty: int_ty,
                source: CaptureLocalSource {
                    owner: FunctionId::from_index(1),
                    local: LocalId::from_index(0),
                },
            }],
        }),
        lambda
    );
    let mut body_fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(body_fb.finish()), body);

    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let borrowed = fb.push_param_with_mode("x", int_ty, ParamMode::SharedBorrow, ParamRole::Normal);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: op_place(borrowed, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    let main = builder.alloc_function(fb.finish());
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::ReadonlyCaptureMustBeImmutableOwned { index: 0 })
    )));
}

#[test]
fn nonescaping_lambda_body_scoped_borrow_requires_capture_decl() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let owner = FunctionId::from_index(1);
    let binding = BindingId::from_index(0);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        owner,
        binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Immutable,
    ));
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner,
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![LambdaCaptureDecl::ScopedBorrow {
                binding,
                borrow: scoped,
                ty: int_ty,
                mutability: Mutability::Mutable,
            }],
        }),
        lambda
    );
    let mut body_fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body_fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::ScopedBorrow(scoped),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(body_fb.finish()), body);
    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    main.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let main = builder.alloc_function(main.finish());
    assert_eq!(main, owner);
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::RawScopedBorrowCaptureBypass { lambda: id, root })
            if id == lambda && root == scoped
    )));
}

#[test]
fn lambda_capture_declarations_must_have_unique_bindings() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner: FunctionId::from_index(0),
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![
                LambdaCaptureDecl::NoRuntime {
                    binding,
                    ty: int_ty,
                },
                LambdaCaptureDecl::ReadonlyLocal {
                    binding,
                    ty: int_ty,
                    source: CaptureLocalSource {
                        owner: FunctionId::from_index(0),
                        local: LocalId::from_index(0),
                    },
                },
            ],
        }),
        lambda
    );
    let mut body_fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(body_fb.finish()), body);
    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let main = builder.alloc_function(main.finish());
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateLambdaCapture {
            lambda: id,
            binding: found_binding,
            first: 0,
            second: 1,
        }) if id == lambda && found_binding == binding
    )));
}

#[test]
fn lambda_capture_declarations_must_have_unique_sources() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let borrow = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Immutable,
    ));
    let (lambda, _, _) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![
            LambdaCaptureDecl::ScopedBorrow {
                binding: BindingId::from_index(0),
                borrow,
                ty: int_ty,
                mutability: Mutability::Immutable,
            },
            LambdaCaptureDecl::ScopedBorrow {
                binding: BindingId::from_index(1),
                borrow,
                ty: int_ty,
                mutability: Mutability::Immutable,
            },
        ],
    );
    let owner = FunctionId::from_index(1);
    let mut owner_fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    owner_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(owner_fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateLambdaCaptureSource {
            lambda: found,
            first: 0,
            second: 1,
        }) if found == lambda
    )));
}

#[test]
fn make_lambda_scoped_capture_must_match_binding_identity() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let expected = BindingId::from_index(0);
    let other = BindingId::from_index(1);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ScopedLocal {
            binding: expected,
            ty: int_ty,
            mutability: Mutability::Immutable,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param("x", int_ty, ParamRole::Normal);
    fb.bind_local(local, other);
    let bb0 = fb.push_block(term_return(op_place(local, int_ty)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ScopedLocal {
                place: place(local, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
            lambda: found,
            index: 0,
        }) if found == lambda
    )));
}

#[test]
fn make_lambda_scoped_capture_must_match_scoped_borrow_owner() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(0),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Immutable,
    ));
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ScopedLocal {
            binding,
            ty: int_ty,
            mutability: Mutability::Immutable,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ScopedLocal {
                place: Place {
                    root: PlaceRoot::ScopedBorrow(scoped),
                    projection: vec![],
                    ty: int_ty,
                },
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::LambdaCaptureMismatch { index: 0 })
    )));
}

#[test]
fn readonly_capture_must_match_binding_identity() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let expected = BindingId::from_index(0);
    let other = BindingId::from_index(1);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ReadonlyLocal {
            binding: expected,
            ty: int_ty,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param("x", int_ty, ParamRole::Normal);
    fb.bind_local(local, other);
    let bb0 = fb.push_block(term_return(op_place(local, int_ty)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: op_place(local, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
            lambda: found,
            index: 0,
        }) if found == lambda
    )));
}

#[test]
fn readonly_capture_must_not_use_const_operand() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ReadonlyLocal {
            binding,
            ty: int_ty,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let value = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(value)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: op_const(value),
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::LambdaCaptureMismatch { index: 0 })
    )));
}

#[test]
fn readonly_capture_must_not_use_temp_local() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let source = LocalId::from_index(0);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ReadonlyLocal {
            binding,
            ty: int_ty,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: source,
            },
        }],
    );
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut owner_fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let temp = owner_fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    assert_eq!(temp, source);
    owner_fb.bind_local(temp, binding);
    let bb0 = owner_fb.push_block(term_return(op_const(zero)));
    owner_fb.add_statement(bb0, stmt_init(temp, RValue::Use(op_const(zero))));
    owner_fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: op_place(temp, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    let owner = builder.alloc_function(owner_fb.finish());
    assert_eq!(owner, FunctionId::from_index(1));
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ReadonlyLambdaCaptureSourceMustBeImmutableOwned {
            lambda: found,
            index: 0,
            local,
        }) if found == lambda && local == temp
    )));
}

#[test]
fn readonly_capture_must_not_snapshot_mutable_local() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ReadonlyLocal {
            binding,
            ty: int_ty,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let local = fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User);
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(bb0, stmt_init(local, RValue::Use(op_const(zero))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: op_place(local, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::ReadonlyCaptureMustBeImmutableOwned { index: 0 })
    )));
}

#[test]
fn make_lambda_scoped_capture_mutability_must_match_decl() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ScopedLocal {
            binding,
            ty: int_ty,
            mutability: Mutability::Mutable,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param("x", int_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return(op_place(local, int_ty)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ScopedLocal {
                place: place(local, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::LambdaCaptureSourceMismatch {
            lambda: found,
            index: 0,
        }) if found == lambda
    )));
}

#[test]
fn lambda_callee_must_have_function_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let value = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(value)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Lambda(op_const(value)),
            args: vec![],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, EK::BadCall(BadCall::LambdaCalleeMustBeFunction)))
    );
}

#[test]
fn lambda_call_args_must_match_signature() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let lambda_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![ParamType {
            ty: int_ty,
            mode: ParamMode::MutBorrow,
            escape: ParamEscape::NonEscaping,
        }],
        ReturnMode::Value(int_ty),
    )));
    let arg = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let callee = fb.push_param("f", lambda_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return(op_const(arg)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Lambda(op_place(callee, lambda_ty)),
            args: vec![CallArg::Value(op_const(arg))],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgModeMismatch {
            index: 0,
            expected: ParamMode::MutBorrow,
            found: ParamMode::Value,
        })
    )));
}

#[test]
fn escaping_function_param_rejects_nonescaping_lambda_value() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    let lambda = LambdaId::from_index(0);
    let lambda_body = FunctionId::from_index(0);
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body: lambda_body,
            owner: FunctionId::from_index(0),
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![],
        }),
        lambda
    );
    let mut body = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(body.finish()), lambda_body);

    let callee = FunctionId::from_index(1);
    let mut accept = FunctionBuilder::new("accept", module, FunctionKind::Normal, int_ty);
    let param = accept.push_param("f", lambda_ty, ParamRole::Normal);
    accept.set_param_escape(0, ParamEscape::Escaping);
    accept.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(param, LocalId::from_index(0));
    assert_eq!(builder.alloc_function(accept.finish()), callee);

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let local = main.push_local(Some("f"), lambda_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    main.add_statement(
        bb0,
        stmt_init(
            local,
            RValue::MakeLambda {
                lambda,
                captures: vec![],
                ty: lambda_ty,
            },
        ),
    );
    main.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee),
            args: vec![CallArg::Value(op_place(local, lambda_ty))],
        }),
    );
    let main = builder.alloc_function(main.finish());
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgEscapeMismatch {
            index: 0,
            expected: ParamEscape::Escaping,
            found: ParamEscape::NonEscaping,
        })
    )));
}

#[test]
fn function_value_proof_rejects_nonescaping_local_as_escaping() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig));

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let param = main.push_param("f", lambda_ty, ParamRole::Normal);
    let temp = main.push_local(None, lambda_ty, Mutability::Immutable, LocalKind::Temp);
    let bb0 = main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    main.add_statement(
        bb0,
        stmt_init(
            temp,
            RValue::FunctionValue {
                value: op_place(param, lambda_ty),
                capability: FunctionValueCapability::Escaping,
            },
        ),
    );
    let main = builder.alloc_function(main.finish());
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::FunctionValueEscapeMismatch {
            claimed: FunctionValueCapability::Escaping,
            actual: FunctionValueCapability::NonEscaping,
        })
    )));
}

#[test]
fn escaping_function_param_rejects_unknown_function_value() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig));
    let tuple_ty = builder.alloc_type(TypeData::Tuple(vec![lambda_ty]));

    let callee = FunctionId::from_index(0);
    let mut accept = FunctionBuilder::new("accept", module, FunctionKind::Normal, int_ty);
    accept.push_param("f", lambda_ty, ParamRole::Normal);
    accept.set_param_escape(0, ParamEscape::Escaping);
    accept.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(accept.finish()), callee);

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let pair = main.push_param("pair", tuple_ty, ParamRole::Normal);
    let bb0 = main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    main.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee),
            args: vec![CallArg::Value(Operand::Place(Place {
                root: PlaceRoot::Local(pair),
                projection: vec![Projection::TupleField(0)],
                ty: lambda_ty,
            }))],
        }),
    );
    let main = builder.alloc_function(main.finish());
    builder.set_entry(main);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgEscapeUnknown {
            index: 0,
            expected: ParamEscape::Escaping,
        })
    )));
}

#[test]
fn loop_capture_cell_use_outside_declared_loop_rejected() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);
    let int = builder.int_ty();
    let void = builder.void_ty();
    let value = builder.alloc_const(ConstData {
        ty: int,
        value: ConstValue::Int(1),
    });
    let binding = BindingId::from_index(0);
    let mut function = FunctionBuilder::new("f", module, FunctionKind::Normal, void);
    let source = function.push_local(Some("x"), int, Mutability::Mutable, LocalKind::User);
    function.bind_local(source, binding);
    let block = function.push_block(AirTail::Return(None));
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner: FunctionId::from_index(0),
        source_local: source,
        ty: int,
        lifetime: CaptureCellLifetime::Loop {
            loop_id: AirLoopId::from_index(0),
        },
    });
    function.add_statement(
        block,
        stmt_assign(
            Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int,
            },
            RValue::Use(Operand::Const(value)),
        ),
    );
    function.add_statement(
        block,
        AirStmt::Loop(AirLoop {
            id: AirLoopId::from_index(0),
            body: AirBlock::default(),
        }),
    );
    builder.alloc_function(function.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadFunction(BadFunction::CaptureCellOutsideLoop { cell: found, .. }) if found == cell
    )));
}

#[test]
fn duplicate_capture_cell_for_binding_owner_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let binding = BindingId::from_index(0);
    let owner = FunctionId::from_index(0);
    let first = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local: LocalId::from_index(0),
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let second = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local: LocalId::from_index(0),
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateCaptureCell {
            owner: found_owner,
            binding: found_binding,
            first: found_first,
            second: found_second,
        }) if found_owner == owner
            && found_binding == binding
            && found_first == first
            && found_second == second
    )));
}

#[test]
fn duplicate_capture_cell_for_source_local_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let first = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let second = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(1),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateCaptureCellSourceLocal {
            owner: found_owner,
            local: found_local,
            first: found_first,
            second: found_second,
        }) if found_owner == owner
            && found_local == local
            && found_first == first
            && found_second == second
    )));
}

#[test]
fn promoted_binding_must_not_use_source_local_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let binding = BindingId::from_index(0);
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    fb.push_block(term_return(op_place(local, int_ty)));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::PromotedBindingBypassesCell {
            binding: found_binding,
            cell: found_cell,
            local: found_local,
        }) if found_binding == binding && found_cell == cell && found_local == local
    )));
}

#[test]
fn promoted_binding_must_not_be_initialized_as_local() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let binding = BindingId::from_index(0);
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    let bb0 = fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    fb.add_statement(
        bb0,
        stmt_init(
            local,
            RValue::Use(op_const(builder.alloc_const(ConstData {
                ty: int_ty,
                value: ConstValue::Int(0),
            }))),
        ),
    );
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::PromotedBindingBypassesCell {
            binding: found_binding,
            cell: found_cell,
            local: found_local,
        }) if found_binding == binding && found_cell == cell && found_local == local
    )));
}

#[test]
fn capture_cell_read_requires_initialization() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::CaptureCell(cell),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedCaptureCell(found)) if found == cell
    )));
}

#[test]
fn lambda_body_must_not_use_raw_capture_cell_root_for_capture() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            owner,
            body,
            signature: SignatureType::new(vec![], ReturnMode::Value(int_ty)),
            escape: LambdaEscape::Escaping,
            captures: vec![LambdaCaptureDecl::CaptureCell {
                binding,
                cell,
                ty: int_ty,
            }],
        }),
        lambda
    );
    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    lambda_body.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::CaptureCell(cell),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);
    let mut owner_fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        owner_fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    owner_fb.bind_local(local, binding);
    owner_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(owner_fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::RawCaptureCellCaptureBypass { lambda: found, root })
            if found == lambda && root == cell
    )));
}

#[test]
fn capture_cell_cannot_be_used_from_unrelated_function() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let owner = FunctionId::from_index(0);
    let other = FunctionId::from_index(1);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut owner_fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        owner_fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    owner_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(owner_fb.finish()), owner);

    let mut other_fb = FunctionBuilder::new("other", module, FunctionKind::Normal, int_ty);
    other_fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::CaptureCell(cell),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(other_fb.finish()), other);
    builder.set_entry(other);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::CaptureCellNotAccessible {
            cell: found_cell,
            function,
        }) if found_cell == cell && function == other
    )));
}

#[test]
fn promoted_binding_must_not_use_source_local_as_index() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let array_ty = builder.alloc_type(TypeData::Array {
        elem: int_ty,
        len: 2,
    });
    let binding = BindingId::from_index(0);
    let owner = FunctionId::from_index(0);
    let index_local = LocalId::from_index(0);
    let array_local = LocalId::from_index(1);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local: index_local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(fb.push_param("i", int_ty, ParamRole::Normal), index_local);
    assert_eq!(
        fb.push_param("xs", array_ty, ParamRole::Normal),
        array_local
    );
    fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::Local(array_local),
        projection: vec![Projection::Index(index_local)],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::PromotedBindingBypassesCell {
            binding: found_binding,
            cell: found_cell,
            local: found_local,
        }) if found_binding == binding && found_cell == cell && found_local == index_local
    )));
}

#[test]
fn capture_cell_source_local_must_match_payload_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let string_ty = builder.string_ty();
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), string_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CaptureCellSourceLocalTypeMismatch {
            cell: found_cell,
            expected,
            found,
        }) if found_cell == cell && expected == int_ty && found == string_ty
    )));
}

#[test]
fn capture_cell_source_local_must_match_binding() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let expected = BindingId::from_index(0);
    let other = BindingId::from_index(1);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: expected,
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        local
    );
    fb.bind_local(local, other);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CaptureCellSourceLocalBindingMismatch {
            cell: found_cell,
            expected: found_expected,
            found,
        }) if found_cell == cell && found_expected == expected && found == Some(other)
    )));
}

#[test]
fn capture_cell_source_local_must_be_owned_binding() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal),
        local
    );
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CaptureCellSourceLocalMustBeOwnedBinding {
            cell: found_cell,
            local: found_local,
            kind: LocalKind::Arg,
        }) if found_cell == cell && found_local == local
    )));
}

#[test]
fn capture_cell_source_local_must_not_be_pattern_alias() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(
            Some("x"),
            int_ty,
            Mutability::Mutable,
            LocalKind::PatternBinding
        ),
        local
    );
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CaptureCellSourceLocalMustBeOwnedBinding {
            cell: found_cell,
            local: found_local,
            kind: LocalKind::PatternBinding,
        }) if found_cell == cell && found_local == local
    )));
}

#[test]
fn capture_cell_source_local_must_be_mutable() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let owner = FunctionId::from_index(0);
    let local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner,
        source_local: local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), int_ty, Mutability::Immutable, LocalKind::User),
        local
    );
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CaptureCellSourceLocalMustBeMutable {
            cell: found_cell,
            local: found_local,
        }) if found_cell == cell && found_local == local
    )));
}

#[test]
fn mutable_scoped_lambda_capture_requires_mutable_place() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let (lambda, _, lambda_ty) = simple_lambda_program(
        &mut builder,
        module,
        int_ty,
        vec![LambdaCaptureDecl::ScopedLocal {
            binding,
            ty: int_ty,
            mutability: Mutability::Mutable,
            source: CaptureLocalSource {
                owner: FunctionId::from_index(1),
                local: LocalId::from_index(0),
            },
        }],
    );
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param("x", int_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return(op_place(local, int_ty)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ScopedLocal {
                place: place(local, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::ImmutableRoot(PlaceRoot::Local(id))) if id == local
    )));
}

#[test]
fn call_overlapping_shared_and_mut_borrow_args() {
    let mut builder = ProgramBuilder::default();
    let string_ty = builder.string_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let mut callee = FunctionBuilder::new("callee", module, FunctionKind::Normal, void_ty);
    callee.push_param_with_mode(
        "read",
        string_ty,
        ParamMode::SharedBorrow,
        ParamRole::Normal,
    );
    callee.push_param_with_mode("write", string_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    let callee = builder.alloc_function(callee.finish());

    let mut fb = FunctionBuilder::new("alias", module, FunctionKind::Normal, void_ty);
    let local = fb.push_param_with_mode("text", string_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee),
            args: vec![
                CallArg::SharedBorrow(place(local, string_ty)),
                CallArg::MutBorrow(place(local, string_ty)),
            ],
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgAliasConflict {
            first: 0,
            second: 1
        })
    )));
}

#[test]
fn init_local_out_of_range() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);
    let c = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("bad_init", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return(op_const(c)));
    fb.add_statement(
        bb0,
        stmt_init(LocalId::from_index(99), RValue::Use(op_const(c))),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidLocal(LocalId(99)))
    )));
}

#[test]
fn init_param_local_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_init_param", module, FunctionKind::Normal, int_ty);
    let p = fb.push_param("p", int_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return(op_place(p, int_ty)));
    fb.add_statement(bb0, stmt_init(p, RValue::Use(op_place(p, int_ty))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(
        |e| matches!(e.kind, EK::BadStatement(BadStatement::InitParamLocal(local)) if local == p)
    ));
}

#[test]
fn init_value_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_init_type", module, FunctionKind::Normal, int_ty);
    let local = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    let b = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = fb.push_block(term_return(op_place(local, int_ty)));
    fb.add_statement(bb0, stmt_init(local, RValue::Use(op_place(b, bool_ty))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadStatement(BadStatement::InitTypeMismatch { expected, found }) if expected == int_ty && found == bool_ty)));
}

#[test]
fn assign_immutable_local_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_assign_imm", module, FunctionKind::Normal, int_ty);
    let imm = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = fb.push_block(term_return(op_place(imm, int_ty)));
    fb.add_statement(
        bb0,
        stmt_assign(place(imm, int_ty), RValue::Use(op_place(imm, int_ty))),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::ImmutableRoot(PlaceRoot::Local(local))) if local == imm
    )));
}

#[test]
fn assign_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_assign_type", module, FunctionKind::Normal, int_ty);
    let int = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::User);
    let bool_ = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = fb.push_block(term_return(op_place(int, int_ty)));
    fb.add_statement(
        bb0,
        stmt_assign(place(int, int_ty), RValue::Use(op_place(bool_, bool_ty))),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadStatement(BadStatement::AssignTypeMismatch { expected, found }) if expected == int_ty && found == bool_ty)));
}

#[test]
fn duplicate_primitive_type_is_invalid() {
    let mut builder = ProgramBuilder::default();
    let first = builder.alloc_type(TypeData::Int);
    let duplicate = builder.alloc_type(TypeData::Int);
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadType(BadType::DuplicatePrimitive { kind: PrimitiveKind::Int, first: a, duplicate: b }) if a == first && b == duplicate)));
}

#[test]
fn missing_nominal_type_identity_is_invalid() {
    let mut program = Program::default();
    let module = program.alloc_module(Module::default());
    let aggregate = program.alloc_aggregate(AggregateDecl {
        name: Ident::new("Missing"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);

    let errors = verify(&program).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| { error.kind == EK::BadType(BadType::MissingAggregate(aggregate)) })
    );
}

#[test]
fn duplicate_nominal_type_identity_is_invalid() {
    let mut program = Program::default();
    let module = program.alloc_module(Module::default());
    let aggregate = program.alloc_aggregate(AggregateDecl {
        name: Ident::new("Duplicate"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    let first = program.alloc_type(TypeData::Aggregate(aggregate));
    let duplicate = program.alloc_type(TypeData::Aggregate(aggregate));

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadType(BadType::DuplicateAggregate {
            aggregate: found,
            first: a,
            duplicate: b,
        }) if found == aggregate && a == first && b == duplicate
    )));
}

#[test]
fn struct_represented_as_dataref_is_invalid() {
    let mut program = Program::default();
    let module = program.alloc_module(Module::default());
    let aggregate = program.alloc_aggregate(AggregateDecl {
        name: Ident::new("WrongStruct"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    program.alloc_type(TypeData::DataRef(aggregate));

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadType(BadType::AggregateKindMismatch {
            aggregate: found,
            declared: AggregateKind::Struct,
            represented: AggregateKind::DataRef,
        }) if found == aggregate
    )));
}

#[test]
fn dataref_represented_as_struct_is_invalid() {
    let mut program = Program::default();
    let module = program.alloc_module(Module::default());
    let aggregate = program.alloc_aggregate(AggregateDecl {
        name: Ident::new("WrongDataRef"),
        module,
        kind: AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: true,
        stringify_override: None,
    });
    program.module_mut(module).aggregates.push(aggregate);
    program.alloc_type(TypeData::Aggregate(aggregate));

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadType(BadType::AggregateKindMismatch {
            aggregate: found,
            declared: AggregateKind::DataRef,
            represented: AggregateKind::Struct,
        }) if found == aggregate
    )));
}

#[test]
fn recursive_type_is_invalid() {
    let mut program = Program::default();
    let ty = program.alloc_type(TypeData::Optional(TypeId::from_index(999)));
    *program.type_arena.data_mut(ty) = TypeData::Optional(ty);

    let errors = verify(&program).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, EK::BadType(BadType::Recursive(id)) if id == ty))
    );
}

#[test]
fn dynamic_type_must_reference_a_surface() {
    let mut builder = ProgramBuilder::default();
    let missing = ContractSurfaceId::from_index(7);
    builder.alloc_type(TypeData::Dyn(missing));

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadReference(BadReference::InvalidContractSurface(id)) if id == missing
    )));
}

#[test]
fn contract_surface_must_not_be_empty() {
    let mut builder = ProgramBuilder::default();
    builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Empty".into(),
        slots: vec![],
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, EK::BadContract(BadContract::EmptySurface)))
    );
}

#[test]
fn contract_slot_types_must_exist() {
    let mut builder = ProgramBuilder::default();
    let missing = TypeId::from_index(7);
    builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Bad".into(),
        slots: vec![ContractSlotDecl {
            id: ContractSlotId::from_index(0),
            name: Ident::new("call"),
            receiver: ContractReceiver::Value,
            params: vec![ContractParamDecl {
                ty: missing,
                mode: ParamMode::SharedBorrow,
                cast_accept: false,
                escape: ParamEscape::NonEscaping,
            }],
            ret: ContractReturnDecl::Iter,
        }],
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadReference(BadReference::InvalidType(id)) if id == missing
    )));
}

#[test]
fn malformed_dynamic_rvalues_reject() {
    let mut builder = ProgramBuilder::default();
    let int = builder.int_ty();
    let void = builder.void_ty();
    let module = test_module(&mut builder);
    let surface = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Action".into(),
        slots: vec![contract_slot("act", 0, void)],
    });
    let dyn_ty = builder.alloc_type(TypeData::Dyn(surface));
    let mut fb = FunctionBuilder::new("bad_dyn", module, FunctionKind::Normal, void);
    let concrete = fb.push_local(None, int, Mutability::Immutable, LocalKind::User);
    let dynamic = fb.push_local(None, dyn_ty, Mutability::Mutable, LocalKind::User);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::DynPack {
            value: op_place(concrete, int),
            use_: DynOwnedUse::ReusableRead,
            witness: ContractWitnessId::from_index(99),
            ty: dyn_ty,
        }),
    );
    fb.add_statement(
        bb0,
        stmt_eval(RValue::DynWeaken {
            value: op_place(dynamic, dyn_ty),
            use_: DynOwnedUse::ReusableRead,
            weakening: ContractWeakeningId::from_index(99),
            ty: dyn_ty,
        }),
    );
    fb.add_statement(
        bb0,
        stmt_eval(RValue::DynCall {
            receiver: crate::air::DynReceiver::Owned(op_place(dynamic, dyn_ty)),
            surface,
            slot: ContractSlotId::from_index(99),
            args: vec![],
        }),
    );
    let function = builder.alloc_function(fb.finish());
    builder.set_entry(function);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, EK::BadRValue(BadRValue::InvalidDynPack)))
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, EK::BadRValue(BadRValue::InvalidDynWeaken)))
    );
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, EK::BadRValue(BadRValue::InvalidDynCall)))
    );
}

#[test]
fn dynamic_borrow_args_cannot_alias() {
    let mut builder = ProgramBuilder::default();
    let void = builder.void_ty();
    let surface = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Action".into(),
        slots: vec![contract_slot("act", 0, void)],
    });
    let dyn_ty = builder.alloc_type(TypeData::Dyn(surface));
    let module = test_module(&mut builder);
    let mut callee = FunctionBuilder::new("callee", module, FunctionKind::Normal, void);
    callee.push_param_with_mode("left", dyn_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_param_with_mode("right", dyn_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    let callee = builder.alloc_function(callee.finish());

    let mut caller = FunctionBuilder::new("caller", module, FunctionKind::Normal, void);
    let source = caller.push_local(None, dyn_ty, Mutability::Mutable, LocalKind::User);
    let place = place(source, dyn_ty);
    let borrow = crate::air::DynBorrow {
        source: crate::air::DynBorrowSource::Owned(place),
        ty: dyn_ty,
        surface,
        weakening: None,
    };
    let bb0 = caller.push_block(term_return_void());
    caller.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee),
            args: vec![
                CallArg::DynBorrow(borrow.clone()),
                CallArg::DynBorrow(borrow),
            ],
        }),
    );
    let caller = builder.alloc_function(caller.finish());
    builder.set_entry(caller);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadCall(BadCall::ArgAliasConflict {
            first: 0,
            second: 1
        })
    )));
}

#[test]
fn dynamic_call_checks_escaping_callback_args() {
    let mut builder = ProgramBuilder::default();
    let void = builder.void_ty();
    let callback_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![],
        ReturnMode::Value(void),
    )));
    let surface = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Action".into(),
        slots: vec![ContractSlotDecl {
            id: ContractSlotId::from_index(0),
            name: Ident::new("run"),
            receiver: ContractReceiver::Value,
            params: vec![ContractParamDecl {
                ty: callback_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::Escaping,
                cast_accept: false,
            }],
            ret: ContractReturnDecl::Value(void),
        }],
    });
    let dyn_ty = builder.alloc_type(TypeData::Dyn(surface));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_escape", module, FunctionKind::Normal, void);
    let receiver = fb.push_param_with_mode(
        "receiver",
        dyn_ty,
        ParamMode::SharedBorrow,
        ParamRole::Normal,
    );
    let callback = fb.push_param("callback", callback_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::DynCall {
            receiver: crate::air::DynReceiver::Owned(op_place(receiver, dyn_ty)),
            surface,
            slot: ContractSlotId::from_index(0),
            args: vec![CallArg::Value(op_place(callback, callback_ty))],
        }),
    );
    let function = builder.alloc_function(fb.finish());
    builder.set_entry(function);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadCall(BadCall::ArgEscapeMismatch {
            expected: ParamEscape::Escaping,
            found: ParamEscape::NonEscaping,
            ..
        })
    )));
}

#[test]
fn borrowed_dynamic_param_cannot_escape_as_value() {
    let mut builder = ProgramBuilder::default();
    let void = builder.void_ty();
    let module = test_module(&mut builder);
    let surface = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Action".into(),
        slots: vec![contract_slot("act", 0, void)],
    });
    let dyn_ty = builder.alloc_type(TypeData::Dyn(surface));
    let owner = FunctionId::from_index(0);
    let mut fb = FunctionBuilder::new("bad_borrow", module, FunctionKind::Normal, dyn_ty);
    let source = fb.push_param_with_mode("value", dyn_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let borrow = builder.alloc_dyn_borrow_param(DynBorrowParamDecl {
        owner,
        source,
        ty: dyn_ty,
        surface,
    });
    fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::DynBorrowParam(borrow),
        projection: vec![],
        ty: dyn_ty,
    })));
    let function = builder.alloc_function(fb.finish());
    builder.set_entry(function);

    let mut program = builder.finish();
    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadPlace(BadPlace::DynBorrowParamEscapes(id)) if id == borrow
    )));

    program.function_mut(function).body.block.tail = AirTail::Return(Some(Operand::Place(Place {
        root: PlaceRoot::Local(source),
        projection: vec![],
        ty: dyn_ty,
    })));
    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadPlace(BadPlace::DynBorrowParamEscapes(id)) if id == borrow
    )));

    let function = program.function_mut(function);
    function.body.block.tail = AirTail::Unreachable;
    function
        .body
        .block
        .stmts
        .push(AirStmt::Eval(RValue::Use(Operand::Place(Place {
            root: PlaceRoot::Local(source),
            projection: vec![],
            ty: dyn_ty,
        }))));
    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadPlace(BadPlace::DynBorrowParamEscapes(id)) if id == borrow
    )));
}

#[test]
fn dynamic_borrow_param_is_unique_and_complete() {
    let mut builder = ProgramBuilder::default();
    let void = builder.void_ty();
    let surface = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Action".into(),
        slots: vec![contract_slot("act", 0, void)],
    });
    let dyn_ty = builder.alloc_type(TypeData::Dyn(surface));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("borrow", module, FunctionKind::Normal, void);
    let source = fb.push_param_with_mode("value", dyn_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.push_block(term_return_void());
    let owner = builder.alloc_function(fb.finish());
    builder.set_entry(owner);
    let mut program = builder.finish();

    let error = |program: &Program, found| {
        verify(program).unwrap_err().iter().any(|error| {
            matches!(
                error.kind,
                EK::BadFunction(BadFunction::DynBorrowParamCount { local, found: count })
                    if local == source && count == found
            )
        })
    };
    assert!(error(&program, 0));

    let decl = DynBorrowParamDecl {
        owner,
        source,
        ty: dyn_ty,
        surface,
    };
    program.alloc_dyn_borrow_param(decl.clone());
    program.alloc_dyn_borrow_param(decl);
    assert!(error(&program, 2));
}

#[test]
fn contract_witness_target_must_exist() {
    let mut builder = ProgramBuilder::default();
    let int = builder.int_ty();
    let surface = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Action".into(),
        slots: vec![contract_slot("act", 0, int)],
    });
    let slots = vec![ContractWitnessSlotDecl {
        slot: ContractSlotId::from_index(0),
        receiver: ParamMode::Value,
        target: ContractWitnessTarget::Function {
            function: FunctionId::from_index(7),
        },
    }];
    builder.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: int,
            surface,
            slots,
        },
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadReference(BadReference::InvalidFunction(id)) if id == FunctionId::from_index(7)
    )));
}

#[test]
fn malformed_promoted_witness_does_not_panic() {
    let mut builder = ProgramBuilder::default();
    let int = builder.int_ty();
    let missing = TypeId::from_index(99);
    let surface = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Action".into(),
        slots: vec![contract_slot("act", 0, int)],
    });
    let slots = vec![ContractWitnessSlotDecl {
        slot: ContractSlotId::from_index(0),
        receiver: ParamMode::SharedBorrow,
        target: ContractWitnessTarget::Promoted {
            fields: vec![],
            target: Box::new(ContractWitnessTarget::Function {
                function: FunctionId::from_index(7),
            }),
        },
    }];
    builder.alloc_contract_witness(ContractWitnessDecl {
        key: ContractWitnessKey {
            concrete_ty: missing,
            surface,
            slots,
        },
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadReference(BadReference::InvalidType(id)) if id == missing
    )));
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadContract(BadContract::InvalidWitnessProjection)
    )));
}

#[test]
fn contract_weakening_must_be_proper() {
    let mut builder = ProgramBuilder::default();
    let int = builder.int_ty();
    let source = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Source".into(),
        slots: vec![contract_slot("a", 0, int), contract_slot("b", 1, int)],
    });
    builder.alloc_contract_weakening(ContractWeakeningDecl {
        source,
        target: source,
        target_to_source: vec![ContractSlotId::from_index(0), ContractSlotId::from_index(1)],
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, EK::BadContract(BadContract::WeakeningNotProper)))
    );
}

#[test]
fn contract_weakening_must_be_unique() {
    let mut builder = ProgramBuilder::default();
    let int = builder.int_ty();
    let source = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Source".into(),
        slots: vec![contract_slot("a", 0, int), contract_slot("b", 1, int)],
    });
    let target = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Target".into(),
        slots: vec![contract_slot("a", 0, int)],
    });
    let weakening = ContractWeakeningDecl {
        source,
        target,
        target_to_source: vec![ContractSlotId::from_index(0)],
    };
    builder.alloc_contract_weakening(weakening.clone());
    builder.alloc_contract_weakening(weakening);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|error| matches!(error.kind, EK::BadContract(BadContract::DuplicateWeakening)))
    );
}

#[test]
fn contract_weakening_must_preserve_slot_name() {
    let mut builder = ProgramBuilder::default();
    let int = builder.int_ty();
    let source = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Source".into(),
        slots: vec![contract_slot("a", 0, int), contract_slot("b", 1, int)],
    });
    let target = builder.alloc_contract_surface(ContractSurfaceDecl {
        display_name: "Target".into(),
        slots: vec![contract_slot("c", 0, int)],
    });
    builder.alloc_contract_weakening(ContractWeakeningDecl {
        source,
        target,
        target_to_source: vec![ContractSlotId::from_index(0)],
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|error| matches!(
        error.kind,
        EK::BadContract(BadContract::WeakeningSlotSignatureMismatch)
    )));
}

fn contract_slot(name: &str, index: usize, ret: TypeId) -> ContractSlotDecl {
    ContractSlotDecl {
        id: ContractSlotId::from_index(index),
        name: Ident::new(name),
        receiver: ContractReceiver::Value,
        params: vec![],
        ret: ContractReturnDecl::Value(ret),
    }
}

#[test]
fn constants_must_match_value_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Int(1),
    });
    builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Nil,
    });
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadConst(BadConst::TypeMismatch { expected, found }) if expected == int_ty && found == bool_ty)));
    assert!(
        errors.iter().any(
            |e| matches!(e.kind, EK::BadConst(BadConst::NilMustBeOptional(ty)) if ty == int_ty)
        )
    );
}

#[test]
fn place_claimed_type_and_index_type_are_verified() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_place_ty", module, FunctionKind::Normal, int_ty);
    let list = fb.push_local(None, list_ty, Mutability::Immutable, LocalKind::User);
    let idx = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::User);
    let bad_place = Place {
        root: PlaceRoot::Local(list),
        projection: vec![Projection::Index(idx)],
        ty: list_ty,
    };
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(bb0, stmt_eval(RValue::Use(Operand::Place(bad_place))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadPlace(BadPlace::IndexLocalTypeMismatch { expected, found }) if expected == int_ty && found == bool_ty)));
}

#[test]
fn rvalue_binary_and_cast_invariants() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_rvalues", module, FunctionKind::Normal, int_ty);
    let i = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    let b = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = fb.push_block(term_return(op_place(i, int_ty)));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Binary {
            op: crate::ast::BinaryOp::And,
            lhs: op_place(b, bool_ty),
            rhs: op_place(b, bool_ty),
            ty: bool_ty,
        }),
    );
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Cast {
            value: op_place(i, int_ty),
            target: int_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::UnsupportedBinaryOp(crate::ast::BinaryOp::And))
    )));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadRValue(BadRValue::CastMustConvertIntAndFloat { value, target }) if value == int_ty && target == int_ty)));
}

#[test]
fn raw_enum_cast_must_match_backing() {
    let mut builder = ProgramBuilder::default();
    let string_ty = builder.string_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let (_, enum_ty) = builder.raw_int_enum(module, "State", vec![("Idle", 0)]);

    let errors = verify_void_entry(builder, "bad_cast", module, void_ty, |fb, bb0| {
        let state = fb.push_param("state", enum_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Cast {
                value: op_place(state, enum_ty),
                target: string_ty,
            }),
        );
    });

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CastMustConvertIntAndFloat { value, target })
            if value == enum_ty && target == string_ty
    )));
}

#[test]
fn non_raw_enum_cast_to_primitive_is_invalid() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let (_, enum_ty) = builder.unit_enum(module, "State");

    let errors = verify_void_entry(builder, "bad_cast", module, void_ty, |fb, bb0| {
        let state = fb.push_param("state", enum_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Cast {
                value: op_place(state, enum_ty),
                target: int_ty,
            }),
        );
    });

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CastMustConvertIntAndFloat { value, target })
            if value == enum_ty && target == int_ty
    )));
}

#[test]
fn primitive_cast_to_enum_is_invalid() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let (_, enum_ty) = builder.raw_int_enum(module, "State", vec![("Idle", 0)]);

    let errors = verify_void_entry(builder, "bad_cast", module, void_ty, |fb, bb0| {
        let raw = fb.push_param("raw", int_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Cast {
                value: op_place(raw, int_ty),
                target: enum_ty,
            }),
        );
    });

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CastMustConvertIntAndFloat { value, target })
            if value == int_ty && target == enum_ty
    )));
}

#[test]
fn function_param_local_must_match() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let module = test_module(&mut builder);
    let local = LocalId::from_index(0);
    let func = Function {
        name: Ident::new("bad_param"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("p")),
                ty: int_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
                role: ParamRole::Normal,
                local_id: local,
            }],
            int_ty,
        ),
        locals: vec![Local {
            name: Some(Ident::new("p")),
            binding: None,
            ty: bool_ty,
            mutability: Mutability::Immutable,
            kind: LocalKind::User,
        }],
        body: body_from_block(AirBlock {
            stmts: vec![],
            tail: term_return(op_place(local, bool_ty)),
        }),
    };
    let fid = builder.alloc_function(func);
    builder.set_entry(fid);
    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::ParamLocalMustBeArg { local: l, .. }) if l == local)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::ParamLocalTypeMismatch { expected, found, .. }) if expected == int_ty && found == bool_ty)));
}

#[test]
fn module_missing_wrong_and_duplicate_items_are_invalid() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let m0 = test_module(&mut builder);
    let m1 = builder.alloc_module(empty_module("other"));

    let mut fb = FunctionBuilder::new("missing", m0, FunctionKind::Normal, void_ty);
    fb.push_block(term_return_void());
    let missing = builder.alloc_function_raw(fb.finish());
    let missing_agg = builder.alloc_aggregate_raw(AggregateDecl {
        name: Ident::new("MissingAgg"),
        module: m0,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let missing_enum = builder.alloc_enum_raw(EnumDecl {
        name: Ident::new("MissingEnum"),
        module: m0,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![],
    });
    let missing_ext_ty = builder.alloc_extern_type_raw(ExternTypeDecl {
        name: Ident::new("MissingExtType"),
        module: m0,
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
    let missing_ext = builder.alloc_extern_raw(ExternDecl {
        name: Ident::new("missing_ext"),
        module: m0,
        member: ExternMember::FreeFunction,
        params: vec![],
        return_type: void_ty,
        abi: crate::air::ExternAbi::default(),
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });
    let missing_global = builder.alloc_global_raw(GlobalDecl {
        name: Ident::new("missing_global"),
        module: m0,
        ty: void_ty,
        mutability: Mutability::Immutable,
        init: FunctionId::from_index(99),
    });
    let wrong_global = builder.alloc_global_raw(GlobalDecl {
        name: Ident::new("wrong_global"),
        module: m1,
        ty: void_ty,
        mutability: Mutability::Immutable,
        init: FunctionId::from_index(99),
    });

    let mut fb = FunctionBuilder::new("wrong", m1, FunctionKind::Normal, void_ty);
    fb.push_block(term_return_void());
    let wrong = builder.alloc_function_raw(fb.finish());
    let module = builder.module_mut(m0);
    module.functions.push(wrong);
    module.functions.push(wrong);
    module.globals.push(wrong_global);
    module.globals.push(wrong_global);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Function(id))) if id == missing)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Aggregate(id))) if id == missing_agg)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Enum(id))) if id == missing_enum)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::ExternType(id))) if id == missing_ext_ty)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Extern(id))) if id == missing_ext)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Global(id))) if id == missing_global)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::ItemWrongModule { item: ModuleItem::Function(id), expected, found }) if id == wrong && expected == m0 && found == m1)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::ItemWrongModule { item: ModuleItem::Global(id), expected, found }) if id == wrong_global && expected == m0 && found == m1)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::DuplicateItem(ModuleItem::Function(id))) if id == wrong)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::DuplicateItem(ModuleItem::Global(id))) if id == wrong_global)));
}

#[test]
fn malformed_globals_are_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let module = test_module(&mut builder);
    let (global, init) = builder.alloc_global_with_init(module, "g", int_ty, Mutability::Mutable);
    let mut program = builder.finish();
    program.functions[init.index()]
        .signature
        .params
        .push(Param {
            name: Some(Ident::new("x")),
            ty: int_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
            role: ParamRole::Normal,
            local_id: LocalId::from_index(0),
        });
    program.functions[init.index()].signature.return_mode = ReturnMode::Value(bool_ty);

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::GlobalInitSignatureMismatch { global: id, init: found }) if id == global && found == init)));
}

#[test]
fn global_initializer_place_return_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (global, init) = builder.alloc_global_with_init(module, "g", int_ty, Mutability::Immutable);
    let mut program = builder.finish();
    program.functions[init.index()].signature.return_mode = ReturnMode::Place(int_ty);

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::GlobalInitSignatureMismatch { global: id, init: found }) if id == global && found == init)));
}

#[test]
fn global_initializer_backlink_mismatch_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (first, first_init) =
        builder.alloc_global_with_init(module, "first", int_ty, Mutability::Immutable);
    let (second, second_init) =
        builder.alloc_global_with_init(module, "second", int_ty, Mutability::Immutable);
    let mut program = builder.finish();
    program.functions[first_init.index()].kind = FunctionKind::GlobalInit(second);

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::GlobalInitKindMismatch { global, init }) if global == first && init == first_init)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::GlobalInitFunctionMismatch { global, init }) if global == second && init == second_init)));
}

#[test]
fn global_initializer_module_mismatch_is_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let other = builder.alloc_module(empty_module("other"));
    let (global, init) = builder.alloc_global_with_init(module, "g", int_ty, Mutability::Immutable);
    let mut program = builder.finish();
    program.functions[init.index()].module = other;

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::GlobalInitModuleMismatch { global: id, expected, found }) if id == global && expected == module && found == other)));
}

#[test]
fn global_root_set_invariants_are_rejected() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let module = test_module(&mut builder);
    let (mutable_global, _) =
        builder.alloc_global_with_init(module, "v", int_ty, Mutability::Mutable);
    let (immutable_global, _) =
        builder.alloc_global_with_init(module, "c", int_ty, Mutability::Immutable);
    let value = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let void_ty = builder.void_ty();
    let errors = verify_void_entry(builder, "main", module, void_ty, |fb, bb0| {
        fb.add_statement(
            bb0,
            AirStmt::GlobalEnsure {
                global: GlobalId::from_index(99),
            },
        );
        fb.add_statement(
            bb0,
            AirStmt::GlobalSetRoot {
                global: mutable_global,
                value: RValue::Use(op_const(value)),
                init: GlobalInitEffect::InitializeFirst,
            },
        );
        fb.add_statement(
            bb0,
            AirStmt::GlobalSetRoot {
                global: immutable_global,
                value: RValue::Use(op_const(value)),
                init: GlobalInitEffect::StoreWithoutInit,
            },
        );
        fb.add_statement(
            bb0,
            stmt_assign(
                Place {
                    root: PlaceRoot::Global(mutable_global),
                    projection: vec![],
                    ty: int_ty,
                },
                RValue::Use(op_const(value)),
            ),
        );
    });

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidGlobal(id)) if id == GlobalId::from_index(99)
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::GlobalSetRootInitMustStoreWithoutInit)
    )));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadStatement(BadStatement::GlobalSetRootTypeMismatch { expected, found }) if expected == int_ty && found == bool_ty)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadPlace(BadPlace::ImmutableRoot(PlaceRoot::Global(id))) if id == immutable_global)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadStatement(BadStatement::AssignGlobalRoot(id)) if id == mutable_global)));
}

#[test]
fn global_initializer_function_cannot_be_entry() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (_global, init) =
        builder.alloc_global_with_init(module, "g", int_ty, Mutability::Immutable);
    builder.set_entry(init);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(
        |e| matches!(e.kind, EK::BadFunction(BadFunction::EntryMustBeNamed(id)) if id == init)
    ));
}

#[test]
fn global_initializer_functions_are_not_source_callable() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (_global, init) =
        builder.alloc_global_with_init(module, "g", int_ty, Mutability::Immutable);
    let void_ty = builder.void_ty();
    let errors = verify_void_entry(builder, "main", module, void_ty, |fb, bb0| {
        fb.add_statement(
            bb0,
            stmt_eval(RValue::FunctionRef {
                function: init,
                ty: int_ty,
            }),
        );
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Call {
                callee: Callee::Function(init),
                args: vec![],
            }),
        );
    });

    assert!(errors.iter().any(
        |e| matches!(e.kind, EK::BadRValue(BadRValue::FunctionRefMustBeNamed(id)) if id == init)
    ));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadCall(BadCall::FunctionCalleeMustBeSourceCallable(id)) if id == init)));
}

#[test]
fn collection_loan_rejects_global_root_set() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let (global, _) = builder.alloc_global_with_init(module, "xs", list_ty, Mutability::Mutable);
    let mut fb = FunctionBuilder::new("loan", module, FunctionKind::Normal, void_ty);
    let source = fb.push_param("source", list_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: Place {
                    root: PlaceRoot::Global(global),
                    projection: vec![],
                    ty: list_ty,
                },
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::ReadonlySequence,
                body: AirBlock {
                    stmts: vec![AirStmt::GlobalSetRoot {
                        global,
                        value: RValue::Use(op_place(source, list_ty)),
                        init: GlobalInitEffect::StoreWithoutInit,
                    }],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanRootRebindConflict {
            mode: AirCollectionLoanMode::ReadonlySequence,
        })
    )));
}

#[test]
fn slice_view_rejects_non_sequence_source_and_non_slice_result() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("bad_slice_view", module, FunctionKind::Normal, void_ty);
    let scalar = fb.push_param("x", int_ty, ParamRole::Normal);
    let xs = fb.push_param("xs", list_ty, ParamRole::Normal);
    let start = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let end = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(bb0, stmt_init(start, RValue::Use(op_const(zero))));
    fb.add_statement(bb0, stmt_init(end, RValue::Use(op_const(one))));
    fb.add_statement(
        bb0,
        stmt_eval(RValue::SliceView {
            source: place(scalar, int_ty),
            start,
            end,
            inclusive: false,
            ty: list_ty,
        }),
    );
    fb.add_statement(
        bb0,
        stmt_eval(RValue::SliceView {
            source: place(xs, list_ty),
            start,
            end,
            inclusive: false,
            ty: list_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SliceViewSourceMustBeSequence(ty)) if ty == int_ty
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SliceViewResultMustBeSlice { expected_elem, found })
            if expected_elem == int_ty && found == list_ty
    )));
}

#[test]
fn map_entry_and_slice_view_indices_must_exist_and_be_int() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let map_ty = builder.alloc_type(TypeData::Map {
        key: int_ty,
        value: bool_ty,
        order: MapOrder::Insertion,
    });
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_indices", module, FunctionKind::Normal, int_ty);
    let list = fb.push_local(None, list_ty, Mutability::Immutable, LocalKind::User);
    let map = fb.push_local(None, map_ty, Mutability::Immutable, LocalKind::User);
    let bool_index = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MapEntryAt {
            map: place(map, map_ty),
            index: LocalId::from_index(99),
            ty: bool_ty,
        }),
    );
    fb.add_statement(
        bb0,
        stmt_eval(RValue::SliceView {
            source: place(list, list_ty),
            start: bool_index,
            end: LocalId::from_index(100),
            inclusive: false,
            ty: list_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidLocal(LocalId(99)))
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidLocal(LocalId(100)))
    )));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadFunction(BadFunction::SliceIndexMustBeInt { which: "start", found }) if found == bool_ty)));
}

#[test]
fn const_requires_matching_primitive_even_when_missing() {
    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.alloc_type(TypeData::Bool);
    builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Int(1),
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadConst(BadConst::MissingPrimitive(PrimitiveKind::Int))
    )));
}

#[test]
fn implicit_primitive_rvalues_require_canonical_result_type() {
    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let list_ty = builder.alloc_type(TypeData::List(bool_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new(
        "missing_rvalue_prims",
        module,
        FunctionKind::Normal,
        bool_ty,
    );
    let local_bool = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::User);
    let local_list = fb.push_local(None, list_ty, Mutability::Immutable, LocalKind::User);
    let bb0 = fb.push_block(term_return(op_place(local_bool, bool_ty)));
    fb.add_statement(
        bb0,
        stmt_init(
            local_bool,
            RValue::Len {
                source: place(local_list, list_ty),
            },
        ),
    );
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Stringify {
            value: op_place(local_bool, bool_ty),
            source_ty: bool_ty,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::MissingPrimitive(PrimitiveKind::Int))
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::MissingPrimitive(PrimitiveKind::String))
    )));
}

#[test]
fn stringify_operand_must_match_source_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    builder.string_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let errors = verify_void_entry(builder, "bad_stringify", module, void_ty, |fb, bb0| {
        let local = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Stringify {
                value: op_place(local, int_ty),
                source_ty: bool_ty,
            }),
        );
    });

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::StringifyOperandTypeMismatch { operand, source })
            if operand == int_ty && source == bool_ty
    )));
}

#[test]
fn stringify_operand_must_match_non_scalar_source_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    builder.string_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
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
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let errors = verify_void_entry(builder, "bad_stringify", module, void_ty, |fb, bb0| {
        let local = fb.push_local(None, aggregate_ty, Mutability::Immutable, LocalKind::User);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Stringify {
                value: op_place(local, aggregate_ty),
                source_ty: list_ty,
            }),
        );
    });

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::StringifyOperandTypeMismatch { operand, source })
            if operand == aggregate_ty && source == list_ty
    )));
}

#[test]
fn stringify_rejects_void_source_type() {
    let mut builder = ProgramBuilder::default();
    let string_ty = builder.string_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_void_stringify", module, FunctionKind::Normal, void_ty);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Stringify {
            value: op_const(builder.alloc_const(ConstData {
                ty: string_ty,
                value: ConstValue::String("bad".into()),
            })),
            source_ty: void_ty,
        }),
    );
    builder.alloc_function(fb.finish());
    let program = builder.finish();
    let errors = verify(&program).expect_err("expected void stringify rejection");

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::StringifyVoidSource { source }) if source == void_ty
    )));
}

#[test]
fn stringify_rejects_invalid_and_any_source_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let any_ty = builder.any_ty();
    builder.string_ty();
    let void_ty = builder.void_ty();
    let invalid_ty = TypeId::from_index(999);
    let module = test_module(&mut builder);
    let errors = verify_void_entry(builder, "bad_stringify", module, void_ty, |fb, bb0| {
        let int_local = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
        let any_local = fb.push_local(None, any_ty, Mutability::Immutable, LocalKind::User);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Stringify {
                value: op_place(int_local, int_ty),
                source_ty: invalid_ty,
            }),
        );
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Stringify {
                value: op_place(any_local, any_ty),
                source_ty: any_ty,
            }),
        );
    });

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(ty)) if ty == invalid_ty
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::StringifyAnySource { source }) if source == any_ty
    )));
}

#[test]
fn function_local_bindings_must_be_unique() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let first = fb.push_local(Some("a"), int_ty, Mutability::Immutable, LocalKind::User);
    let second = fb.push_local(Some("b"), int_ty, Mutability::Immutable, LocalKind::User);
    fb.bind_local(first, binding);
    fb.bind_local(second, binding);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateLocalBinding {
            binding: found_binding,
            first: found_first,
            second: found_second,
        }) if found_binding == binding && found_first == first && found_second == second
    )));
}

#[test]
fn local_binding_requires_binding_local_kind() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let local = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.bind_local(local, binding);
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::LocalBindingInvalidKind {
            local: found_local,
            kind: LocalKind::Temp,
        }) if found_local == local
    )));
}

#[test]
fn aggregate_stringify_override_invalid_function() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);
    builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("S"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: Some(FunctionId::from_index(999)),
    });
    let program = builder.finish();
    let errors = verify(&program).expect_err("expected invalid override");

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidFunction(id)) if id == FunctionId::from_index(999)
    )));
}

#[test]
fn aggregate_stringify_override_module_mismatch() {
    let mut builder = ProgramBuilder::default();
    let string_ty = builder.string_ty();
    let aggregate_module = test_module(&mut builder);
    let function_module = builder.alloc_module(empty_module("other"));
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("S"),
        module: aggregate_module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let aggregate_ty = builder.alloc_type(TypeData::Aggregate(aggregate));
    let mut fb = FunctionBuilder::new(
        "to_string",
        function_module,
        FunctionKind::Method,
        string_ty,
    );
    fb.push_param("self", aggregate_ty, ParamRole::Receiver);
    let string_const = builder.alloc_const(ConstData {
        ty: string_ty,
        value: ConstValue::String("S".into()),
    });
    fb.push_block(term_return(op_const(string_const)));
    let override_id = builder.alloc_function(fb.finish());
    let mut program = builder.finish();
    program.aggregate_mut(aggregate).stringify_override = Some(override_id);
    let errors = verify(&program).expect_err("expected module mismatch");

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::StringifyOverrideModuleMismatch { expected, found })
            if expected == aggregate_module && found == function_module
    )));
}

#[test]
fn aggregate_stringify_override_must_not_be_lambda_body() {
    let mut builder = ProgramBuilder::default();
    let string_ty = builder.string_ty();
    let module = test_module(&mut builder);
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("S"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: Some(FunctionId::from_index(0)),
    });
    let aggregate_ty = builder.alloc_type(TypeData::Aggregate(aggregate));
    let lambda = builder.alloc_lambda(LambdaDecl {
        source: crate::ast::ExprId(0),
        module,
        body: FunctionId::from_index(0),
        owner: FunctionId::from_index(0),
        signature: SignatureType::new(vec![], ReturnMode::Value(string_ty)),
        escape: LambdaEscape::NonEscaping,
        captures: vec![],
    });
    let mut fb = FunctionBuilder::new("to_string", module, FunctionKind::Lambda(lambda), string_ty);
    fb.push_param("self", aggregate_ty, ParamRole::Receiver);
    let string_const = builder.alloc_const(ConstData {
        ty: string_ty,
        value: ConstValue::String("S".into()),
    });
    fb.push_block(term_return(op_const(string_const)));
    assert_eq!(
        builder.alloc_function(fb.finish()),
        FunctionId::from_index(0)
    );

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::StringifyOverrideMustBeNamed(id))
            if id == FunctionId::from_index(0)
    )));
}

#[test]
fn aggregate_stringify_override_invalid_return_type_ref() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);
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
    let mut fb = FunctionBuilder::new(
        "to_string",
        module,
        FunctionKind::Method,
        TypeId::from_index(999),
    );
    fb.push_param("self", aggregate_ty, ParamRole::Receiver);
    fb.push_block(term_unreachable());
    let override_id = builder.alloc_function(fb.finish());
    let mut program = builder.finish();
    program.aggregate_mut(aggregate).stringify_override = Some(override_id);
    let errors = verify(&program).expect_err("expected invalid return type");

    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(ty)) if ty == TypeId::from_index(999)
    )));
}

#[test]
fn aggregate_stringify_override_wrong_shape() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let string_ty = builder.string_ty();
    let module = test_module(&mut builder);
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
    let other = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Other"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![],
        cycle_capable: false,
        stringify_override: None,
    });
    let other_ty = builder.alloc_type(TypeData::Aggregate(other));
    let wrong_kind_ty = builder.alloc_type(TypeData::DataRef(aggregate));

    let mut wrong_ret = FunctionBuilder::new("bad_ret", module, FunctionKind::Method, int_ty);
    wrong_ret.push_param("self", aggregate_ty, ParamRole::Receiver);
    wrong_ret.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    }))));
    let wrong_ret = builder.alloc_function(wrong_ret.finish());

    let place_ret_self = LocalId::from_index(0);
    let place_ret_value = LocalId::from_index(1);
    let place_ret = builder.alloc_function(Function {
        name: Ident::new("bad_ret_mode"),
        module,
        kind: FunctionKind::Method,
        owner: None,
        specialization: None,
        signature: Signature::with_return_mode(
            vec![Param {
                name: Some(Ident::new("self")),
                ty: aggregate_ty,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
                role: ParamRole::Receiver,
                local_id: place_ret_self,
            }],
            ReturnMode::Place(string_ty),
        ),
        locals: vec![
            Local {
                name: Some(Ident::new("self")),
                binding: None,
                ty: aggregate_ty,
                mutability: Mutability::Immutable,
                kind: LocalKind::Arg,
            },
            Local {
                name: Some(Ident::new("value")),
                binding: None,
                ty: string_ty,
                mutability: Mutability::Mutable,
                kind: LocalKind::User,
            },
        ],
        body: body_from_block(AirBlock {
            stmts: vec![],
            tail: term_return(op_place(place_ret_value, string_ty)),
        }),
    });

    let mut wrong_receiver =
        FunctionBuilder::new("bad_receiver", module, FunctionKind::Method, string_ty);
    wrong_receiver.push_param("self", other_ty, ParamRole::Receiver);
    wrong_receiver.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: string_ty,
        value: ConstValue::String("bad".into()),
    }))));
    let wrong_receiver = builder.alloc_function(wrong_receiver.finish());

    let mut wrong_kind = FunctionBuilder::new("bad_kind", module, FunctionKind::Method, string_ty);
    wrong_kind.push_param("self", wrong_kind_ty, ParamRole::Receiver);
    wrong_kind.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: string_ty,
        value: ConstValue::String("bad".into()),
    }))));
    let wrong_kind = builder.alloc_function(wrong_kind.finish());

    let mut no_receiver =
        FunctionBuilder::new("no_receiver", module, FunctionKind::Method, string_ty);
    no_receiver.push_param("self", aggregate_ty, ParamRole::Normal);
    no_receiver.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: string_ty,
        value: ConstValue::String("bad".into()),
    }))));
    let no_receiver = builder.alloc_function(no_receiver.finish());

    let mut program = builder.finish();
    for (function, expected) in [
        (wrong_ret, "ret"),
        (place_ret, "ret_mode"),
        (wrong_receiver, "receiver_ty"),
        (wrong_kind, "receiver_kind"),
        (no_receiver, "receiver_role"),
    ] {
        program.aggregate_mut(aggregate).stringify_override = Some(function);
        let errors = verify(&program).expect_err("expected invalid override");
        match expected {
            "ret" => assert!(errors.iter().any(|e| matches!(
                e.kind,
                EK::BadFunction(BadFunction::StringifyOverrideReturnMustBeString(ty)) if ty == int_ty
            ))),
            "ret_mode" => assert!(errors.iter().any(|e| matches!(
                e.kind,
                EK::BadFunction(BadFunction::StringifyOverrideReturnMustBeString(ty)) if ty == string_ty
            ))),
            "receiver_ty" => assert!(errors.iter().any(|e| matches!(
                e.kind,
                EK::BadFunction(BadFunction::StringifyOverrideReceiverTypeMismatch { expected: id, found })
                    if id == aggregate && found == other_ty
            ))),
            "receiver_kind" => assert!(errors.iter().any(|e| matches!(
                e.kind,
                EK::BadFunction(BadFunction::StringifyOverrideReceiverTypeMismatch { expected: id, found })
                    if id == aggregate && found == wrong_kind_ty
            ))),
            "receiver_role" => assert!(errors.iter().any(|e| matches!(
                e.kind,
                EK::BadFunction(BadFunction::StringifyOverrideMissingReceiver)
            ))),
            _ => unreachable!(),
        }
        program.aggregate_mut(aggregate).stringify_override = None;
    }
}

#[test]
fn aggregate_ctor_slot_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Pair"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![field("z", int_ty), field("a", bool_ty)],
        cycle_capable: false,
        stringify_override: None,
    });
    let aggregate_ty = builder.alloc_type(TypeData::Aggregate(aggregate));

    let errors = verify_void_entry(builder, "bad_struct_ctor", module, void_ty, |fb, bb0| {
        let p_i = fb.push_param("i", int_ty, ParamRole::Normal);
        let p_b = fb.push_param("b", bool_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::Struct(aggregate),
                fields: vec![op_place(p_b, bool_ty), op_place(p_i, int_ty)],
                ty: aggregate_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::AggregateCtorFieldTypeMismatch { aggregate: id, field: 0, expected, found })
            if id == aggregate && expected == int_ty && found == bool_ty
    )));
}

#[test]
fn enum_struct_ctor_slot_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Event"),
        module,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![VariantDecl {
            name: Ident::new("Hit"),
            shape: VariantShape::Struct(vec![field("z", int_ty), field("a", bool_ty)]),
            raw_value: None,
        }],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));

    let errors = verify_void_entry(builder, "bad_enum_ctor", module, void_ty, |fb, bb0| {
        let p_i = fb.push_param("i", int_ty, ParamRole::Normal);
        let p_b = fb.push_param("b", bool_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::EnumVariant {
                    enum_id,
                    variant: VariantId::from_index(0),
                },
                fields: vec![op_place(p_b, bool_ty), op_place(p_i, int_ty)],
                ty: enum_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::EnumCtorFieldTypeMismatch { enum_id: id, variant, field: 0, expected, found })
            if id == enum_id && variant == VariantId::from_index(0) && expected == int_ty && found == bool_ty
    )));
}

#[test]
fn dataref_field_projection_reaches_field_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let dataref = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![field("value", int_ty)],
        cycle_capable: true,
        stringify_override: None,
    });
    let dataref_ty = builder.alloc_type(TypeData::DataRef(dataref));

    let errors = verify_void_entry(
        builder,
        "bad_dataref_projection",
        module,
        void_ty,
        |fb, bb0| {
            let local = fb.push_local(None, dataref_ty, Mutability::Immutable, LocalKind::User);
            let bad_place = Place {
                root: PlaceRoot::Local(local),
                projection: vec![Projection::Field(FieldId::from_index(0))],
                ty: bool_ty,
            };
            fb.add_statement(bb0, stmt_eval(RValue::Use(Operand::Place(bad_place))));
        },
    );
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::PlaceTypeMismatch { expected, found }) if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn aggregate_ctor_field_count_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Pair"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![field("a", int_ty), field("b", int_ty)],
        cycle_capable: false,
        stringify_override: None,
    });
    let aggregate_ty = builder.alloc_type(TypeData::Aggregate(aggregate));

    let errors = verify_void_entry(builder, "bad_count", module, void_ty, |fb, bb0| {
        let p = fb.push_param("p", int_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::Struct(aggregate),
                fields: vec![op_place(p, int_ty)],
                ty: aggregate_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::AggregateCtorFieldCountMismatch { aggregate: id, expected: 2, found: 1 }) if id == aggregate
    )));
}

#[test]
fn array_ctor_shape_is_verified() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let array_ty = builder.alloc_type(TypeData::Array {
        elem: int_ty,
        len: 2,
    });
    let module = test_module(&mut builder);

    let errors = verify_void_entry(builder, "bad_array", module, void_ty, |fb, bb0| {
        let p = fb.push_param("p", bool_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::Array,
                fields: vec![op_place(p, bool_ty)],
                ty: array_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CollectionCtorFieldCountMismatch {
            ctor: AggregateCtor::Array,
            expected: 2,
            found: 1
        })
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CollectionCtorFieldTypeMismatch { ctor: AggregateCtor::Array, field: 0, expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn tuple_ctor_field_count_is_verified() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let tuple_ty = builder.alloc_type(TypeData::Tuple(vec![int_ty, bool_ty]));
    let module = test_module(&mut builder);

    let errors = verify_void_entry(builder, "bad_tuple", module, void_ty, |fb, bb0| {
        let p = fb.push_param("p", bool_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::Tuple,
                fields: vec![op_place(p, bool_ty)],
                ty: tuple_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CollectionCtorFieldCountMismatch {
            ctor: AggregateCtor::Tuple,
            expected: 2,
            found: 1
        })
    )));
}

#[test]
fn tuple_ctor_field_types_are_verified() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let tuple_ty = builder.alloc_type(TypeData::Tuple(vec![int_ty, bool_ty]));
    let module = test_module(&mut builder);

    let errors = verify_void_entry(builder, "bad_tuple_field", module, void_ty, |fb, bb0| {
        let lhs = fb.push_param("lhs", bool_ty, ParamRole::Normal);
        let rhs = fb.push_param("rhs", bool_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::Tuple,
                fields: vec![op_place(lhs, bool_ty), op_place(rhs, bool_ty)],
                ty: tuple_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CollectionCtorFieldTypeMismatch { ctor: AggregateCtor::Tuple, field: 0, expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn tuple_ctor_result_type_is_verified() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let errors = verify_void_entry(builder, "bad_tuple_result", module, void_ty, |fb, bb0| {
        let p = fb.push_param("p", int_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::Tuple,
                fields: vec![op_place(p, int_ty)],
                ty: int_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CollectionCtorResultTypeMismatch { ctor: AggregateCtor::Tuple, found })
            if found == int_ty
    )));
}

#[test]
fn list_ctor_shape_is_verified() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let module = test_module(&mut builder);

    let errors = verify_void_entry(builder, "bad_list", module, void_ty, |fb, bb0| {
        let p = fb.push_param("p", bool_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::List,
                fields: vec![op_place(p, bool_ty)],
                ty: list_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CollectionCtorFieldTypeMismatch { ctor: AggregateCtor::List, field: 0, expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn collection_ctor_result_type_is_verified() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let errors = verify_void_entry(
        builder,
        "bad_collection_result",
        module,
        void_ty,
        |fb, bb0| {
            let p = fb.push_param("p", int_ty, ParamRole::Normal);
            fb.add_statement(
                bb0,
                stmt_eval(RValue::Aggregate {
                    kind: AggregateCtor::Array,
                    fields: vec![op_place(p, int_ty)],
                    ty: int_ty,
                }),
            );
        },
    );
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::CollectionCtorResultTypeMismatch { ctor: AggregateCtor::Array, found })
            if found == int_ty
    )));
}

#[test]
fn aggregate_ctor_result_and_kind_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![field("value", int_ty)],
        cycle_capable: true,
        stringify_override: None,
    });

    let errors = verify_void_entry(builder, "bad_result_kind", module, void_ty, |fb, bb0| {
        let p = fb.push_param("p", int_ty, ParamRole::Normal);
        fb.add_statement(
            bb0,
            stmt_eval(RValue::Aggregate {
                kind: AggregateCtor::Struct(aggregate),
                fields: vec![op_place(p, int_ty)],
                ty: int_ty,
            }),
        );
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::AggregateCtorResultTypeMismatch { aggregate: id, expected: AggregateKind::Struct, found })
            if id == aggregate && found == int_ty
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::AggregateCtorKindMismatch { aggregate: id, expected: AggregateKind::Struct, found: AggregateKind::DataRef })
            if id == aggregate
    )));
}

#[test]
fn enum_ctor_unit_count_tuple_type_and_result_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("E"),
        module,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![
            VariantDecl {
                name: Ident::new("Unit"),
                shape: VariantShape::Unit,
                raw_value: None,
            },
            VariantDecl {
                name: Ident::new("Tuple"),
                shape: VariantShape::Tuple(vec![int_ty]),
                raw_value: None,
            },
        ],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));

    let errors = verify_void_entry(builder, "bad_enum", module, void_ty, |fb, bb0| {
        let p = fb.push_param("p", bool_ty, ParamRole::Normal);
        for (variant, ty) in [(0, int_ty), (1, enum_ty)] {
            fb.add_statement(
                bb0,
                stmt_eval(RValue::Aggregate {
                    kind: AggregateCtor::EnumVariant {
                        enum_id,
                        variant: VariantId::from_index(variant),
                    },
                    fields: vec![op_place(p, bool_ty)],
                    ty,
                }),
            );
        }
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::EnumCtorResultTypeMismatch { enum_id: id, found }) if id == enum_id && found == int_ty
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::EnumCtorFieldCountMismatch { enum_id: id, variant, expected: 0, found: 1 })
            if id == enum_id && variant == VariantId::from_index(0)
    )));
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadRValue(BadRValue::EnumCtorFieldTypeMismatch { enum_id: id, variant, field: 0, expected, found })
            if id == enum_id && variant == VariantId::from_index(1) && expected == int_ty && found == bool_ty
    )));
}

#[test]
fn dataref_field_projection_kind_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("S"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![field("value", int_ty)],
        cycle_capable: false,
        stringify_override: None,
    });
    let dataref_ty = builder.alloc_type(TypeData::DataRef(aggregate));

    let errors = verify_void_entry(builder, "bad_dataref_field", module, void_ty, |fb, bb0| {
        let local = fb.push_local(None, dataref_ty, Mutability::Immutable, LocalKind::User);
        let bad_kind = Place {
            root: PlaceRoot::Local(local),
            projection: vec![Projection::Field(FieldId::from_index(0))],
            ty: int_ty,
        };
        fb.add_statement(bb0, stmt_eval(RValue::Use(Operand::Place(bad_kind))));
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::FieldProjectionKindMismatch { aggregate: id, expected: AggregateKind::DataRef, found: AggregateKind::Struct })
            if id == aggregate
    )));
}

#[test]
fn dataref_field_projection_out_of_range() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let dataref = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![field("value", int_ty)],
        cycle_capable: true,
        stringify_override: None,
    });
    let dataref_ty = builder.alloc_type(TypeData::DataRef(dataref));

    let errors = verify_void_entry(builder, "dataref_oob", module, void_ty, |fb, bb0| {
        let local = fb.push_local(None, dataref_ty, Mutability::Immutable, LocalKind::User);
        let bad_place = Place {
            root: PlaceRoot::Local(local),
            projection: vec![Projection::Field(FieldId::from_index(9))],
            ty: int_ty,
        };
        fb.add_statement(bb0, stmt_eval(RValue::Use(Operand::Place(bad_place))));
    });
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidField { aggregate: id, field })
            if id == dataref && field == FieldId::from_index(9)
    )));
}

#[test]
fn optional_match_rejects_non_optional_discriminant_and_payload_type() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_optional_match", module, FunctionKind::Normal, int_ty);
    let int = fb.push_param("value", int_ty, ParamRole::Normal);
    let opt = fb.push_param("opt", opt_ty, ParamRole::Normal);
    let bad_payload = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_unreachable());
    let func_id = builder.alloc_function(fb.finish());
    let block = |discr, discr_ty, payload| AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::OptionalMatch(AirOptionalMatch {
                discr: place(discr, discr_ty),
                payload,
                payload_ref: false,
                payload_escapes: false,
                some_block: AirBlock::default(),
                none_block: AirBlock::default(),
            })],
            tail: AirTail::None,
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &block(int, int_ty, None)).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SwitchDiscriminantMustBeEnum(found)) if found == int_ty
    )));

    let errors = verify_structured_body(&program, func_id, &block(opt, opt_ty, Some(bad_payload)))
        .unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::InitTypeMismatch { expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn optional_match_rejects_invalid_escaping_payload() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_optional_escape", module, FunctionKind::Normal, int_ty);
    let opt = fb.push_param("opt", opt_ty, ParamRole::Normal);
    let payload = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::PatternBinding);
    fb.push_block(term_unreachable());
    let func_id = builder.alloc_function(fb.finish());
    let body = |payload, payload_ref, none_tail| AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::OptionalMatch(AirOptionalMatch {
                discr: place(opt, opt_ty),
                payload,
                payload_ref,
                payload_escapes: true,
                some_block: AirBlock::default(),
                none_block: AirBlock {
                    stmts: vec![],
                    tail: none_tail,
                },
            })],
            tail: AirTail::Unreachable,
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body(None, true, AirTail::Unreachable))
        .unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::OptionalPayloadEscapeRequiresPayload)
    )));

    let errors = verify_structured_body(
        &program,
        func_id,
        &body(Some(payload), false, AirTail::Unreachable),
    )
    .unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::OptionalPayloadEscapeRequiresRef)
    )));

    let errors =
        verify_structured_body(&program, func_id, &body(Some(payload), true, AirTail::None))
            .unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::OptionalPayloadEscapeNoneMustDiverge)
    )));
}

#[test]
fn optional_match_payload_is_not_initialized_in_none_branch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_optional_match", module, FunctionKind::Normal, int_ty);
    let opt = fb.push_param("opt", opt_ty, ParamRole::Normal);
    let payload = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_unreachable());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::OptionalMatch(AirOptionalMatch {
                discr: place(opt, opt_ty),
                payload: Some(payload),
                payload_ref: false,
                payload_escapes: false,
                some_block: AirBlock::default(),
                none_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(Some(op_place(payload, int_ty))),
                },
            })],
            tail: AirTail::Unreachable,
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(found)) if found == payload
    )));
}

#[test]
fn optional_match_rejects_reused_or_mutable_payload_local() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let module = test_module(&mut builder);
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut fb = FunctionBuilder::new("bad_optional_match", module, FunctionKind::Normal, int_ty);
    let opt = fb.push_param("opt", opt_ty, ParamRole::Normal);
    let reused = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let mutable = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::Temp);
    fb.push_block(term_unreachable());
    let func_id = builder.alloc_function(fb.finish());
    let optional_match = |payload| {
        AirStmt::OptionalMatch(AirOptionalMatch {
            discr: place(opt, opt_ty),
            payload: Some(payload),
            payload_ref: false,
            payload_escapes: false,
            some_block: AirBlock::default(),
            none_block: AirBlock::default(),
        })
    };
    let reused_body = AirBody {
        block: AirBlock {
            stmts: vec![
                stmt_init(reused, RValue::Use(op_const(zero))),
                optional_match(reused),
            ],
            tail: AirTail::Unreachable,
        },
    };
    let mutable_body = AirBody {
        block: AirBlock {
            stmts: vec![optional_match(mutable)],
            tail: AirTail::Unreachable,
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &reused_body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::OptionalPayloadLocalAlreadyInitialized(found)) if found == reused
    )));

    let errors = verify_structured_body(&program, func_id, &mutable_body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::OptionalPayloadLocalMustBeImmutable(found)) if found == mutable
    )));
}

#[test]
fn collection_loan_root_kind_and_mode_must_match() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("loan", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param("xs", list_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let root_kind_body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::Map,
                mode: AirCollectionLoanMode::ReadonlyMap,
                body: AirBlock::default(),
            })],
            tail: AirTail::Return(None),
        },
    };
    let mode_body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::ReadonlyMap,
                body: AirBlock::default(),
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &root_kind_body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanRootKindMismatch {
            root_kind: AirCollectionRootKind::Map,
            found,
        }) if found == list_ty
    )));

    let errors = verify_structured_body(&program, func_id, &mode_body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanModeRootKindMismatch {
            root_kind: AirCollectionRootKind::List,
            mode: AirCollectionLoanMode::ReadonlyMap,
        })
    )));
}

#[test]
fn collection_slot_locals_cannot_outlive_scope() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut fb = FunctionBuilder::new("loan", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let index = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let slot = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::Temp);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let out_of_scope_body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::MutableSequenceElement,
                body: AirBlock {
                    stmts: vec![
                        stmt_init(index, RValue::Use(op_const(zero))),
                        AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                            root: place(xs, list_ty),
                            index,
                            slots: vec![AirCollectionSlot {
                                kind: AirCollectionSlotKind::SequenceElement,
                                local: slot,
                                ty: int_ty,
                                mutable: true,
                            }],
                            body: AirBlock::default(),
                        }),
                        stmt_eval(RValue::Use(op_place(slot, int_ty))),
                    ],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &out_of_scope_body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanSlotOutOfScope(local)) if local == slot
    )));
}

#[test]
fn for_ref_alias_scoped_borrow_source_must_be_collection_slot() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let owner = FunctionId::from_index(0);
    let mut fb = FunctionBuilder::new("for_ref_alias", module, FunctionKind::Normal, void_ty);
    fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let slot = fb.push_local(
        Some("x"),
        int_ty,
        Mutability::Mutable,
        LocalKind::PatternBinding,
    );
    fb.bind_local(slot, binding);
    fb.push_block(term_return_void());
    let scoped = builder.alloc_scoped_borrow(ScopedBorrowDecl {
        owner,
        binding,
        source: ScopedBorrowSource::ForRefAlias {
            source: place(slot, int_ty),
        },
        ty: int_ty,
        mutability: Mutability::Mutable,
    });
    builder.alloc_function(fb.finish());

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::UnsupportedScopedBorrowProjection(id)) if id == scoped
    )));
}

#[test]
fn collection_slot_scope_requires_matching_active_loan() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut fb = FunctionBuilder::new("slot_scope", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let index = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let slot = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::Temp);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![
                stmt_init(index, RValue::Use(op_const(zero))),
                AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                    root: place(xs, list_ty),
                    index,
                    slots: vec![AirCollectionSlot {
                        kind: AirCollectionSlotKind::SequenceElement,
                        local: slot,
                        ty: int_ty,
                        mutable: true,
                    }],
                    body: AirBlock {
                        stmts: vec![stmt_eval(RValue::Use(op_place(slot, int_ty)))],
                        tail: AirTail::None,
                    },
                }),
            ],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanSlotOutOfScope(local)) if local == slot
    )));
}

#[test]
fn collection_slot_scope_index_must_be_int() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let truth = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let mut fb = FunctionBuilder::new("slot_scope", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let index = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::Temp);
    let slot = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::Temp);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::MutableSequenceElement,
                body: AirBlock {
                    stmts: vec![
                        stmt_init(index, RValue::Use(op_const(truth))),
                        AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                            root: place(xs, list_ty),
                            index,
                            slots: vec![AirCollectionSlot {
                                kind: AirCollectionSlotKind::SequenceElement,
                                local: slot,
                                ty: int_ty,
                                mutable: true,
                            }],
                            body: AirBlock::default(),
                        }),
                    ],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SliceIndexMustBeInt { which: "index", found }) if found == bool_ty
    )));
}

#[test]
fn collection_slot_locals_cannot_return() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("loan", module, FunctionKind::Normal, int_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let index = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let slot = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::Temp);
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::MutableSequenceElement,
                body: AirBlock {
                    stmts: vec![
                        stmt_init(index, RValue::Use(op_const(zero))),
                        AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                            root: place(xs, list_ty),
                            index,
                            slots: vec![AirCollectionSlot {
                                kind: AirCollectionSlotKind::SequenceElement,
                                local: slot,
                                ty: int_ty,
                                mutable: true,
                            }],
                            body: AirBlock {
                                stmts: vec![],
                                tail: AirTail::Return(Some(op_place(slot, int_ty))),
                            },
                        }),
                    ],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Unreachable,
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanSlotEscapesBody(local)) if local == slot
    )));
}

#[test]
fn collection_loan_rejects_same_root_structural_ops() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("loan", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::ReadonlySequence,
                body: AirBlock {
                    stmts: vec![stmt_eval(RValue::ListPush {
                        list: place(xs, list_ty),
                        value: op_const(one),
                    })],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanStructuralOpConflict {
            mode: AirCollectionLoanMode::ReadonlySequence,
            op: "ListPush",
        })
    )));
}

#[test]
fn collection_loan_rejects_root_rebinding() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("loan", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let ys = fb.push_param("ys", list_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::ReadonlySequence,
                body: AirBlock {
                    stmts: vec![stmt_assign(
                        place(xs, list_ty),
                        RValue::Use(op_place(ys, list_ty)),
                    )],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanRootRebindConflict {
            mode: AirCollectionLoanMode::ReadonlySequence,
        })
    )));
}

#[test]
fn collection_loan_rejects_containing_root_rebinding() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let outer_ty = builder.alloc_type(TypeData::List(list_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut fb = FunctionBuilder::new("loan", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", outer_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let ys = fb.push_param("ys", outer_ty, ParamRole::Normal);
    let index = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let mut root = place(xs, outer_ty);
    root.projection.push(Projection::Index(index));
    root.ty = list_ty;
    let body = AirBody {
        block: AirBlock {
            stmts: vec![
                stmt_init(index, RValue::Use(op_const(zero))),
                AirStmt::CollectionLoan(AirCollectionLoan {
                    root,
                    root_kind: AirCollectionRootKind::List,
                    mode: AirCollectionLoanMode::ReadonlySequence,
                    body: AirBlock {
                        stmts: vec![stmt_assign(
                            place(xs, outer_ty),
                            RValue::Use(op_place(ys, outer_ty)),
                        )],
                        tail: AirTail::None,
                    },
                }),
            ],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanRootRebindConflict {
            mode: AirCollectionLoanMode::ReadonlySequence,
        })
    )));
}

#[test]
fn escaping_lambda_cannot_capture_collection_slot() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let module = test_module(&mut builder);
    let lambda = LambdaId::from_index(0);
    let lambda_body_id = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let source = LocalId::from_index(2);
    let binding = BindingId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(void_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            owner,
            body: lambda_body_id,
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![LambdaCaptureDecl::ScopedLocal {
                binding,
                source: CaptureLocalSource {
                    owner,
                    local: source,
                },
                ty: int_ty,
                mutability: Mutability::Mutable,
            }],
        }),
        lambda
    );
    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), void_ty);
    lambda_body.push_block(term_return_void());
    assert_eq!(builder.alloc_function(lambda_body.finish()), lambda_body_id);

    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let index = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let slot = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::Temp);
    assert_eq!(slot, source);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    assert_eq!(func_id, owner);
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::MutableSequenceElement,
                body: AirBlock {
                    stmts: vec![
                        stmt_init(
                            index,
                            RValue::Use(op_const(builder.alloc_const(ConstData {
                                ty: int_ty,
                                value: ConstValue::Int(0),
                            }))),
                        ),
                        AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                            root: place(xs, list_ty),
                            index,
                            slots: vec![AirCollectionSlot {
                                kind: AirCollectionSlotKind::SequenceElement,
                                local: slot,
                                ty: int_ty,
                                mutable: true,
                            }],
                            body: AirBlock {
                                stmts: vec![stmt_eval(RValue::MakeLambda {
                                    lambda,
                                    captures: vec![LambdaCaptureArg::ScopedLocal {
                                        place: place(slot, int_ty),
                                    }],
                                    ty: lambda_ty,
                                })],
                                tail: AirTail::None,
                            },
                        }),
                    ],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::CollectionLoanSlotEscapesBody(local)) if local == slot
    )));
}

#[test]
fn pattern_match_literal_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let konst = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", int_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, int_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![AirPatternTest::Literal {
                            path: AirPatternPath::default(),
                            value: konst,
                        }],
                        bindings: vec![],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadConst(BadConst::TypeMismatch { expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn pattern_match_binding_type_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", int_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        bool_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, int_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![],
                        bindings: vec![AirPatternBinding {
                            local: binding,
                            path: AirPatternPath::default(),
                            ty: bool_ty,
                            mode: AirPatternBindingMode::Owned,
                        }],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::InitTypeMismatch { expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn pattern_match_rejects_invalid_binding_path() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", int_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        int_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, int_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![],
                        bindings: vec![AirPatternBinding {
                            local: binding,
                            path: AirPatternPath {
                                steps: vec![AirPatternPathStep::TupleField(0)],
                            },
                            ty: int_ty,
                            mode: AirPatternBindingMode::Owned,
                        }],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::None,
                    },
                }],
            })],
            tail: AirTail::Return(Some(op_place(binding, int_ty))),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, EK::BadFunction(BadFunction::PatternPathInvalid)))
    );
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(local)) if local == binding
    )));
}

#[test]
fn pattern_match_rejects_unguarded_enum_payload_path() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("E"),
        module,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![VariantDecl {
            name: Ident::new("V"),
            shape: VariantShape::Tuple(vec![int_ty]),
            raw_value: None,
        }],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", enum_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        int_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, enum_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![],
                        bindings: vec![AirPatternBinding {
                            local: binding,
                            path: AirPatternPath {
                                steps: vec![AirPatternPathStep::EnumTupleField {
                                    enum_id,
                                    variant: VariantId::from_index(0),
                                    field: 0,
                                }],
                            },
                            ty: int_ty,
                            mode: AirPatternBindingMode::Owned,
                        }],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::PatternPayloadWithoutVariantTest)
    )));
}

#[test]
fn pattern_match_rejects_enum_tuple_path_for_struct_variant() {
    assert_pattern_payload_shape_mismatch(
        |int_ty| VariantShape::Struct(vec![field("value", int_ty)]),
        |enum_id, variant| AirPatternPathStep::EnumTupleField {
            enum_id,
            variant,
            field: 0,
        },
    );
}

#[test]
fn pattern_match_rejects_enum_struct_path_for_tuple_variant() {
    assert_pattern_payload_shape_mismatch(
        |int_ty| VariantShape::Tuple(vec![int_ty]),
        |enum_id, variant| AirPatternPathStep::EnumStructField {
            enum_id,
            variant,
            field: 0,
        },
    );
}

fn assert_pattern_payload_shape_mismatch(
    shape: impl FnOnce(TypeId) -> VariantShape,
    step: impl FnOnce(EnumId, VariantId) -> AirPatternPathStep,
) {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Slot"),
        module,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![VariantDecl {
            name: Ident::new("Item"),
            shape: shape(int_ty),
            raw_value: None,
        }],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));
    let variant = VariantId::from_index(0);
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", enum_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        int_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, enum_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![AirPatternTest::EnumVariant {
                            path: AirPatternPath::default(),
                            enum_id,
                            variant,
                        }],
                        bindings: vec![AirPatternBinding {
                            local: binding,
                            path: AirPatternPath {
                                steps: vec![step(enum_id, variant)],
                            },
                            ty: int_ty,
                            mode: AirPatternBindingMode::Owned,
                        }],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, EK::BadFunction(BadFunction::PatternPathInvalid)))
    );
}

#[test]
fn pattern_match_rejects_alternative_binding_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", int_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        int_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, int_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![
                        AirPatternAlternative {
                            tests: vec![],
                            bindings: vec![AirPatternBinding {
                                local: binding,
                                path: AirPatternPath::default(),
                                ty: int_ty,
                                mode: AirPatternBindingMode::Owned,
                            }],
                        },
                        AirPatternAlternative {
                            tests: vec![],
                            bindings: vec![],
                        },
                    ],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::PatternAlternativeBindingMismatch(local)) if local == binding
    )));
}

#[test]
fn pattern_match_rejects_second_alternative_bad_binding_path() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", int_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        int_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, int_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![
                        AirPatternAlternative {
                            tests: vec![],
                            bindings: vec![AirPatternBinding {
                                local: binding,
                                path: AirPatternPath::default(),
                                ty: int_ty,
                                mode: AirPatternBindingMode::Owned,
                            }],
                        },
                        AirPatternAlternative {
                            tests: vec![],
                            bindings: vec![AirPatternBinding {
                                local: binding,
                                path: AirPatternPath {
                                    steps: vec![AirPatternPathStep::TupleField(0)],
                                },
                                ty: int_ty,
                                mode: AirPatternBindingMode::Owned,
                            }],
                        },
                    ],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, EK::BadFunction(BadFunction::PatternPathInvalid)))
    );
}

#[test]
fn pattern_match_rejects_unguarded_optional_payload_path() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", opt_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        int_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, opt_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![],
                        bindings: vec![AirPatternBinding {
                            local: binding,
                            path: AirPatternPath {
                                steps: vec![AirPatternPathStep::OptionalSome],
                            },
                            ty: int_ty,
                            mode: AirPatternBindingMode::Owned,
                        }],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::PatternPayloadWithoutVariantTest)
    )));
}

#[test]
fn pattern_match_rejects_unguarded_optional_payload_test_path() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let value = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", opt_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, opt_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![AirPatternTest::Literal {
                            path: AirPatternPath {
                                steps: vec![AirPatternPathStep::OptionalSome],
                            },
                            value,
                        }],
                        bindings: vec![],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::PatternPayloadWithoutVariantTest)
    )));
}

#[test]
fn pattern_match_rejects_optional_payload_test_before_guard() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let value = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", opt_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, opt_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![
                            AirPatternTest::Literal {
                                path: AirPatternPath {
                                    steps: vec![AirPatternPathStep::OptionalSome],
                                },
                                value,
                            },
                            AirPatternTest::OptionalSome {
                                path: AirPatternPath::default(),
                            },
                        ],
                        bindings: vec![],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::Return(None),
                    },
                }],
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::PatternPayloadWithoutVariantTest)
    )));
}

#[test]
fn pattern_match_owned_binding_does_not_escape_arm() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", int_ty, ParamRole::Normal);
    let binding = fb.push_local(
        None,
        int_ty,
        Mutability::Immutable,
        LocalKind::PatternBinding,
    );
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, int_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![],
                        bindings: vec![AirPatternBinding {
                            local: binding,
                            path: AirPatternPath::default(),
                            ty: int_ty,
                            mode: AirPatternBindingMode::Owned,
                        }],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::None,
                    },
                }],
            })],
            tail: AirTail::Return(Some(op_place(binding, int_ty))),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(local)) if local == binding
    )));
}

#[test]
fn pattern_match_alias_binding_does_not_escape_arm() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Slot"),
        module,
        core: None,
        repr: crate::air::EnumRepr::Adt,
        raw_type: None,
        type_args: vec![],
        const_args: vec![],
        variants: vec![VariantDecl {
            name: Ident::new("Value"),
            shape: VariantShape::Tuple(vec![int_ty]),
            raw_value: None,
        }],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));
    let mut fb = FunctionBuilder::new("bad_pattern", module, FunctionKind::Normal, void_ty);
    let subject = fb.push_param("x", enum_ty, ParamRole::Normal);
    let binding = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::PatternBinding);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::PatternMatch(AirPatternMatch {
                subject: place(subject, enum_ty),
                arms: vec![AirPatternArm {
                    alternatives: vec![AirPatternAlternative {
                        tests: vec![AirPatternTest::EnumVariant {
                            path: AirPatternPath::default(),
                            enum_id,
                            variant: VariantId::from_index(0),
                        }],
                        bindings: vec![AirPatternBinding {
                            local: binding,
                            path: AirPatternPath {
                                steps: vec![AirPatternPathStep::EnumTupleField {
                                    enum_id,
                                    variant: VariantId::from_index(0),
                                    field: 0,
                                }],
                            },
                            ty: int_ty,
                            mode: AirPatternBindingMode::Alias,
                        }],
                    }],
                    block: AirBlock {
                        stmts: vec![],
                        tail: AirTail::None,
                    },
                }],
            })],
            tail: AirTail::Return(Some(op_place(binding, int_ty))),
        },
    };
    let program = builder.finish();

    let errors = verify_structured_body(&program, func_id, &body).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadStatement(BadStatement::ReadUninitializedLocal(local)) if local == binding
    )));
}
