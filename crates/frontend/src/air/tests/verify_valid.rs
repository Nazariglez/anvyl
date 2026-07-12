use super::{super::verify::verify_structured_body, *};
use crate::{
    air::{FunctionValueCapability, MapWriteKind},
    ast::Ident,
};
#[test]
fn local_root_reads_verify() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("local_root", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param("x", int_ty, ParamRole::Normal);
    fb.push_block(term_return(op_place(local, int_ty)));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    expect_verified(&builder.finish());
}

#[test]
fn explicit_function_value_capability_verifies_escaping_arg() {
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

    let target = FunctionId::from_index(1);
    let mut callback = FunctionBuilder::new("callback", module, FunctionKind::Normal, int_ty);
    callback.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(callback.finish()), target);

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let function = main.push_local(None, lambda_ty, Mutability::Immutable, LocalKind::Temp);
    let pair = main.push_local(None, tuple_ty, Mutability::Immutable, LocalKind::Temp);
    let temp = main.push_local(None, lambda_ty, Mutability::Immutable, LocalKind::Temp);
    let bb0 = main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    main.add_statement(
        bb0,
        stmt_init(
            function,
            RValue::FunctionRef {
                function: target,
                ty: lambda_ty,
            },
        ),
    );
    main.add_statement(
        bb0,
        stmt_init(
            pair,
            RValue::Aggregate {
                kind: AggregateCtor::Tuple,
                fields: vec![op_place(function, lambda_ty)],
                ty: tuple_ty,
            },
        ),
    );
    main.add_statement(
        bb0,
        stmt_init(
            temp,
            RValue::FunctionValue {
                value: Operand::Place(Place {
                    root: PlaceRoot::Local(pair),
                    projection: vec![Projection::TupleField(0)],
                    ty: lambda_ty,
                }),
                capability: FunctionValueCapability::Escaping,
            },
        ),
    );
    main.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee),
            args: vec![CallArg::Value(op_place(temp, lambda_ty))],
        }),
    );
    let main = builder.alloc_function(main.finish());
    builder.set_entry(main);

    expect_verified(&builder.finish());
}

#[test]
fn explicit_function_value_capability_verifies_projected_storage() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig));
    let tuple_ty = builder.alloc_type(TypeData::Tuple(vec![lambda_ty]));

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let pair = main.push_param("pair", tuple_ty, ParamRole::Normal);
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
                value: Operand::Place(Place {
                    root: PlaceRoot::Local(pair),
                    projection: vec![Projection::TupleField(0)],
                    ty: lambda_ty,
                }),
                capability: FunctionValueCapability::Escaping,
            },
        ),
    );
    let main = builder.alloc_function(main.finish());
    builder.set_entry(main);

    expect_verified(&builder.finish());
}

#[test]
fn global_with_initializer_verifies() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let (global, init) = builder.alloc_global_with_init(module, "g", int_ty, Mutability::Mutable);
    let mut fb = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::Global(global),
        projection: vec![],
        ty: int_ty,
    })));
    let main = builder.alloc_function(fb.finish());
    builder.set_entry(main);

    let program = builder.finish();
    assert!(matches!(
        program.functions[init.index()].kind,
        FunctionKind::GlobalInit(id) if id == global
    ));
    expect_verified(&program);
}

#[test]
fn non_local_roots_verify_with_declarations() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(1),
        BindingId::from_index(1),
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner: FunctionId::from_index(1),
        source_local: LocalId::from_index(1),
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let (global, _) = builder.alloc_global_with_init(module, "g", int_ty, Mutability::Mutable);

    let mut fb = FunctionBuilder::new("roots", module, FunctionKind::Normal, int_ty);
    let param = fb.push_param_with_mode("p", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(param, BindingId::from_index(1));
    let x = fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User);
    fb.bind_local(x, BindingId::from_index(0));
    let tmp = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let bb0 = fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::Global(global),
        projection: vec![],
        ty: int_ty,
    })));
    fb.add_statement(
        bb0,
        stmt_init(
            tmp,
            RValue::Use(Operand::Place(Place {
                root: PlaceRoot::ScopedBorrow(scoped),
                projection: vec![],
                ty: int_ty,
            })),
        ),
    );
    fb.add_statement(
        bb0,
        stmt_assign(
            Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int_ty,
            },
            RValue::Use(op_place(tmp, int_ty)),
        ),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    expect_verified(&builder.finish());
}

#[test]
fn source_mut_param_scoped_borrow_verifies() {
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

    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let local = fb.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.bind_local(local, binding);
    fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::ScopedBorrow(scoped),
        projection: vec![],
        ty: int_ty,
    })));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    expect_verified(&builder.finish());
}

#[test]
fn source_ref_call_accepts_scoped_borrow_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);

    let mut callee = FunctionBuilder::new("inc", module, FunctionKind::Normal, void_ty);
    let arg = callee.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    assert_eq!(arg, LocalId::from_index(0));
    let inc = builder.alloc_function(callee.finish());

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
    owner.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(inc),
            args: vec![CallArg::MutBorrow(Place {
                root: PlaceRoot::ScopedBorrow(scoped),
                projection: vec![],
                ty: int_ty,
            })],
        }),
    );
    let owner = builder.alloc_function(owner.finish());
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn source_ref_call_accepts_lambda_slot_scoped_borrow() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);

    let mut callee = FunctionBuilder::new("inc", module, FunctionKind::Normal, void_ty);
    callee.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    let inc = builder.alloc_function(callee.finish());

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
    lambda_body.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(inc),
            args: vec![CallArg::MutBorrow(Place {
                root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
                projection: vec![],
                ty: int_ty,
            })],
        }),
    );
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);

    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, void_ty);
    let local = owner.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(local, binding);
    owner.push_block(term_return_void());
    let owner = builder.alloc_function(owner.finish());
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn distinct_scoped_borrows_do_not_alias() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let mut callee = FunctionBuilder::new("both", module, FunctionKind::Normal, void_ty);
    callee.push_param_with_mode("a", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_param_with_mode("b", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    let both = builder.alloc_function(callee.finish());

    let first_binding = BindingId::from_index(0);
    let second_binding = BindingId::from_index(1);
    let first = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(1),
        first_binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let second = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(1),
        second_binding,
        LocalId::from_index(1),
        int_ty,
        Mutability::Mutable,
    ));

    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, void_ty);
    let a = owner.push_param_with_mode("a", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(a, first_binding);
    let b = owner.push_param_with_mode("b", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(b, second_binding);
    let bb0 = owner.push_block(term_return_void());
    owner.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(both),
            args: vec![
                CallArg::MutBorrow(Place {
                    root: PlaceRoot::ScopedBorrow(first),
                    projection: vec![],
                    ty: int_ty,
                }),
                CallArg::MutBorrow(Place {
                    root: PlaceRoot::ScopedBorrow(second),
                    projection: vec![],
                    ty: int_ty,
                }),
            ],
        }),
    );
    let owner = builder.alloc_function(owner.finish());
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn scoped_borrow_and_distinct_local_do_not_alias() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let mut callee = FunctionBuilder::new("both", module, FunctionKind::Normal, void_ty);
    callee.push_param_with_mode("a", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_param_with_mode("b", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    callee.push_block(term_return_void());
    let both = builder.alloc_function(callee.finish());

    let first_binding = BindingId::from_index(0);
    let second_binding = BindingId::from_index(1);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(1),
        first_binding,
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));

    let mut owner = FunctionBuilder::new("owner", module, FunctionKind::Normal, void_ty);
    let x = owner.push_param_with_mode("x", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(x, first_binding);
    let y = owner.push_param_with_mode("y", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    owner.bind_local(y, second_binding);
    let bb0 = owner.push_block(term_return_void());
    owner.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(both),
            args: vec![
                CallArg::MutBorrow(Place {
                    root: PlaceRoot::ScopedBorrow(scoped),
                    projection: vec![],
                    ty: int_ty,
                }),
                CallArg::MutBorrow(place(y, int_ty)),
            ],
        }),
    );
    let owner = builder.alloc_function(owner.finish());
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn capture_cell_is_shared_by_lambdas_and_owner() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let lambda_sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(lambda_sig.clone()));
    let lambda_a = LambdaId::from_index(0);
    let lambda_b = LambdaId::from_index(1);
    let body_a = FunctionId::from_index(0);
    let body_b = FunctionId::from_index(1);
    let owner = FunctionId::from_index(2);
    let source_local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    for (lambda, body) in [(lambda_a, body_a), (lambda_b, body_b)] {
        assert_eq!(
            builder.alloc_lambda(LambdaDecl {
                source: crate::ast::ExprId(lambda.index() as u64),
                module,
                body,
                owner,
                signature: lambda_sig.clone(),
                escape: LambdaEscape::Escaping,
                captures: vec![LambdaCaptureDecl::CaptureCell {
                    binding,
                    cell,
                    ty: int_ty,
                }],
            }),
            lambda
        );
        let mut fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
        fb.push_block(term_return(Operand::Place(Place {
            root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
            projection: vec![],
            ty: int_ty,
        })));
        assert_eq!(builder.alloc_function(fb.finish()), body);
    }

    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        fb.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        source_local
    );
    fb.bind_local(source_local, binding);
    let bb0 = fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::CaptureCell(cell),
        projection: vec![],
        ty: int_ty,
    })));
    fb.add_statement(
        bb0,
        stmt_assign(
            Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int_ty,
            },
            RValue::Use(op_const(zero)),
        ),
    );
    for lambda in [lambda_a, lambda_b] {
        fb.add_statement(
            bb0,
            stmt_eval(RValue::MakeLambda {
                lambda,
                captures: vec![LambdaCaptureArg::CaptureCell { cell }],
                ty: lambda_ty,
            }),
        );
    }
    fb.add_statement(
        bb0,
        stmt_assign(
            Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int_ty,
            },
            RValue::Use(op_const(one)),
        ),
    );
    assert_eq!(builder.alloc_function(fb.finish()), owner);
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn nested_lambda_forwards_capture_cell() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let binding = BindingId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    let outer_lambda = LambdaId::from_index(0);
    let inner_lambda = LambdaId::from_index(1);
    let outer_body = FunctionId::from_index(0);
    let inner_body = FunctionId::from_index(1);
    let owner = FunctionId::from_index(2);
    let source_local = LocalId::from_index(0);
    let cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    for (lambda, body) in [(outer_lambda, outer_body), (inner_lambda, inner_body)] {
        assert_eq!(
            builder.alloc_lambda(LambdaDecl {
                source: crate::ast::ExprId(lambda.index() as u64),
                module,
                body,
                owner: if lambda == outer_lambda {
                    owner
                } else {
                    outer_body
                },
                signature: sig.clone(),
                escape: LambdaEscape::Escaping,
                captures: vec![LambdaCaptureDecl::CaptureCell {
                    binding,
                    cell,
                    ty: int_ty,
                }],
            }),
            lambda
        );
    }

    let mut outer = FunctionBuilder::new(
        "outer_lambda",
        module,
        FunctionKind::Lambda(outer_lambda),
        int_ty,
    );
    let bb0 = outer.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
        projection: vec![],
        ty: int_ty,
    })));
    outer.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda: inner_lambda,
            captures: vec![LambdaCaptureArg::CaptureCell { cell }],
            ty: lambda_ty,
        }),
    );
    assert_eq!(builder.alloc_function(outer.finish()), outer_body);

    let mut inner = FunctionBuilder::new(
        "inner_lambda",
        module,
        FunctionKind::Lambda(inner_lambda),
        int_ty,
    );
    inner.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(inner.finish()), inner_body);

    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    assert_eq!(
        main.push_local(Some("x"), int_ty, Mutability::Mutable, LocalKind::User),
        source_local
    );
    main.bind_local(source_local, binding);
    let bb0 = main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    main.add_statement(
        bb0,
        stmt_assign(
            Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int_ty,
            },
            RValue::Use(op_const(zero)),
        ),
    );
    main.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda: outer_lambda,
            captures: vec![LambdaCaptureArg::CaptureCell { cell }],
            ty: lambda_ty,
        }),
    );
    assert_eq!(builder.alloc_function(main.finish()), owner);
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn distinct_capture_cell_call_args_do_not_alias() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let callee = FunctionId::from_index(0);
    let body = FunctionId::from_index(1);
    let owner = FunctionId::from_index(2);
    let lambda = LambdaId::from_index(0);
    let first_binding = BindingId::from_index(0);
    let second_binding = BindingId::from_index(1);
    let first_local = LocalId::from_index(0);
    let second_local = LocalId::from_index(1);
    let first_cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: first_binding,
        owner,
        source_local: first_local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });
    let second_cell = builder.alloc_capture_cell(CaptureCellDecl {
        binding: second_binding,
        owner,
        source_local: second_local,
        ty: int_ty,
        lifetime: CaptureCellLifetime::Function,
    });

    let mut both = FunctionBuilder::new("both", module, FunctionKind::Normal, void_ty);
    both.push_param_with_mode("a", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    both.push_param_with_mode("b", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    both.push_block(term_return_void());
    assert_eq!(builder.alloc_function(both.finish()), callee);

    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            owner,
            body,
            signature: SignatureType::new(vec![], ReturnMode::Value(void_ty)),
            escape: LambdaEscape::NonEscaping,
            captures: vec![
                LambdaCaptureDecl::CaptureCell {
                    binding: first_binding,
                    cell: first_cell,
                    ty: int_ty,
                },
                LambdaCaptureDecl::CaptureCell {
                    binding: second_binding,
                    cell: second_cell,
                    ty: int_ty,
                },
            ],
        }),
        lambda
    );
    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), void_ty);
    let bb0 = lambda_body.push_block(term_return_void());
    lambda_body.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(callee),
            args: vec![
                CallArg::MutBorrow(Place {
                    root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
                    projection: vec![],
                    ty: int_ty,
                }),
                CallArg::MutBorrow(Place {
                    root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(1)),
                    projection: vec![],
                    ty: int_ty,
                }),
            ],
        }),
    );
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);

    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let lambda_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![],
        ReturnMode::Value(void_ty),
    )));
    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, void_ty);
    assert_eq!(
        main.push_local(Some("a"), int_ty, Mutability::Mutable, LocalKind::User),
        first_local
    );
    main.bind_local(first_local, first_binding);
    assert_eq!(
        main.push_local(Some("b"), int_ty, Mutability::Mutable, LocalKind::User),
        second_local
    );
    main.bind_local(second_local, second_binding);
    let bb0 = main.push_block(term_return_void());
    for cell in [first_cell, second_cell] {
        main.add_statement(
            bb0,
            stmt_assign(
                Place {
                    root: PlaceRoot::CaptureCell(cell),
                    projection: vec![],
                    ty: int_ty,
                },
                RValue::Use(op_const(zero)),
            ),
        );
    }
    main.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![
                LambdaCaptureArg::CaptureCell { cell: first_cell },
                LambdaCaptureArg::CaptureCell { cell: second_cell },
            ],
            ty: lambda_ty,
        }),
    );
    assert_eq!(builder.alloc_function(main.finish()), owner);
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn nonescaping_lambda_body_may_use_declared_scoped_borrow() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let binding = BindingId::from_index(0);
    let scoped = builder.alloc_scoped_borrow(scoped_mut_param_borrow(
        FunctionId::from_index(1),
        BindingId::from_index(0),
        LocalId::from_index(0),
        int_ty,
        Mutability::Mutable,
    ));
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner: FunctionId::from_index(1),
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
    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    lambda_body.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);
    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let param = main.push_param_with_mode("p", int_ty, ParamMode::MutBorrow, ParamRole::Normal);
    main.bind_local(param, binding);
    main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    let main = builder.alloc_function(main.finish());
    builder.set_entry(main);

    expect_verified(&builder.finish());
}

#[test]
fn readonly_local_capture_uses_capture_slot_root() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let binding = BindingId::from_index(0);
    let source = LocalId::from_index(0);
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
            captures: vec![LambdaCaptureDecl::ReadonlyLocal {
                binding,
                source: CaptureLocalSource {
                    owner,
                    local: source,
                },
                ty: int_ty,
            }],
        }),
        lambda
    );

    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    lambda_body.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
        projection: vec![],
        ty: int_ty,
    })));
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, void_ty);
    let captured = main.push_param("x", int_ty, ParamRole::Normal);
    assert_eq!(captured, source);
    main.bind_local(captured, binding);
    let bb0 = main.push_block(term_return_void());
    main.add_statement(
        bb0,
        stmt_eval(RValue::MakeLambda {
            lambda,
            captures: vec![LambdaCaptureArg::ReadonlyLocal {
                value: op_place(captured, int_ty),
            }],
            ty: lambda_ty,
        }),
    );
    assert_eq!(builder.alloc_function(main.finish()), owner);
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn lambda_value_and_function_ref_are_valid() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let lambda = LambdaId::from_index(0);
    let body_id = FunctionId::from_index(0);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body: body_id,
            owner: FunctionId::from_index(2),
            signature: sig,
            escape: LambdaEscape::NonEscaping,
            captures: vec![LambdaCaptureDecl::NoRuntime {
                binding: BindingId::from_index(0),
                ty: int_ty,
            }],
        }),
        lambda
    );

    let mut body = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    body.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(body.finish()), body_id);
    let mut target = FunctionBuilder::new("target", module, FunctionKind::Normal, int_ty);
    target.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(2),
    }))));
    let target = builder.alloc_function(target.finish());

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, lambda_ty);
    let tmp = main.push_local(None, lambda_ty, Mutability::Immutable, LocalKind::Temp);
    let bb0 = main.push_block(term_return(Operand::Place(place(tmp, lambda_ty))));
    main.add_statement(
        bb0,
        stmt_init(
            tmp,
            RValue::MakeLambda {
                lambda,
                captures: vec![LambdaCaptureArg::NoRuntime],
                ty: lambda_ty,
            },
        ),
    );
    main.add_statement(
        bb0,
        stmt_eval(RValue::FunctionRef {
            function: target,
            ty: lambda_ty,
        }),
    );
    let main_id = builder.alloc_function(main.finish());
    builder.set_entry(main_id);

    expect_verified(&builder.finish());
}

#[test]
fn escaping_function_param_accepts_known_escaping_values() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let lambda_ty = builder.alloc_type(TypeData::Function(sig.clone()));

    let accept_id = FunctionId::from_index(0);
    let mut accept = FunctionBuilder::new("accept", module, FunctionKind::Normal, int_ty);
    accept.push_param("f", lambda_ty, ParamRole::Normal);
    accept.set_param_escape(0, ParamEscape::Escaping);
    accept.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(accept.finish()), accept_id);

    let target_id = FunctionId::from_index(1);
    let mut target = FunctionBuilder::new("target", module, FunctionKind::Normal, int_ty);
    target.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(2),
    }))));
    assert_eq!(builder.alloc_function(target.finish()), target_id);

    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(2);
    let owner = FunctionId::from_index(3);
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            owner,
            body,
            signature: sig,
            escape: LambdaEscape::Escaping,
            captures: vec![],
        }),
        lambda
    );
    let mut lambda_body =
        FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    lambda_body.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(3),
    }))));
    assert_eq!(builder.alloc_function(lambda_body.finish()), body);

    let mut main = FunctionBuilder::new("main", module, FunctionKind::Normal, int_ty);
    let named = main.push_local(
        Some("named"),
        lambda_ty,
        Mutability::Immutable,
        LocalKind::Temp,
    );
    let owned = main.push_local(
        Some("owned"),
        lambda_ty,
        Mutability::Immutable,
        LocalKind::Temp,
    );
    let bb0 = main.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(4),
    }))));
    main.add_statement(
        bb0,
        stmt_init(
            named,
            RValue::FunctionRef {
                function: target_id,
                ty: lambda_ty,
            },
        ),
    );
    main.add_statement(
        bb0,
        stmt_init(
            owned,
            RValue::MakeLambda {
                lambda,
                captures: vec![],
                ty: lambda_ty,
            },
        ),
    );
    for local in [named, owned] {
        main.add_statement(
            bb0,
            stmt_eval(RValue::Call {
                callee: Callee::Function(accept_id),
                args: vec![CallArg::Value(op_place(local, lambda_ty))],
            }),
        );
    }
    assert_eq!(builder.alloc_function(main.finish()), owner);
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn escaping_function_param_accepts_escaping_capture_slot() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let callback_sig = SignatureType::new(vec![], ReturnMode::Value(int_ty));
    let callback_ty = builder.alloc_type(TypeData::Function(callback_sig.clone()));

    let accept = FunctionId::from_index(0);
    let mut accept_fb = FunctionBuilder::new("accept", module, FunctionKind::Normal, int_ty);
    accept_fb.push_param("f", callback_ty, ParamRole::Normal);
    accept_fb.set_param_escape(0, ParamEscape::Escaping);
    accept_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    }))));
    assert_eq!(builder.alloc_function(accept_fb.finish()), accept);

    let lambda = LambdaId::from_index(0);
    let body = FunctionId::from_index(1);
    let owner = FunctionId::from_index(2);
    let binding = BindingId::from_index(0);
    assert_eq!(
        builder.alloc_lambda(LambdaDecl {
            source: crate::ast::ExprId(0),
            module,
            body,
            owner,
            signature: callback_sig,
            escape: LambdaEscape::Escaping,
            captures: vec![LambdaCaptureDecl::ReadonlyLocal {
                binding,
                source: CaptureLocalSource {
                    owner,
                    local: LocalId::from_index(0),
                },
                ty: callback_ty,
            }],
        }),
        lambda
    );
    let mut body_fb = FunctionBuilder::new("lambda", module, FunctionKind::Lambda(lambda), int_ty);
    let bb0 = body_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(2),
    }))));
    body_fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Function(accept),
            args: vec![CallArg::Value(Operand::Place(Place {
                root: PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)),
                projection: vec![],
                ty: callback_ty,
            }))],
        }),
    );
    assert_eq!(builder.alloc_function(body_fb.finish()), body);

    let mut owner_fb = FunctionBuilder::new("owner", module, FunctionKind::Normal, int_ty);
    let param = owner_fb.push_param("f", callback_ty, ParamRole::Normal);
    owner_fb.set_param_escape(0, ParamEscape::Escaping);
    assert_eq!(param, LocalId::from_index(0));
    owner_fb.bind_local(param, binding);
    owner_fb.push_block(term_return(op_const(builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(3),
    }))));
    assert_eq!(builder.alloc_function(owner_fb.finish()), owner);
    builder.set_entry(owner);

    expect_verified(&builder.finish());
}

#[test]
fn raw_enum_to_raw_cast_is_valid() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let (_, enum_ty) = builder.raw_int_enum(module, "State", vec![("Idle", 0)]);

    let mut fb = FunctionBuilder::new("cast", module, FunctionKind::Normal, void_ty);
    let state = fb.push_param("state", enum_ty, ParamRole::Normal);
    let out = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_init(
            out,
            RValue::Cast {
                value: op_place(state, enum_ty),
                target: int_ty,
            },
        ),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    expect_verified(&builder.finish());
}

#[test]
fn tuple_ctor_shape_is_valid() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let bool_ty = builder.bool_ty();
    let void_ty = builder.void_ty();
    let tuple_ty = builder.alloc_type(TypeData::Tuple(vec![int_ty, bool_ty]));
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("tuple", module, FunctionKind::Normal, void_ty);
    let lhs = fb.push_param("lhs", int_ty, ParamRole::Normal);
    let rhs = fb.push_param("rhs", bool_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Aggregate {
            kind: AggregateCtor::Tuple,
            fields: vec![op_place(lhs, int_ty), op_place(rhs, bool_ty)],
            ty: tuple_ty,
        }),
    );
    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    expect_verified(&builder.finish());
}

#[test]
fn inline_extern_field_projection_is_valid() {
    let mut program = Program::default();
    let int_ty = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(empty_module("test"));
    let ext_id = crate::air::ExternTypeId::from_index(0);
    let ext_ty = program.alloc_type(TypeData::Extern(ext_id));
    assert_eq!(
        program.alloc_extern_type(ExternTypeDecl {
            name: Ident::new("Point"),
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
            fields: vec![ExternFieldDecl {
                name: Ident::new("x"),
                ty: int_ty,
                abi: anvyx_externs::ExternTypeExpr::Int,
                get_receiver: ExternReceiverDecl {
                    ty: ext_ty,
                    mode: ParamMode::SharedBorrow,
                },
                set_receiver: ExternReceiverDecl {
                    ty: ext_ty,
                    mode: ParamMode::MutBorrow,
                },
                computed: false,
                readable: true,
                writable: true,
            }],
            variants: vec![],
            variant_abis: vec![],
            methods: vec![],
            statics: vec![],
            operators: vec![],
        }),
        ext_id
    );
    program.module_mut(module).extern_types.push(ext_id);

    let mut fb = FunctionBuilder::new("extern_field", module, FunctionKind::Normal, int_ty);
    let point = fb.push_param("point", ext_ty, ParamRole::Normal);
    fb.push_block(term_return(Operand::Place(Place {
        root: PlaceRoot::Local(point),
        projection: vec![Projection::Field(FieldId::from_index(0))],
        ty: int_ty,
    })));
    let fid = program.alloc_function(fb.finish());
    program.module_mut(module).functions.push(fid);
    program.set_entry(fid);

    expect_verified(&program);
}

#[test]
fn fn_return() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("ret42", module, FunctionKind::Normal, int_ty);
    let c42 = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(42),
    });
    fb.push_block(term_return(op_const(c42)));
    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    let program = builder.finish();
    expect_verified(&program);
    assert_eq!(program.entry(), Some(func_id));
}

#[test]
fn structured_straight_line_scalar_return() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let value = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("structured", module, FunctionKind::Normal, int_ty);
    let local = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_return(op_const(value)));
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::Init {
                local,
                value: RValue::Use(op_const(value)),
            }],
            tail: AirTail::Return(Some(op_place(local, int_ty))),
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_if_return_then_fallthrough() {
    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.bool_ty();
    let module = test_module(&mut builder);
    let yes = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let no = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(false),
    });
    let mut fb = FunctionBuilder::new("branch", module, FunctionKind::Normal, bool_ty);
    let cond = fb.push_param("cond", bool_ty, ParamRole::Normal);
    fb.push_block(term_return(op_const(no)));
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::If(AirIf {
                cond: op_place(cond, bool_ty),
                then_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(Some(op_const(yes))),
                },
                else_block: None,
            })],
            tail: AirTail::Return(Some(op_const(no))),
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_both_if_branches_return() {
    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.bool_ty();
    let module = test_module(&mut builder);
    let yes = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let no = builder.alloc_const(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(false),
    });
    let mut fb = FunctionBuilder::new("both_return", module, FunctionKind::Normal, bool_ty);
    let cond = fb.push_param("cond", bool_ty, ParamRole::Normal);
    fb.push_block(term_return(op_const(no)));
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::If(AirIf {
                cond: op_place(cond, bool_ty),
                then_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(Some(op_const(yes))),
                },
                else_block: Some(AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(Some(op_const(no))),
                }),
            })],
            tail: AirTail::Unreachable,
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_loop_break_state_reaches_after_loop() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("loop_break", module, FunctionKind::Normal, int_ty);
    let out = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_return(op_const(one)));
    let func_id = builder.alloc_function(fb.finish());
    let loop_id = AirLoopId::from_index(0);
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::Loop(AirLoop {
                id: loop_id,
                body: AirBlock {
                    stmts: vec![AirStmt::Init {
                        local: out,
                        value: RValue::Use(op_const(one)),
                    }],
                    tail: AirTail::Break(loop_id),
                },
            })],
            tail: AirTail::Return(Some(op_place(out, int_ty))),
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_branch_result_initialized_in_both_arms() {
    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.bool_ty();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let two = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(2),
    });
    let mut fb = FunctionBuilder::new("branch_value", module, FunctionKind::Normal, int_ty);
    let cond = fb.push_param("cond", bool_ty, ParamRole::Normal);
    let out = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    fb.push_block(term_return(op_const(one)));
    let func_id = builder.alloc_function(fb.finish());
    let init_arm = |value| AirBlock {
        stmts: vec![AirStmt::Init {
            local: out,
            value: RValue::Use(op_const(value)),
        }],
        tail: AirTail::None,
    };
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::If(AirIf {
                cond: op_place(cond, bool_ty),
                then_block: init_arm(one),
                else_block: Some(init_arm(two)),
            })],
            tail: AirTail::Return(Some(op_place(out, int_ty))),
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn init_and_assign_mutable_local() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);
    let c1 = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let c2 = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(2),
    });

    let mut fb = FunctionBuilder::new("mutate", module, FunctionKind::Normal, int_ty);
    let local = fb.push_local(None, int_ty, Mutability::Mutable, LocalKind::User);
    let block = fb.push_block(term_return(op_place(local, int_ty)));
    fb.add_statement(block, stmt_init(local, RValue::Use(op_const(c1))));
    fb.add_statement(
        block,
        stmt_assign(place(local, int_ty), RValue::Use(op_const(c2))),
    );
    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    expect_verified(&builder.finish());
}

#[test]
fn primitive_rvalues() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let float_ty = builder.float_ty();
    let bool_ty = builder.bool_ty();
    builder.string_ty();
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("primitive_rvalues", module, FunctionKind::Normal, int_ty);
    let int = fb.push_param("i", int_ty, ParamRole::Normal);
    let float = fb.push_param("f", float_ty, ParamRole::Normal);
    let bool_ = fb.push_param("b", bool_ty, ParamRole::Normal);
    let block = fb.push_block(term_return(op_place(int, int_ty)));
    for value in [
        RValue::Unary {
            op: crate::ast::UnaryOp::Neg,
            value: op_place(int, int_ty),
            ty: int_ty,
        },
        RValue::Unary {
            op: crate::ast::UnaryOp::Not,
            value: op_place(bool_, bool_ty),
            ty: bool_ty,
        },
        RValue::Binary {
            op: crate::ast::BinaryOp::Add,
            lhs: op_place(int, int_ty),
            rhs: op_place(int, int_ty),
            ty: int_ty,
        },
        RValue::Binary {
            op: crate::ast::BinaryOp::LessThan,
            lhs: op_place(float, float_ty),
            rhs: op_place(float, float_ty),
            ty: bool_ty,
        },
        RValue::Cast {
            value: op_place(int, int_ty),
            target: float_ty,
        },
        RValue::Cast {
            value: op_place(float, float_ty),
            target: int_ty,
        },
        RValue::Stringify {
            value: op_place(bool_, bool_ty),
            source_ty: bool_ty,
        },
    ] {
        fb.add_statement(block, stmt_eval(value));
    }
    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    expect_verified(&builder.finish());
}

#[test]
fn fn_params() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("add", module, FunctionKind::Normal, int_ty);
    let p_a = fb.push_param("a", int_ty, ParamRole::Normal);
    fb.push_param("b", int_ty, ParamRole::Normal);
    fb.push_block(term_return(op_place(p_a, int_ty)));
    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    expect_verified(&builder.finish());
}

#[test]
fn fn_aggregate() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let agg_id = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Point"),
        module,
        kind: AggregateKind::Struct,
        type_args: vec![],
        const_args: vec![],
        fields: vec![
            FieldDecl {
                name: Ident::new("x"),
                ty: int_ty,
            },
            FieldDecl {
                name: Ident::new("y"),
                ty: int_ty,
            },
        ],
        cycle_capable: false,
        stringify_override: None,
    });
    let agg_ty = builder.alloc_type(TypeData::Aggregate(agg_id));

    let mut fb = FunctionBuilder::new("mk_point", module, FunctionKind::Normal, void_ty);
    let p_x = fb.push_param("x", int_ty, ParamRole::Normal);
    let p_y = fb.push_param("y", int_ty, ParamRole::Normal);
    let local_agg = fb.push_local(None, agg_ty, Mutability::Immutable, LocalKind::User);

    let x_proj = Place {
        root: PlaceRoot::Local(local_agg),
        projection: vec![Projection::Field(FieldId::from_index(0))],
        ty: int_ty,
    };
    let y_proj = Place {
        root: PlaceRoot::Local(local_agg),
        projection: vec![Projection::Field(FieldId::from_index(1))],
        ty: int_ty,
    };

    let block0 = fb.push_block(term_return_void());
    fb.add_statement(
        block0,
        stmt_init(
            local_agg,
            RValue::Aggregate {
                kind: AggregateCtor::Struct(agg_id),
                fields: vec![op_place(p_x, int_ty), op_place(p_y, int_ty)],
                ty: agg_ty,
            },
        ),
    );
    fb.add_statement(block0, stmt_eval(RValue::Use(Operand::Place(x_proj))));
    fb.add_statement(block0, stmt_eval(RValue::Use(Operand::Place(y_proj))));
    fb.add_statement(block0, stmt_eval(RValue::Use(op_place(local_agg, agg_ty))));

    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    expect_verified(&builder.finish());
}

#[test]
fn function_call() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let module = test_module(&mut builder);

    let mut callee = FunctionBuilder::new("id", module, FunctionKind::Normal, int_ty);
    let p_value = callee.push_param("value", int_ty, ParamRole::Normal);
    callee.push_block(term_return(op_place(p_value, int_ty)));
    let callee_id = builder.alloc_function(callee.finish());

    let mut caller = FunctionBuilder::new("call_id", module, FunctionKind::Normal, int_ty);
    let p_arg = caller.push_param("arg", int_ty, ParamRole::Normal);
    let result = caller.push_local(
        Some("result"),
        int_ty,
        Mutability::Immutable,
        LocalKind::User,
    );
    let bb0 = caller.push_block(term_return(op_place(result, int_ty)));
    caller.add_statement(
        bb0,
        stmt_init(
            result,
            RValue::Call {
                callee: Callee::Function(callee_id),
                args: vec![CallArg::Value(op_place(p_arg, int_ty))],
            },
        ),
    );
    let caller_id = builder.alloc_function(caller.finish());
    builder.set_entry(caller_id);

    expect_verified(&builder.finish());
}

#[test]
fn extern_call() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    builder.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Console"),
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

    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("log"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: int_ty,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: void_ty,
        abi: crate::air::ExternAbi {
            params: vec![anvyx_externs::ExternTypeExpr::Int],
            ret: anvyx_externs::ExternTypeExpr::Void,
        },
        binding: None,
        effects: anvyx_externs::ExternEffects::default(),
    });

    let mut fb = FunctionBuilder::new("call_ext", module, FunctionKind::Normal, void_ty);
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
    expect_verified(&builder.finish());
}

#[test]
fn if_bool() {
    let mut builder = ProgramBuilder::default();
    let bool_ty = builder.bool_ty();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("branch", module, FunctionKind::Normal, void_ty);
    let p_cond = fb.push_param("cond", bool_ty, ParamRole::Normal);
    let block = fb.push_block(term_return_void());
    fb.add_statement(
        block,
        AirStmt::If(AirIf {
            cond: op_place(p_cond, bool_ty),
            then_block: AirBlock {
                stmts: vec![],
                tail: term_return_void(),
            },
            else_block: Some(AirBlock {
                stmts: vec![],
                tail: term_return_void(),
            }),
        }),
    );

    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    expect_verified(&builder.finish());
}

#[test]
fn unreachable_fn() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("unreachable_fn", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_unreachable());

    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);
    expect_verified(&builder.finish());
}

#[test]
fn multi_module() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();

    let m0 = builder.alloc_module(empty_module("a"));
    let m1 = builder.alloc_module(empty_module("b"));

    let mut fb0 = FunctionBuilder::new("fn_a", m0, FunctionKind::Normal, void_ty);
    fb0.push_block(term_return_void());
    let f0 = builder.alloc_function(fb0.finish());

    let mut fb1 = FunctionBuilder::new("fn_b", m1, FunctionKind::Normal, void_ty);
    fb1.push_block(term_return_void());
    let f1 = builder.alloc_function(fb1.finish());

    builder.set_entry(f0);

    let program = builder.finish();
    expect_verified(&program);
    assert_eq!(program.module(m0).path[0].as_str(), "a");
    assert_eq!(program.module(m1).path[0].as_str(), "b");
    assert_eq!(program.module(m1).functions[0], f1);
}

#[test]
fn structured_optional_match_with_payload() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("optional_match", module, FunctionKind::Normal, int_ty);
    let opt = fb.push_param("opt", opt_ty, ParamRole::Normal);
    let payload = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let none = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    fb.push_block(AirTail::Return(Some(op_place(payload, int_ty))));
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::OptionalMatch(AirOptionalMatch {
                discr: place(opt, opt_ty),
                payload: Some(payload),
                payload_ref: false,
                payload_escapes: false,
                some_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::None,
                },
                none_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(Some(op_const(none))),
                },
            })],
            tail: AirTail::Return(Some(op_place(payload, int_ty))),
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_optional_match_without_payload() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let opt_ty = builder.alloc_type(TypeData::Optional(int_ty));
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let zero = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(0),
    });
    let mut fb = FunctionBuilder::new("optional_match", module, FunctionKind::Normal, int_ty);
    let opt = fb.push_param("opt", opt_ty, ParamRole::Normal);
    fb.push_block(AirTail::Unreachable);
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::OptionalMatch(AirOptionalMatch {
                discr: place(opt, opt_ty),
                payload: None,
                payload_ref: false,
                payload_escapes: false,
                some_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(Some(op_const(one))),
                },
                none_block: AirBlock {
                    stmts: vec![],
                    tail: AirTail::Return(Some(op_const(zero))),
                },
            })],
            tail: AirTail::Unreachable,
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_collection_loan_with_loop_verifies() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let mut fb = FunctionBuilder::new("loan_loop", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param("xs", list_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let loop_id = AirLoopId::from_index(0);
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(xs, list_ty),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::ReadonlySequence,
                body: AirBlock {
                    stmts: vec![AirStmt::Loop(AirLoop {
                        id: loop_id,
                        body: AirBlock {
                            stmts: vec![],
                            tail: AirTail::Break(loop_id),
                        },
                    })],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn slice_view_requires_sequence_source_and_slice_result() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let slice_ty = builder.alloc_type(TypeData::Slice(int_ty));
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
    let mut fb = FunctionBuilder::new("slice_view", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param("xs", list_ty, ParamRole::Normal);
    let start = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let end = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
    let out = fb.push_local(None, slice_ty, Mutability::Immutable, LocalKind::Temp);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(bb0, stmt_init(start, RValue::Use(op_const(zero))));
    fb.add_statement(bb0, stmt_init(end, RValue::Use(op_const(one))));
    fb.add_statement(
        bb0,
        stmt_init(
            out,
            RValue::SliceView {
                source: place(xs, list_ty),
                start,
                end,
                inclusive: false,
                ty: slice_ty,
            },
        ),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    expect_verified(&builder.finish());
}

#[test]
fn structured_mutable_sequence_collection_slot_verifies() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let list_ty = builder.alloc_type(TypeData::List(int_ty));
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("loan_slot", module, FunctionKind::Normal, void_ty);
    let xs = fb.push_param_with_mode("xs", list_ty, ParamMode::MutBorrow, ParamRole::Normal);
    let index = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);
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
                        stmt_init(index, RValue::Use(op_const(one))),
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
                                stmts: vec![
                                    stmt_eval(RValue::Use(op_place(slot, int_ty))),
                                    stmt_assign(place(slot, int_ty), RValue::Use(op_const(one))),
                                ],
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

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_map_value_update_inside_loan_verifies() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.int_ty();
    let map_ty = builder.alloc_type(TypeData::Map {
        key: int_ty,
        value: int_ty,
        order: MapOrder::Insertion,
    });
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let one = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(1),
    });
    let mut fb = FunctionBuilder::new("map_loan", module, FunctionKind::Normal, void_ty);
    let map = fb.push_param_with_mode("map", map_ty, ParamMode::MutBorrow, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::CollectionLoan(AirCollectionLoan {
                root: place(map, map_ty),
                root_kind: AirCollectionRootKind::Map,
                mode: AirCollectionLoanMode::ReadonlyMap,
                body: AirBlock {
                    stmts: vec![stmt_eval(RValue::MapInsert {
                        map: place(map, map_ty),
                        key: op_const(one),
                        value: op_const(one),
                        kind: MapWriteKind::IndexedAssignment,
                    })],
                    tail: AirTail::None,
                },
            })],
            tail: AirTail::Return(None),
        },
    };
    let program = builder.finish();

    verify_structured_body(&program, func_id, &body).unwrap();
}

#[test]
fn structured_nonescaping_lambda_can_capture_collection_slot() {
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
            escape: LambdaEscape::NonEscaping,
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

    verify_structured_body(&program, func_id, &body).unwrap();
}
