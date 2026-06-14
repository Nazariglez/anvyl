use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, BindingId, CallArg, Callee, CaptureCellDecl, CaptureLocalSource, ExternDecl,
        ExternMember, ExternParamDecl, ExternReceiverDecl, ExternRep, ExternTypeDecl, Function,
        FunctionId, FunctionKind, LambdaDecl, LambdaEscape, Local, LocalKind, Mutability, Operand,
        ParamEscape, ParamMode, Place, PlaceRoot, Program, RValue, Signature, TypeData,
    },
    ast::{ExprId, Ident},
};

use super::{
    compile::{VmCompileError, VmCompileErrorKind, VmCompileErrorSite, VmCompiler},
    runtime::{ExternDispatcher, NoExterns, unsupported_callback},
};
use crate::test_support::{global_with_init, local, param, place, root_module, structured_body};

#[test]
fn compiler_lowers_value_and_shared_borrow_modes() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let value_local = air::LocalId::from_index(0);
    let shared_local = air::LocalId::from_index(1);
    let callee = program.alloc_function(Function {
        name: Ident::new("callee"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![
                param("value", int, ParamMode::Value, value_local),
                param("shared", string, ParamMode::SharedBorrow, shared_local),
            ],
            void,
        ),
        locals: vec![
            local(int, Mutability::Immutable, LocalKind::Arg),
            local(string, Mutability::Immutable, LocalKind::Arg),
        ],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let one = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(1),
    });
    let text = program.alloc_const(air::ConstData {
        ty: string,
        value: air::ConstValue::String("x".into()),
    });
    let caller_string = air::LocalId::from_index(0);
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(string, Mutability::Immutable, LocalKind::User)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: caller_string,
                    value: RValue::Use(Operand::Const(text)),
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(callee),
                    args: vec![
                        CallArg::Value(Operand::Const(one)),
                        CallArg::SharedBorrow(place(caller_string, string)),
                    ],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program
        .module_mut(module)
        .functions
        .extend([callee, caller]);

    let verified = air::verify(&program).expect("AIR verify failed");
    let vir = VmCompiler::compile(verified).expect("VM compile failed");
    assert_eq!(vir.functions[0].params[0].param.mode, ParamMode::Value);
    assert_eq!(
        vir.functions[0].params[1].param.mode,
        ParamMode::SharedBorrow
    );
    assert!(matches!(
        vir.functions[1].calls[0].args[1],
        CallArg::SharedBorrow(_)
    ));
}

#[test]
fn compiler_lowers_mut_borrow_as_projected_place_ref() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let pair = program.alloc_type(TypeData::Tuple(vec![int, int]));
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let arg_local = air::LocalId::from_index(0);
    let callee = program.alloc_function(Function {
        name: Ident::new("callee"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("mut", int, ParamMode::MutBorrow, arg_local)],
            void,
        ),
        locals: vec![local(int, Mutability::Mutable, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let zero = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(0),
    });
    let caller_local = air::LocalId::from_index(0);
    let mut projected = place(caller_local, int);
    projected.projection.push(air::Projection::TupleField(0));
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(pair, Mutability::Mutable, LocalKind::User)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: caller_local,
                    value: RValue::Aggregate {
                        kind: air::AggregateCtor::Tuple,
                        fields: vec![Operand::Const(zero), Operand::Const(zero)],
                        ty: pair,
                    },
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(callee),
                    args: vec![CallArg::MutBorrow(projected.clone())],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program
        .module_mut(module)
        .functions
        .extend([callee, caller]);

    let verified = air::verify(&program).expect("AIR verify failed");
    let vir = VmCompiler::compile(verified).expect("VM compile failed");
    assert_eq!(vir.functions[0].params[0].param.mode, ParamMode::MutBorrow);
    assert_eq!(vir.functions[1].calls[0].callee, Callee::Function(callee));
    assert_eq!(
        vir.functions[1].calls[0].args[0],
        CallArg::MutBorrow(projected)
    );
}

#[test]
fn compiler_collects_calls_inside_structured_control() {
    let mut program = Program::default();
    let bool_ty = program.alloc_type(TypeData::Bool);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee = program.alloc_function(Function {
        name: Ident::new("callee"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let cond = program.alloc_const(air::ConstData {
        ty: bool_ty,
        value: air::ConstValue::Bool(true),
    });
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: air::AirBody {
            block: air::AirBlock {
                stmts: vec![Statement::If(air::AirIf {
                    cond: Operand::Const(cond),
                    then_block: air::AirBlock {
                        stmts: vec![Statement::Eval(RValue::Call {
                            callee: Callee::Function(callee),
                            args: vec![],
                        })],
                        tail: air::AirTail::None,
                    },
                    else_block: None,
                })],
                tail: air::AirTail::Return(None),
            },
        },
    });
    program
        .module_mut(module)
        .functions
        .extend([callee, caller]);

    let verified = air::verify(&program).expect("AIR verify failed");
    let vir = VmCompiler::compile(verified).expect("VM compile failed");
    assert_eq!(vir.functions[1].calls.len(), 1);
    assert_eq!(vir.functions[1].calls[0].callee, Callee::Function(callee));
}

#[test]
fn compiler_records_extern_metadata_from_call_params() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let ext = program.alloc_extern(ExternDecl {
        name: Ident::new("touch"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![ExternParamDecl {
            ty: int,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: void,
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    let konst = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(1),
    });
    program.module_mut(module).externs.push(ext);
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Extern(ext),
                args: vec![CallArg::Value(Operand::Const(konst))],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(caller);

    let verified = air::verify(&program).expect("AIR verify failed");
    let vir = VmCompiler::compile(verified).expect("VM compile failed");

    assert_eq!(vir.externs[0].source, ext);
    assert_eq!(vir.externs[0].params[0].ty, int);
    assert_eq!(vir.externs[0].params[0].mode, ParamMode::Value);
    assert_eq!(vir.externs[0].ret, void);
    assert_eq!(vir.functions[0].calls[0].callee, Callee::Extern(ext));
}

#[test]
fn compiler_records_member_extern_receiver_metadata() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let owner = program.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Thing"),
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
    let owner_ty = program.alloc_type(TypeData::Extern(owner));
    let ext = program.alloc_extern(ExternDecl {
        name: Ident::new("touch"),
        module,
        member: ExternMember::Method {
            owner,
            receiver: ExternReceiverDecl {
                ty: owner_ty,
                mode: ParamMode::SharedBorrow,
            },
        },
        params: vec![ExternParamDecl {
            ty: int,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        return_type: void,
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    program.module_mut(module).externs.push(ext);

    let verified = air::verify(&program).expect("AIR verify failed");
    let vir = VmCompiler::compile(verified).expect("VM compile failed");

    assert_eq!(vir.externs[0].params.len(), 2);
    assert_eq!(vir.externs[0].params[0].ty, owner_ty);
    assert_eq!(vir.externs[0].params[0].mode, ParamMode::SharedBorrow);
    assert_eq!(vir.externs[0].params[1].ty, int);
    assert_eq!(vir.externs[0].params[1].mode, ParamMode::Value);
}

#[test]
fn no_extern_dispatch_reports_missing_id() {
    let mut dispatcher = NoExterns;
    let id = air::ExternId::from_index(7);
    let err = anvyx_runtime::Heap::scope(|heap| {
        let mut ctx = anvyx_runtime::Ctx::new(heap);
        dispatcher.call(&mut ctx, id, &[])
    })
    .unwrap_err();

    assert_eq!(err.message(), "missing VM extern dispatch for extern #7");
}

#[test]
fn callback_dispatch_reports_typed_runtime_gap() {
    let err = unsupported_callback(air::ExternId::from_index(3));

    assert_eq!(err.message(), "VM callback extern #3 is not supported");
}

#[test]
fn compiler_rejects_noncheap_value_params() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let local_id = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("callee"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("value", string, ParamMode::Value, local_id)],
            void,
        ),
        locals: vec![local(string, Mutability::Immutable, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        func,
        VmCompileErrorKind::NonCheapValueParam
    ));
}

#[test]
fn compiler_rejects_lambda_types() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let function_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let local_id = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("takes_lambda"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("f", function_ty, ParamMode::Value, local_id)],
            void,
        ),
        locals: vec![local(function_ty, Mutability::Immutable, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        func,
        VmCompileErrorKind::UnsupportedLambdaType
    ));
}

#[test]
fn compiler_rejects_readonly_lambda_values_and_captures() {
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
    let one = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(1),
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
            mutability: Mutability::Immutable,
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
                    captures: vec![air::LambdaCaptureArg::ReadonlyLocal {
                        value: Operand::Place(place(captured, int)),
                    }],
                    ty: lambda_ty,
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    debug_assert_eq!(func, owner);
    program.module_mut(module).functions.extend([body, func]);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        func,
        VmCompileErrorKind::UnsupportedLambdaValue
    ));
    assert!(has_compile_error(
        &errors,
        func,
        VmCompileErrorKind::UnsupportedLambdaCapture
    ));
}

#[test]
fn compiler_rejects_lambda_calls() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let function_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let local_id = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("f", function_ty, ParamMode::Value, local_id)],
            void,
        ),
        locals: vec![local(function_ty, Mutability::Immutable, LocalKind::Arg)],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Lambda(Operand::Place(place(local_id, function_ty))),
                args: vec![],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(func);

    let errors = compile_errors(&program);
    assert_eq!(
        errors
            .iter()
            .filter(|error| error.site == VmCompileErrorSite::Function(func)
                && error.kind == VmCompileErrorKind::UnsupportedLambdaCall)
            .count(),
        1
    );
}

#[test]
fn compiler_rejects_global_declarations_and_initializers() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", int, Mutability::Immutable);
    let init = program.globals[global.index()].init;

    let errors = compile_errors(&program);
    assert!(errors.iter().any(|error| {
        error.site == VmCompileErrorSite::Global(global)
            && error.kind == VmCompileErrorKind::UnsupportedGlobal
    }));
    assert!(has_compile_error(
        &errors,
        init,
        VmCompileErrorKind::UnsupportedGlobal
    ));
}

#[test]
fn compiler_rejects_global_roots() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", int, Mutability::Immutable);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
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
    });
    program.module_mut(module).functions.push(func);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        func,
        VmCompileErrorKind::UnsupportedGlobal
    ));
}

#[test]
fn compiler_rejects_global_statements() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let value = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(1),
    });
    let module = program.alloc_module(root_module());
    let global = global_with_init(&mut program, module, "g", int, Mutability::Mutable);
    let func = program.alloc_function(Function {
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
            air::AirTail::Return(Some(Operand::Const(value))),
        ),
    });
    program.module_mut(module).functions.push(func);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        func,
        VmCompileErrorKind::UnsupportedGlobal
    ));
    assert!(
        errors
            .iter()
            .filter(|error| error.site == VmCompileErrorSite::Function(func)
                && error.kind == VmCompileErrorKind::UnsupportedGlobal)
            .count()
            >= 2
    );
}

#[test]
fn compiler_rejects_capture_cell_roots() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let module = program.alloc_module(root_module());
    let cell = program.alloc_capture_cell(CaptureCellDecl {
        binding: BindingId::from_index(0),
        owner: FunctionId::from_index(0),
        source_local: air::LocalId::from_index(0),
        ty: int,
    });
    let init = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(0),
    });
    let mut source = local(int, Mutability::Mutable, LocalKind::User);
    source.binding = Some(BindingId::from_index(0));
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
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
    });
    program.module_mut(module).functions.push(func);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        func,
        VmCompileErrorKind::UnsupportedLambdaCell
    ));
}

#[test]
fn compiler_rejects_mutable_upvalue_lambda_cells() {
    for shared in [false, true] {
        let errors = compile_errors(&mutable_upvalue_lambda_program(shared));
        assert_function_errors(
            &errors,
            FunctionId::from_index(1),
            &[
                VmCompileErrorKind::UnsupportedLambdaValue,
                VmCompileErrorKind::UnsupportedLambdaCell,
            ],
        );
    }
}

#[test]
fn compiler_rejects_lambda_body_capture_cell_access() {
    let errors = compile_errors(&mutable_upvalue_lambda_program(false));
    assert_function_errors(
        &errors,
        FunctionId::from_index(0),
        &[
            VmCompileErrorKind::UnsupportedLambdaValue,
            VmCompileErrorKind::UnsupportedLambdaCell,
        ],
    );
}

#[test]
fn vm_rejects_native_lambda_extern_boundaries() {
    for (name, params, returns_lambda) in [
        ("non_escaping", vec![ParamEscape::NonEscaping], false),
        ("escaping", vec![ParamEscape::Escaping], false),
        ("returns_lambda", vec![], true),
    ] {
        let (program, ext) = lambda_extern_boundary_program(name, params, returns_lambda);
        let errors = compile_errors(&program);
        assert!(
            has_extern_compile_error(
                &errors,
                ext,
                VmCompileErrorKind::UnsupportedLambdaExternBoundary
            ),
            "missing VM native lambda boundary gap for {name}"
        );
    }
}

#[test]
fn compiler_rejects_noncheap_value_call_args() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let callee_local = air::LocalId::from_index(0);
    let callee = program.alloc_function(Function {
        name: Ident::new("callee"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param("value", string, ParamMode::Value, callee_local)],
            void,
        ),
        locals: vec![local(string, Mutability::Immutable, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let text = program.alloc_const(air::ConstData {
        ty: string,
        value: air::ConstValue::String("x".into()),
    });
    let caller_local = air::LocalId::from_index(0);
    let caller = program.alloc_function(Function {
        name: Ident::new("caller"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(string, Mutability::Immutable, LocalKind::User)],
        body: structured_body(
            vec![
                Statement::Init {
                    local: caller_local,
                    value: RValue::Use(Operand::Const(text)),
                },
                Statement::Eval(RValue::Call {
                    callee: Callee::Function(callee),
                    args: vec![CallArg::Value(Operand::Place(place(caller_local, string)))],
                }),
            ],
            air::AirTail::Return(None),
        ),
    });
    program
        .module_mut(module)
        .functions
        .extend([callee, caller]);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        caller,
        VmCompileErrorKind::NonCheapValueArg
    ));
}

fn mutable_upvalue_lambda_program(shared: bool) -> Program {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let int = program.alloc_type(TypeData::Int);
    let function_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source = air::LocalId::from_index(0);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);
    let cell = program.alloc_capture_cell(CaptureCellDecl {
        binding,
        owner,
        source_local: source,
        ty: int,
    });
    let lambda = program.alloc_lambda(LambdaDecl {
        source: ExprId(0),
        module,
        owner,
        body,
        signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
        escape: LambdaEscape::Escaping,
        captures: vec![air::LambdaCaptureDecl::CaptureCell {
            binding,
            cell,
            ty: int,
        }],
    });
    let lambda_body = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(
            vec![Statement::Eval(RValue::Use(Operand::Place(Place {
                root: PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                projection: vec![],
                ty: int,
            })))],
            air::AirTail::Return(None),
        ),
    });
    debug_assert_eq!(lambda_body, body);
    let zero = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(0),
    });
    let first = air::LocalId::from_index(1);
    let mut stmts = vec![
        Statement::Assign {
            dst: Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int,
            },
            value: RValue::Use(Operand::Const(zero)),
        },
        Statement::Init {
            local: first,
            value: RValue::MakeLambda {
                lambda,
                captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                ty: function_ty,
            },
        },
    ];
    let mut locals = vec![
        Local {
            name: None,
            binding: Some(binding),
            ty: int,
            mutability: Mutability::Mutable,
            kind: LocalKind::User,
        },
        local(function_ty, Mutability::Immutable, LocalKind::User),
    ];
    if shared {
        let second = air::LocalId::from_index(2);
        locals.push(local(function_ty, Mutability::Immutable, LocalKind::User));
        stmts.push(Statement::Init {
            local: second,
            value: RValue::MakeLambda {
                lambda,
                captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                ty: function_ty,
            },
        });
    }
    let owner_fn = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals,
        body: structured_body(stmts, air::AirTail::Return(None)),
    });
    debug_assert_eq!(owner_fn, owner);
    program
        .module_mut(module)
        .functions
        .extend([lambda_body, owner_fn]);
    program
}

fn lambda_extern_boundary_program(
    name: &str,
    escapes: Vec<ParamEscape>,
    returns_lambda: bool,
) -> (Program, air::ExternId) {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let function_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let params = escapes
        .into_iter()
        .map(|escape| ExternParamDecl {
            ty: function_ty,
            mode: ParamMode::Value,
            escape,
        })
        .collect();
    let ext = program.alloc_extern(ExternDecl {
        name: Ident::new(name),
        module,
        member: ExternMember::FreeFunction,
        params,
        return_type: if returns_lambda { function_ty } else { void },
        binding: None,
        effects: anvyx_runtime::ExternEffects::default(),
    });
    program.module_mut(module).externs.push(ext);
    (program, ext)
}

fn compile_errors(program: &Program) -> Vec<VmCompileError> {
    let verified = air::verify(program).expect("AIR verify failed");
    VmCompiler::compile(verified).expect_err("VM compile should fail")
}

fn has_compile_error(
    errors: &[VmCompileError],
    function: FunctionId,
    kind: VmCompileErrorKind,
) -> bool {
    errors
        .iter()
        .any(|error| error.site == VmCompileErrorSite::Function(function) && error.kind == kind)
}

fn assert_function_errors(
    errors: &[VmCompileError],
    function: FunctionId,
    kinds: &[VmCompileErrorKind],
) {
    for kind in kinds {
        assert!(has_compile_error(errors, function, *kind));
    }
}

fn has_extern_compile_error(
    errors: &[VmCompileError],
    ext: air::ExternId,
    kind: VmCompileErrorKind,
) -> bool {
    errors
        .iter()
        .any(|error| error.site == VmCompileErrorSite::Extern(ext) && error.kind == kind)
}
