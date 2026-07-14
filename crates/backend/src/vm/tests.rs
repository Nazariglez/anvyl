use std::{collections::HashMap, convert::Infallible};

use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, AggregateDecl, AggregateKind, AirCollectionLoan, AirCollectionLoanMode,
        AirCollectionRootKind, AirCollectionSlotScope, AirMapEntryMatch, BindingId, CallArg,
        Callee, CaptureCellDecl, CaptureCellLifetime, CaptureLocalSource, EnumDecl, EnumRepr,
        ExternDecl, ExternFieldDecl, ExternMember, ExternParamDecl, ExternReceiverDecl, ExternRep,
        ExternTypeDecl, FieldDecl, Function, FunctionId, FunctionKind, LambdaDecl, LambdaEscape,
        Local, LocalKind, Mutability, Operand, ParamEscape, ParamMode, Place, PlaceRoot, Program,
        Projection, RValue, ScopedBorrowDecl, ScopedBorrowSource, Signature, TypeData, TypeId,
        VariantDecl, VariantShape,
    },
    ast::{ExprId, Ident},
    pipeline::{
        AirBuildError, AirRootConfig, FrontendConfig, PackageModuleInput, PackageProgramInput,
        PackageSourceInput, PackageSourceLoader, Source, SourceLoadError, build_air_packages,
    },
    resolve::{ModuleId, PackageId, SystemPackages},
};

use super::{
    compile::{VmCompileError, VmCompileErrorKind, VmCompileErrorSite, VmCompiler},
    runtime::{ExternDispatcher, NoExterns, unsupported_callback},
    vir::{VirCallArg, VirCallTarget},
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
    assert_eq!(
        vir.functions[1].calls[0].args[0],
        VirCallArg::Value(Operand::Const(one))
    );
    assert!(matches!(
        vir.functions[1].calls[0].args[1],
        VirCallArg::SharedBorrow(_)
    ));
}

#[test]
fn compiler_lowers_shared_string_const_args() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let shared_local = air::LocalId::from_index(0);
    let callee = program.alloc_function(Function {
        name: Ident::new("callee"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![param(
                "shared",
                string,
                ParamMode::SharedBorrow,
                shared_local,
            )],
            void,
        ),
        locals: vec![local(string, Mutability::Immutable, LocalKind::Arg)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let text = program.alloc_const(air::ConstData {
        ty: string,
        value: air::ConstValue::String("x".into()),
    });
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
                callee: Callee::Function(callee),
                args: vec![CallArg::SharedStringConst(text)],
            })],
            air::AirTail::Return(None),
        ),
    });
    program
        .module_mut(module)
        .functions
        .extend([callee, caller]);

    let verified = air::verify(&program).expect("AIR verify failed");
    let vir = VmCompiler::compile(verified).expect("VM compile failed");
    assert_eq!(
        vir.functions[1].calls[0].target,
        VirCallTarget::Function(callee)
    );
    assert_eq!(
        vir.functions[1].calls[0].args[0],
        VirCallArg::SharedStringConst(text)
    );
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
    projected.projection.push(Projection::TupleField(0));
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
    assert_eq!(
        vir.functions[1].calls[0].target,
        VirCallTarget::Function(callee)
    );
    assert_eq!(
        vir.functions[1].calls[0].args[0],
        VirCallArg::MutBorrow(projected)
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
    assert_eq!(
        vir.functions[1].calls[0].target,
        VirCallTarget::Function(callee)
    );
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
        abi: air::ExternAbi {
            params: vec![anvyx_runtime::ExternTypeExpr::Int],
            ret: anvyx_runtime::ExternTypeExpr::Void,
        },
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
    assert_eq!(vir.functions[0].calls[0].target, VirCallTarget::Extern(ext));
}

#[test]
fn compiler_rejects_native_init_field_args() {
    fn program_with_arg(provided: bool) -> (Program, FunctionId) {
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
            layout: None,
            materialization: None,
            owns_heap_edges: None,
            has_init: true,
            init_args: vec![air::ExternInitArgDecl {
                field: air::FieldId::from_index(0),
                param: 0,
                presence: true,
            }],
            fields: vec![ExternFieldDecl {
                name: Ident::new("value"),
                ty: int,
                abi: anvyx_runtime::ExternTypeExpr::Int,
                get_receiver: ExternReceiverDecl {
                    ty: TypeId::from_index(0),
                    mode: ParamMode::SharedBorrow,
                },
                set_receiver: ExternReceiverDecl {
                    ty: TypeId::from_index(0),
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
        });
        program.module_mut(module).extern_types.push(owner);
        let owner_ty = program.alloc_type(TypeData::Extern(owner));
        let extern_ty = program.extern_type_mut(owner);
        extern_ty.fields[0].get_receiver.ty = owner_ty;
        extern_ty.fields[0].set_receiver.ty = owner_ty;
        let init = program.alloc_extern(ExternDecl {
            name: Ident::new("new"),
            module,
            member: ExternMember::Init { owner },
            params: vec![ExternParamDecl {
                ty: int,
                mode: ParamMode::Value,
                escape: ParamEscape::NonEscaping,
            }],
            return_type: owner_ty,
            abi: air::ExternAbi {
                params: vec![anvyx_runtime::ExternTypeExpr::Int],
                ret: anvyx_runtime::ExternTypeExpr::Named {
                    module: None,
                    name: "Thing".to_string(),
                    args: vec![],
                },
            },
            binding: None,
            effects: anvyx_runtime::ExternEffects::default(),
        });
        program.module_mut(module).externs.push(init);
        let konst = program.alloc_const(air::ConstData {
            ty: int,
            value: air::ConstValue::Int(1),
        });
        let arg = if provided {
            CallArg::InitFieldProvided(Operand::Const(konst))
        } else {
            CallArg::InitFieldOmitted
        };
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
                    callee: Callee::Extern(init),
                    args: vec![arg],
                })],
                air::AirTail::Return(None),
            ),
        });
        program.module_mut(module).functions.push(caller);
        (program, caller)
    }

    for provided in [false, true] {
        let (program, caller) = program_with_arg(provided);
        let errors = compile_errors(&program);

        assert!(has_compile_error(
            &errors,
            caller,
            VmCompileErrorKind::UnsupportedNativeInitField,
        ));
    }
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
        abi: air::ExternAbi {
            params: vec![
                anvyx_runtime::ExternTypeExpr::Named {
                    module: None,
                    name: "Thing".to_string(),
                    args: vec![],
                },
                anvyx_runtime::ExternTypeExpr::Int,
            ],
            ret: anvyx_runtime::ExternTypeExpr::Void,
        },
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
fn compiler_rejects_direct_function_payload_boundaries() {
    for name in ["param", "local", "return", "value_arg"] {
        let mut program = Program::default();
        let void = program.alloc_type(TypeData::Void);
        let function = function_ty(&mut program, void);
        let module = program.alloc_module(root_module());
        let (func, expected, expected_count) = match name {
            "param" => {
                let param_local = air::LocalId::from_index(0);
                let func = program.alloc_function(Function {
                    name: Ident::new("takes_lambda"),
                    module,
                    kind: FunctionKind::Normal,
                    owner: None,
                    specialization: None,
                    signature: Signature::new(
                        vec![param("f", function, ParamMode::Value, param_local)],
                        void,
                    ),
                    locals: vec![local(function, Mutability::Immutable, LocalKind::Arg)],
                    body: structured_body(vec![], air::AirTail::Return(None)),
                });
                (func, VmCompileErrorKind::UnsupportedLambdaType, 1)
            }
            "local" => {
                let func = program.alloc_function(Function {
                    name: Ident::new("stores_lambda"),
                    module,
                    kind: FunctionKind::Normal,
                    owner: None,
                    specialization: None,
                    signature: Signature::new(vec![], void),
                    locals: vec![local(function, Mutability::Immutable, LocalKind::User)],
                    body: structured_body(vec![], air::AirTail::Return(None)),
                });
                (func, VmCompileErrorKind::UnsupportedLambdaType, 1)
            }
            "return" => {
                let func = program.alloc_function(Function {
                    name: Ident::new("returns_lambda"),
                    module,
                    kind: FunctionKind::Normal,
                    owner: None,
                    specialization: None,
                    signature: Signature::new(vec![], function),
                    locals: vec![],
                    body: structured_body(vec![], air::AirTail::Unreachable),
                });
                (func, VmCompileErrorKind::UnsupportedLambdaType, 1)
            }
            "value_arg" => {
                let callee_local = air::LocalId::from_index(0);
                let caller_local = air::LocalId::from_index(0);
                let callee = program.alloc_function(Function {
                    name: Ident::new("callee"),
                    module,
                    kind: FunctionKind::Normal,
                    owner: None,
                    specialization: None,
                    signature: Signature::new(
                        vec![param("f", function, ParamMode::Value, callee_local)],
                        void,
                    ),
                    locals: vec![local(function, Mutability::Immutable, LocalKind::Arg)],
                    body: structured_body(vec![], air::AirTail::Return(None)),
                });
                let caller = program.alloc_function(Function {
                    name: Ident::new("caller"),
                    module,
                    kind: FunctionKind::Normal,
                    owner: None,
                    specialization: None,
                    signature: Signature::new(
                        vec![param("f", function, ParamMode::Value, caller_local)],
                        void,
                    ),
                    locals: vec![local(function, Mutability::Immutable, LocalKind::Arg)],
                    body: structured_body(
                        vec![Statement::Eval(RValue::Call {
                            callee: Callee::Function(callee),
                            args: vec![CallArg::Value(Operand::Place(place(
                                caller_local,
                                function,
                            )))],
                        })],
                        air::AirTail::Return(None),
                    ),
                });
                program
                    .module_mut(module)
                    .functions
                    .extend([callee, caller]);
                (caller, VmCompileErrorKind::UnsupportedLambdaValue, 1)
            }
            _ => unreachable!(),
        };
        if name != "value_arg" {
            program.module_mut(module).functions.push(func);
        }

        let errors = compile_errors(&program);
        if name == "value_arg" {
            assert_eq!(
                count_function_errors(&errors, func, expected),
                expected_count,
                "unexpected function payload gap count for {name}: {errors:?}"
            );
        } else {
            assert!(
                has_compile_error(&errors, func, expected),
                "missing function payload gap for {name}: {errors:?}"
            );
        }
    }
}

#[test]
fn compiler_rejects_lambda_storage_family_types() {
    for case in [
        StorageFamilyCase::Optional,
        StorageFamilyCase::Struct,
        StorageFamilyCase::Tuple,
        StorageFamilyCase::Array,
        StorageFamilyCase::Slice,
        StorageFamilyCase::List,
        StorageFamilyCase::MapValue,
        StorageFamilyCase::MapKey,
        StorageFamilyCase::DataRef,
        StorageFamilyCase::EnumTuple,
        StorageFamilyCase::EnumStruct,
        StorageFamilyCase::ExternField,
    ] {
        let mut program = Program::default();
        let module = program.alloc_module(root_module());
        let int = program.alloc_type(TypeData::Int);
        let void = program.alloc_type(TypeData::Void);
        let ty = case.ty(&mut program, module, int, void);
        let func = program.alloc_function(Function {
            name: Ident::new("main"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![local(ty, Mutability::Immutable, LocalKind::User)],
            body: structured_body(vec![], air::AirTail::Return(None)),
        });
        program.module_mut(module).functions.push(func);

        let errors = compile_errors(&program);
        assert!(
            has_compile_error(&errors, func, VmCompileErrorKind::UnsupportedLambdaType),
            "missing lambda type gap for {case:?}: {errors:?}"
        );
    }
}

#[test]
fn compiler_rejects_lambda_calls() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let function_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![air::ParamType {
            ty: string,
            mode: ParamMode::Value,
            escape: ParamEscape::NonEscaping,
        }],
        air::ReturnMode::Value(void),
    )));
    let module = program.alloc_module(root_module());
    let callee_local = air::LocalId::from_index(0);
    let value_local = air::LocalId::from_index(1);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(
            vec![
                param("f", function_ty, ParamMode::Value, callee_local),
                param("s", string, ParamMode::Value, value_local),
            ],
            void,
        ),
        locals: vec![
            local(function_ty, Mutability::Immutable, LocalKind::Arg),
            local(string, Mutability::Immutable, LocalKind::Arg),
        ],
        body: structured_body(
            vec![Statement::Eval(RValue::Call {
                callee: Callee::Lambda(Operand::Place(place(callee_local, function_ty))),
                args: vec![CallArg::Value(Operand::Place(place(value_local, string)))],
            })],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.push(func);

    let errors = compile_errors(&program);
    assert_eq!(
        count_function_errors(&errors, func, VmCompileErrorKind::UnsupportedLambdaCall),
        1
    );
    assert_eq!(
        count_function_errors(&errors, func, VmCompileErrorKind::NonCheapValueArg),
        1
    );
}

#[test]
fn compiler_rejects_global_matrix() {
    for case in [
        GlobalCase::Declaration,
        GlobalCase::LambdaType,
        GlobalCase::Root,
        GlobalCase::ProjectedRoot,
        GlobalCase::Ensure,
        GlobalCase::SetRoot,
        GlobalCase::SetRootLambda,
        GlobalCase::UpdateRoot,
        GlobalCase::UpdateRootLambda,
    ] {
        let (program, global, init, function, init_gap) = global_program(case);
        let errors = compile_errors(&program);
        assert!(
            errors.iter().any(|error| {
                error.site == VmCompileErrorSite::Global(global)
                    && error.kind == VmCompileErrorKind::UnsupportedGlobal
            }),
            "missing global declaration gap for {case:?}: {errors:?}"
        );
        assert!(
            has_compile_error(&errors, init, init_gap),
            "missing init gap for {case:?}: {errors:?}"
        );
        if let Some(function) = function {
            assert!(
                has_compile_error(&errors, function, VmCompileErrorKind::UnsupportedGlobal),
                "missing function global gap for {case:?}: {errors:?}"
            );
            if matches!(
                case,
                GlobalCase::SetRootLambda | GlobalCase::UpdateRootLambda
            ) {
                assert!(
                    has_compile_error(
                        &errors,
                        function,
                        VmCompileErrorKind::UnsupportedLambdaValue
                    ),
                    "missing nested lambda value gap for {case:?}: {errors:?}"
                );
            }
        }
    }
}

#[test]
fn vm_rejects_native_lambda_extern_boundaries() {
    for case in [
        NativeExternBoundaryCase::NonEscapingParam,
        NativeExternBoundaryCase::EscapingParam,
        NativeExternBoundaryCase::Return,
    ] {
        let (program, ext) = case.program();
        let errors = compile_errors(&program);
        assert!(
            has_extern_compile_error(
                &errors,
                ext,
                VmCompileErrorKind::UnsupportedLambdaExternBoundary
            ),
            "missing VM native lambda boundary gap for {case:?}"
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

#[test]
fn compiler_rejects_function_ref_named_values() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let function = function_ty(&mut program, void);
    let module = program.alloc_module(root_module());
    let target = program.alloc_function(Function {
        name: Ident::new("target"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    let main_local = air::LocalId::from_index(0);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(function, Mutability::Immutable, LocalKind::User)],
        body: structured_body(
            vec![Statement::Init {
                local: main_local,
                value: RValue::FunctionRef {
                    function: target,
                    ty: function,
                },
            }],
            air::AirTail::Return(None),
        ),
    });
    program.module_mut(module).functions.extend([target, main]);

    let errors = compile_errors(&program);
    assert!(has_compile_error(
        &errors,
        main,
        VmCompileErrorKind::UnsupportedLambdaValue
    ));
}

#[test]
fn compiler_rejects_capture_matrix() {
    for case in [
        CaptureCase::SourceNamedFunctionValue,
        CaptureCase::SourceReadonlyCapture,
        CaptureCase::SourceMutableCapture,
        CaptureCase::NoRuntimeArg,
        CaptureCase::ScopedBorrowArg,
        CaptureCase::ScopedLocalArg,
        CaptureCase::CaptureCellArg,
        CaptureCase::LambdaCaptureRoot,
        CaptureCase::LambdaCaptureProjectedRoot,
        CaptureCase::LambdaCaptureCellRoot,
        CaptureCase::LambdaCaptureCellProjectedRoot,
        CaptureCase::ScopedBorrowRoot,
        CaptureCase::ScopedBorrowProjectedRoot,
        CaptureCase::CaptureCellRoot,
        CaptureCase::CaptureCellProjectedRoot,
        CaptureCase::OwnerCaptureCellSingle,
        CaptureCase::OwnerCaptureCellShared,
        CaptureCase::LambdaBodyCaptureCell,
    ] {
        match case {
            CaptureCase::SourceNamedFunctionValue
            | CaptureCase::SourceReadonlyCapture
            | CaptureCase::SourceMutableCapture => {
                let lowered = lower_source_air(case.source().unwrap());
                let function = named_function(lowered.program(), "main");
                let errors = compile_verified_errors(&lowered);
                assert!(
                    has_compile_error(
                        &errors,
                        function,
                        VmCompileErrorKind::UnsupportedLambdaValue,
                    ),
                    "missing lambda value gap for {case:?}: {errors:?}"
                );
                if let Some(kind) = case.secondary_gap() {
                    assert!(
                        has_compile_error(&errors, function, kind),
                        "missing secondary gap for {case:?}: {errors:?}"
                    );
                }
            }
            CaptureCase::NoRuntimeArg
            | CaptureCase::ScopedBorrowArg
            | CaptureCase::ScopedLocalArg
            | CaptureCase::CaptureCellArg => {
                let (program, function, expected) =
                    make_lambda_value_program(case.lambda_value_case().unwrap());
                let errors = compile_errors(&program);
                assert_eq!(
                    count_function_errors(
                        &errors,
                        function,
                        VmCompileErrorKind::UnsupportedLambdaValue,
                    ),
                    1,
                    "unexpected lambda value count for {case:?}: {errors:?}"
                );
                match expected {
                    Some((kind, count)) => assert_eq!(
                        count_function_errors(&errors, function, kind),
                        count,
                        "unexpected secondary gap count for {case:?}: {errors:?}"
                    ),
                    None => {
                        assert_eq!(
                            count_function_errors(
                                &errors,
                                function,
                                VmCompileErrorKind::UnsupportedLambdaCapture,
                            ),
                            0,
                            "unexpected lambda capture gap for {case:?}: {errors:?}"
                        );
                        assert_eq!(
                            count_function_errors(
                                &errors,
                                function,
                                VmCompileErrorKind::UnsupportedLambdaCell,
                            ),
                            0,
                            "unexpected lambda cell gap for {case:?}: {errors:?}"
                        );
                    }
                }
            }
            CaptureCase::LambdaCaptureRoot
            | CaptureCase::LambdaCaptureProjectedRoot
            | CaptureCase::LambdaCaptureCellRoot
            | CaptureCase::LambdaCaptureCellProjectedRoot
            | CaptureCase::ScopedBorrowRoot
            | CaptureCase::ScopedBorrowProjectedRoot
            | CaptureCase::CaptureCellRoot
            | CaptureCase::CaptureCellProjectedRoot => {
                let (program, function, expected) =
                    lambda_root_program(case.lambda_root_case().unwrap());
                let errors = compile_errors(&program);
                assert!(
                    has_compile_error(&errors, function, expected),
                    "missing root gap {expected:?} for {case:?}: {errors:?}"
                );
            }
            CaptureCase::OwnerCaptureCellSingle | CaptureCase::OwnerCaptureCellShared => {
                let errors =
                    compile_errors(&mutable_upvalue_lambda_program(case.capture_cell_shared()));
                assert_function_errors(
                    &errors,
                    FunctionId::from_index(1),
                    &[
                        VmCompileErrorKind::UnsupportedLambdaValue,
                        VmCompileErrorKind::UnsupportedLambdaCell,
                    ],
                );
            }
            CaptureCase::LambdaBodyCaptureCell => {
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
        }
    }
}

#[test]
fn compiler_rejects_source_lowered_global_lambda_root() {
    let lowered = lower_source_air(
        "lazy let scale = 2;\nfn main() { let f: fn(int) -> int = |x| x * scale; }\n",
    );
    let errors = compile_verified_errors(&lowered);
    assert!(
        has_compile_error(
            &errors,
            first_lambda_function(lowered.program()),
            VmCompileErrorKind::UnsupportedGlobal,
        ),
        "missing global gap for source-lowered lambda root: {errors:?}"
    );
}

#[test]
fn compiler_rejects_collection_matrix() {
    for case in [
        CollectionCase::Loan,
        CollectionCase::SlotScope,
        CollectionCase::MapEntry,
        CollectionCase::LoanNestedLambda,
        CollectionCase::SlotScopeNestedLambda,
        CollectionCase::MapEntryNestedLambda,
        CollectionCase::MapEntryGlobalKey,
    ] {
        let (program, function, nested_gaps) = collection_program(case);
        let errors = compile_errors(&program);
        assert!(
            has_compile_error(
                &errors,
                function,
                VmCompileErrorKind::UnsupportedCollectionLoan,
            ),
            "missing collection gap for {case:?}: {errors:?}"
        );
        if matches!(case, CollectionCase::MapEntryNestedLambda) {
            assert_eq!(
                count_function_errors(
                    &errors,
                    function,
                    VmCompileErrorKind::UnsupportedLambdaValue,
                ),
                2,
                "missing both map-entry arm lambda gaps for {case:?}: {errors:?}"
            );
        }
        for gap in nested_gaps {
            assert!(
                has_compile_error(&errors, function, gap),
                "missing nested collection gap for {case:?}: {errors:?}"
            );
        }
    }
}

#[test]
fn compiler_rejects_recursive_function_payload_cycles_without_overflow() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let node = air::AggregateId::from_index(0);
    let node_ref = program.alloc_type(TypeData::DataRef(node));
    let aggregate = program.alloc_aggregate(AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: AggregateKind::DataRef,
        type_args: vec![],
        const_args: vec![],
        fields: vec![FieldDecl {
            name: Ident::new("next"),
            ty: node_ref,
        }],
        cycle_capable: true,
        stringify_override: None,
    });
    debug_assert_eq!(aggregate, node);
    program.module_mut(module).aggregates.push(node);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![local(node_ref, Mutability::Immutable, LocalKind::User)],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);

    let verified = air::verify(&program).expect("AIR verify failed");
    let vir = VmCompiler::compile(verified).expect("VM compile failed");
    assert_eq!(vir.functions.len(), 1);
    assert_eq!(vir.functions[0].source, func);
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
        lifetime: CaptureCellLifetime::Function,
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

fn function_ty(program: &mut Program, ret: TypeId) -> TypeId {
    program.alloc_type(TypeData::Function(air::SignatureType::new(
        vec![],
        air::ReturnMode::Value(ret),
    )))
}

fn projected_root(root: PlaceRoot, ty: TypeId) -> Place {
    Place {
        root,
        projection: vec![Projection::TupleField(0)],
        ty,
    }
}

fn bound_local(ty: TypeId, mutability: Mutability, kind: LocalKind, binding: BindingId) -> Local {
    let mut local = local(ty, mutability, kind);
    local.binding = Some(binding);
    local
}

fn make_lambda_value_program(
    case: LambdaValueCase,
) -> (Program, FunctionId, Option<(VmCompileErrorKind, usize)>) {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let int = program.alloc_type(TypeData::Int);
    let pair = program.alloc_type(TypeData::Tuple(vec![int, int]));
    let lambda_ty = function_ty(&mut program, void);
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source = air::LocalId::from_index(0);
    let body = FunctionId::from_index(0);
    let owner = FunctionId::from_index(1);

    let (captures, args, params, locals, cell, expected) = match case {
        LambdaValueCase::NoRuntime => (
            vec![air::LambdaCaptureDecl::NoRuntime { binding, ty: int }],
            vec![air::LambdaCaptureArg::NoRuntime],
            vec![],
            vec![],
            None,
            None,
        ),
        LambdaValueCase::ScopedBorrow => {
            let scoped = program.alloc_scoped_borrow(ScopedBorrowDecl {
                owner,
                binding,
                source: ScopedBorrowSource::SourceMutParam { local: source },
                ty: pair,
                mutability: Mutability::Mutable,
            });
            (
                vec![air::LambdaCaptureDecl::ScopedBorrow {
                    binding,
                    borrow: scoped,
                    ty: pair,
                    mutability: Mutability::Mutable,
                }],
                vec![air::LambdaCaptureArg::ScopedBorrow {
                    place: Place {
                        root: PlaceRoot::ScopedBorrow(scoped),
                        projection: vec![],
                        ty: pair,
                    },
                }],
                vec![param("source", pair, ParamMode::MutBorrow, source)],
                vec![bound_local(
                    pair,
                    Mutability::Mutable,
                    LocalKind::Arg,
                    binding,
                )],
                None,
                Some((VmCompileErrorKind::UnsupportedLambdaCapture, 1)),
            )
        }
        LambdaValueCase::ScopedLocal => (
            vec![air::LambdaCaptureDecl::ScopedLocal {
                binding,
                source: CaptureLocalSource {
                    owner,
                    local: source,
                },
                ty: pair,
                mutability: Mutability::Mutable,
            }],
            vec![air::LambdaCaptureArg::ScopedLocal {
                place: place(source, pair),
            }],
            vec![],
            vec![bound_local(
                pair,
                Mutability::Mutable,
                LocalKind::User,
                binding,
            )],
            None,
            Some((VmCompileErrorKind::UnsupportedLambdaCapture, 1)),
        ),
        LambdaValueCase::CaptureCell => {
            let cell = program.alloc_capture_cell(CaptureCellDecl {
                binding,
                owner,
                source_local: source,
                ty: int,
                lifetime: CaptureCellLifetime::Function,
            });
            (
                vec![air::LambdaCaptureDecl::CaptureCell {
                    binding,
                    cell,
                    ty: int,
                }],
                vec![air::LambdaCaptureArg::CaptureCell { cell }],
                vec![],
                vec![bound_local(
                    int,
                    Mutability::Mutable,
                    LocalKind::User,
                    binding,
                )],
                Some(cell),
                Some((VmCompileErrorKind::UnsupportedLambdaCell, 2)),
            )
        }
    };
    let escape = match case {
        LambdaValueCase::ScopedBorrow | LambdaValueCase::ScopedLocal => LambdaEscape::NonEscaping,
        LambdaValueCase::NoRuntime | LambdaValueCase::CaptureCell => LambdaEscape::Escaping,
    };
    let lambda = program.alloc_lambda(LambdaDecl {
        source: ExprId(0),
        module,
        body,
        owner,
        signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
        escape,
        captures,
    });
    let body_fn = program.alloc_function(Function {
        name: Ident::new("lambda"),
        module,
        kind: FunctionKind::Lambda(lambda),
        owner: None,
        specialization: None,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: structured_body(vec![], air::AirTail::Return(None)),
    });
    debug_assert_eq!(body_fn, body);
    let zero = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(0),
    });
    let mut stmts = vec![];
    if matches!(case, LambdaValueCase::ScopedLocal) {
        stmts.push(Statement::Init {
            local: source,
            value: RValue::Aggregate {
                kind: air::AggregateCtor::Tuple,
                fields: vec![Operand::Const(zero), Operand::Const(zero)],
                ty: pair,
            },
        });
    }
    if let Some(cell) = cell {
        stmts.push(Statement::Assign {
            dst: Place {
                root: PlaceRoot::CaptureCell(cell),
                projection: vec![],
                ty: int,
            },
            value: RValue::Use(Operand::Const(zero)),
        });
    }
    stmts.push(Statement::Eval(RValue::MakeLambda {
        lambda,
        captures: args,
        ty: lambda_ty,
    }));
    let owner_fn = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(params, void),
        locals,
        body: structured_body(stmts, air::AirTail::Return(None)),
    });
    debug_assert_eq!(owner_fn, owner);
    program
        .module_mut(module)
        .functions
        .extend([body_fn, owner_fn]);
    (program, owner_fn, expected)
}

fn lambda_root_program(case: LambdaRootCase) -> (Program, FunctionId, VmCompileErrorKind) {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let int = program.alloc_type(TypeData::Int);
    let pair = program.alloc_type(TypeData::Tuple(vec![int, int]));
    let module = program.alloc_module(root_module());
    let binding = BindingId::from_index(0);
    let source = air::LocalId::from_index(0);

    match case {
        LambdaRootCase::LambdaCapture | LambdaRootCase::LambdaCaptureProjected => {
            let body = FunctionId::from_index(0);
            let owner = FunctionId::from_index(1);
            let lambda = program.alloc_lambda(LambdaDecl {
                source: ExprId(0),
                module,
                body,
                owner,
                signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
                escape: LambdaEscape::Escaping,
                captures: vec![air::LambdaCaptureDecl::ReadonlyLocal {
                    binding,
                    source: CaptureLocalSource {
                        owner,
                        local: source,
                    },
                    ty: pair,
                }],
            });
            let body_fn = program.alloc_function(Function {
                name: Ident::new("lambda"),
                module,
                kind: FunctionKind::Lambda(lambda),
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![],
                body: structured_body(
                    vec![Statement::Eval(RValue::Use(Operand::Place(
                        if matches!(case, LambdaRootCase::LambdaCaptureProjected) {
                            projected_root(
                                PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                                int,
                            )
                        } else {
                            Place {
                                root: PlaceRoot::LambdaCapture(
                                    air::LambdaCaptureSlotId::from_index(0),
                                ),
                                projection: vec![],
                                ty: pair,
                            }
                        },
                    )))],
                    air::AirTail::Return(None),
                ),
            });
            let owner_fn = program.alloc_function(Function {
                name: Ident::new("main"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![bound_local(
                    pair,
                    Mutability::Immutable,
                    LocalKind::User,
                    binding,
                )],
                body: structured_body(vec![], air::AirTail::Return(None)),
            });
            debug_assert_eq!(body_fn, body);
            debug_assert_eq!(owner_fn, owner);
            program
                .module_mut(module)
                .functions
                .extend([body_fn, owner_fn]);
            (
                program,
                body_fn,
                VmCompileErrorKind::UnsupportedLambdaCapture,
            )
        }
        LambdaRootCase::LambdaCaptureCell | LambdaRootCase::LambdaCaptureCellProjected => {
            let body = FunctionId::from_index(0);
            let owner = FunctionId::from_index(1);
            let cell_ty = if matches!(case, LambdaRootCase::LambdaCaptureCellProjected) {
                pair
            } else {
                int
            };
            let cell = program.alloc_capture_cell(CaptureCellDecl {
                binding,
                owner,
                source_local: source,
                ty: cell_ty,
                lifetime: CaptureCellLifetime::Function,
            });
            let lambda = program.alloc_lambda(LambdaDecl {
                source: ExprId(0),
                module,
                body,
                owner,
                signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
                escape: LambdaEscape::Escaping,
                captures: vec![air::LambdaCaptureDecl::CaptureCell {
                    binding,
                    cell,
                    ty: cell_ty,
                }],
            });
            let body_fn = program.alloc_function(Function {
                name: Ident::new("lambda"),
                module,
                kind: FunctionKind::Lambda(lambda),
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![],
                body: structured_body(
                    vec![Statement::Eval(RValue::Use(Operand::Place(
                        if matches!(case, LambdaRootCase::LambdaCaptureCellProjected) {
                            projected_root(
                                PlaceRoot::LambdaCapture(air::LambdaCaptureSlotId::from_index(0)),
                                int,
                            )
                        } else {
                            Place {
                                root: PlaceRoot::LambdaCapture(
                                    air::LambdaCaptureSlotId::from_index(0),
                                ),
                                projection: vec![],
                                ty: int,
                            }
                        },
                    )))],
                    air::AirTail::Return(None),
                ),
            });
            let zero = program.alloc_const(air::ConstData {
                ty: int,
                value: air::ConstValue::Int(0),
            });
            let init = if matches!(case, LambdaRootCase::LambdaCaptureCellProjected) {
                RValue::Aggregate {
                    kind: air::AggregateCtor::Tuple,
                    fields: vec![Operand::Const(zero), Operand::Const(zero)],
                    ty: pair,
                }
            } else {
                RValue::Use(Operand::Const(zero))
            };
            let lambda_ty = function_ty(&mut program, void);
            let owner_fn = program.alloc_function(Function {
                name: Ident::new("main"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![bound_local(
                    cell_ty,
                    Mutability::Mutable,
                    LocalKind::User,
                    binding,
                )],
                body: structured_body(
                    vec![
                        Statement::Assign {
                            dst: Place {
                                root: PlaceRoot::CaptureCell(cell),
                                projection: vec![],
                                ty: cell_ty,
                            },
                            value: init,
                        },
                        Statement::Eval(RValue::MakeLambda {
                            lambda,
                            captures: vec![air::LambdaCaptureArg::CaptureCell { cell }],
                            ty: lambda_ty,
                        }),
                    ],
                    air::AirTail::Return(None),
                ),
            });
            debug_assert_eq!(body_fn, body);
            debug_assert_eq!(owner_fn, owner);
            program
                .module_mut(module)
                .functions
                .extend([body_fn, owner_fn]);
            (program, body_fn, VmCompileErrorKind::UnsupportedLambdaCell)
        }
        LambdaRootCase::ScopedBorrow | LambdaRootCase::ScopedBorrowProjected => {
            let function = FunctionId::from_index(0);
            let scoped = program.alloc_scoped_borrow(ScopedBorrowDecl {
                owner: function,
                binding,
                source: ScopedBorrowSource::SourceMutParam { local: source },
                ty: pair,
                mutability: Mutability::Mutable,
            });
            let place = if matches!(case, LambdaRootCase::ScopedBorrowProjected) {
                projected_root(PlaceRoot::ScopedBorrow(scoped), int)
            } else {
                Place {
                    root: PlaceRoot::ScopedBorrow(scoped),
                    projection: vec![],
                    ty: pair,
                }
            };
            let func = program.alloc_function(Function {
                name: Ident::new("main"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(
                    vec![param("source", pair, ParamMode::MutBorrow, source)],
                    void,
                ),
                locals: vec![bound_local(
                    pair,
                    Mutability::Mutable,
                    LocalKind::Arg,
                    binding,
                )],
                body: structured_body(
                    vec![Statement::Eval(RValue::Use(Operand::Place(place)))],
                    air::AirTail::Return(None),
                ),
            });
            debug_assert_eq!(func, function);
            program.module_mut(module).functions.push(func);
            (program, func, VmCompileErrorKind::UnsupportedLambdaCapture)
        }
        LambdaRootCase::CaptureCell | LambdaRootCase::CaptureCellProjected => {
            let function = FunctionId::from_index(0);
            let cell_ty = if matches!(case, LambdaRootCase::CaptureCellProjected) {
                pair
            } else {
                int
            };
            let cell = program.alloc_capture_cell(CaptureCellDecl {
                binding,
                owner: function,
                source_local: source,
                ty: cell_ty,
                lifetime: CaptureCellLifetime::Function,
            });
            let place = if matches!(case, LambdaRootCase::CaptureCellProjected) {
                projected_root(PlaceRoot::CaptureCell(cell), int)
            } else {
                Place {
                    root: PlaceRoot::CaptureCell(cell),
                    projection: vec![],
                    ty: int,
                }
            };
            let zero = program.alloc_const(air::ConstData {
                ty: int,
                value: air::ConstValue::Int(0),
            });
            let init = if matches!(case, LambdaRootCase::CaptureCellProjected) {
                RValue::Aggregate {
                    kind: air::AggregateCtor::Tuple,
                    fields: vec![Operand::Const(zero), Operand::Const(zero)],
                    ty: pair,
                }
            } else {
                RValue::Use(Operand::Const(zero))
            };
            let func = program.alloc_function(Function {
                name: Ident::new("main"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![bound_local(
                    cell_ty,
                    Mutability::Mutable,
                    LocalKind::User,
                    binding,
                )],
                body: structured_body(
                    vec![
                        Statement::Assign {
                            dst: Place {
                                root: PlaceRoot::CaptureCell(cell),
                                projection: vec![],
                                ty: cell_ty,
                            },
                            value: init,
                        },
                        Statement::Eval(RValue::Use(Operand::Place(place))),
                    ],
                    air::AirTail::Return(None),
                ),
            });
            debug_assert_eq!(func, function);
            program.module_mut(module).functions.push(func);
            (program, func, VmCompileErrorKind::UnsupportedLambdaCell)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StorageFamilyCase {
    Optional,
    Struct,
    Tuple,
    Array,
    Slice,
    List,
    MapValue,
    MapKey,
    DataRef,
    EnumTuple,
    EnumStruct,
    ExternField,
}

impl StorageFamilyCase {
    fn ty(self, program: &mut Program, module: air::ModuleId, int: TypeId, void: TypeId) -> TypeId {
        let function = function_ty(program, void);
        match self {
            Self::Optional => program.alloc_type(TypeData::Optional(function)),
            Self::Struct => {
                let id = program.alloc_aggregate(AggregateDecl {
                    name: Ident::new("Payload"),
                    module,
                    kind: AggregateKind::Struct,
                    type_args: vec![],
                    const_args: vec![],
                    fields: vec![FieldDecl {
                        name: Ident::new("f"),
                        ty: function,
                    }],
                    cycle_capable: false,
                    stringify_override: None,
                });
                program.module_mut(module).aggregates.push(id);
                program.alloc_type(TypeData::Aggregate(id))
            }
            Self::Tuple => program.alloc_type(TypeData::Tuple(vec![int, function])),
            Self::Array => program.alloc_type(TypeData::Array {
                elem: function,
                len: 1,
            }),
            Self::Slice => program.alloc_type(TypeData::Slice(function)),
            Self::List => program.alloc_type(TypeData::List(function)),
            Self::MapValue => program.alloc_type(TypeData::Map {
                key: int,
                value: function,
                order: air::MapOrder::Insertion,
            }),
            Self::MapKey => program.alloc_type(TypeData::Map {
                key: function,
                value: int,
                order: air::MapOrder::Insertion,
            }),
            Self::DataRef => {
                let id = program.alloc_aggregate(AggregateDecl {
                    name: Ident::new("PayloadRef"),
                    module,
                    kind: AggregateKind::DataRef,
                    type_args: vec![],
                    const_args: vec![],
                    fields: vec![FieldDecl {
                        name: Ident::new("f"),
                        ty: function,
                    }],
                    cycle_capable: true,
                    stringify_override: None,
                });
                program.module_mut(module).aggregates.push(id);
                program.alloc_type(TypeData::DataRef(id))
            }
            Self::EnumTuple => {
                let id = program.alloc_enum(EnumDecl {
                    name: Ident::new("PayloadEnum"),
                    module,
                    type_args: vec![],
                    const_args: vec![],
                    variants: vec![VariantDecl {
                        name: Ident::new("Some"),
                        shape: VariantShape::Tuple(vec![function]),
                        raw_value: None,
                    }],
                    core: None,
                    repr: EnumRepr::Adt,
                    raw_type: None,
                });
                program.module_mut(module).enums.push(id);
                program.alloc_type(TypeData::Enum(id))
            }
            Self::EnumStruct => {
                let id = program.alloc_enum(EnumDecl {
                    name: Ident::new("PayloadEnum"),
                    module,
                    type_args: vec![],
                    const_args: vec![],
                    variants: vec![VariantDecl {
                        name: Ident::new("Some"),
                        shape: VariantShape::Struct(vec![FieldDecl {
                            name: Ident::new("f"),
                            ty: function,
                        }]),
                        raw_value: None,
                    }],
                    core: None,
                    repr: EnumRepr::Adt,
                    raw_type: None,
                });
                program.module_mut(module).enums.push(id);
                program.alloc_type(TypeData::Enum(id))
            }
            Self::ExternField => {
                let id = program.alloc_extern_type(ExternTypeDecl {
                    name: Ident::new("PayloadExtern"),
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
                    fields: vec![ExternFieldDecl {
                        name: Ident::new("f"),
                        ty: function,
                        abi: anvyx_runtime::ExternTypeExpr::Int,
                        get_receiver: ExternReceiverDecl {
                            ty: TypeId::from_index(0),
                            mode: ParamMode::SharedBorrow,
                        },
                        set_receiver: ExternReceiverDecl {
                            ty: TypeId::from_index(0),
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
                });
                program.module_mut(module).extern_types.push(id);
                let owner_ty = program.alloc_type(TypeData::Extern(id));
                let extern_ty = program.extern_type_mut(id);
                extern_ty.fields[0].get_receiver.ty = owner_ty;
                extern_ty.fields[0].set_receiver.ty = owner_ty;
                owner_ty
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CaptureCase {
    SourceNamedFunctionValue,
    SourceReadonlyCapture,
    SourceMutableCapture,
    NoRuntimeArg,
    ScopedBorrowArg,
    ScopedLocalArg,
    CaptureCellArg,
    LambdaCaptureRoot,
    LambdaCaptureProjectedRoot,
    LambdaCaptureCellRoot,
    LambdaCaptureCellProjectedRoot,
    ScopedBorrowRoot,
    ScopedBorrowProjectedRoot,
    CaptureCellRoot,
    CaptureCellProjectedRoot,
    OwnerCaptureCellSingle,
    OwnerCaptureCellShared,
    LambdaBodyCaptureCell,
}

impl CaptureCase {
    fn source(self) -> Option<&'static str> {
        match self {
            Self::SourceNamedFunctionValue => Some("fn tick() {}\nfn main() { let f = tick; }\n"),
            Self::SourceReadonlyCapture => Some("fn main() { let x = 1; let f = || x; }\n"),
            Self::SourceMutableCapture => {
                Some("fn main() { var x = 1; let f = || { x += 1; }; }\n")
            }
            _ => None,
        }
    }

    fn secondary_gap(self) -> Option<VmCompileErrorKind> {
        match self {
            Self::SourceReadonlyCapture => Some(VmCompileErrorKind::UnsupportedLambdaCapture),
            Self::SourceMutableCapture => Some(VmCompileErrorKind::UnsupportedLambdaCell),
            _ => None,
        }
    }

    fn lambda_value_case(self) -> Option<LambdaValueCase> {
        match self {
            Self::NoRuntimeArg => Some(LambdaValueCase::NoRuntime),
            Self::ScopedBorrowArg => Some(LambdaValueCase::ScopedBorrow),
            Self::ScopedLocalArg => Some(LambdaValueCase::ScopedLocal),
            Self::CaptureCellArg => Some(LambdaValueCase::CaptureCell),
            _ => None,
        }
    }

    fn lambda_root_case(self) -> Option<LambdaRootCase> {
        match self {
            Self::LambdaCaptureRoot => Some(LambdaRootCase::LambdaCapture),
            Self::LambdaCaptureProjectedRoot => Some(LambdaRootCase::LambdaCaptureProjected),
            Self::LambdaCaptureCellRoot => Some(LambdaRootCase::LambdaCaptureCell),
            Self::LambdaCaptureCellProjectedRoot => {
                Some(LambdaRootCase::LambdaCaptureCellProjected)
            }
            Self::ScopedBorrowRoot => Some(LambdaRootCase::ScopedBorrow),
            Self::ScopedBorrowProjectedRoot => Some(LambdaRootCase::ScopedBorrowProjected),
            Self::CaptureCellRoot => Some(LambdaRootCase::CaptureCell),
            Self::CaptureCellProjectedRoot => Some(LambdaRootCase::CaptureCellProjected),
            _ => None,
        }
    }

    fn capture_cell_shared(self) -> bool {
        matches!(self, Self::OwnerCaptureCellShared)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LambdaValueCase {
    NoRuntime,
    ScopedBorrow,
    ScopedLocal,
    CaptureCell,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LambdaRootCase {
    LambdaCapture,
    LambdaCaptureProjected,
    LambdaCaptureCell,
    LambdaCaptureCellProjected,
    ScopedBorrow,
    ScopedBorrowProjected,
    CaptureCell,
    CaptureCellProjected,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeExternBoundaryCase {
    NonEscapingParam,
    EscapingParam,
    Return,
}

impl NativeExternBoundaryCase {
    fn program(self) -> (Program, air::ExternId) {
        let mut program = Program::default();
        let void = program.alloc_type(TypeData::Void);
        let function_ty = function_ty(&mut program, void);
        let module = program.alloc_module(root_module());
        let (name, params, return_type) = match self {
            Self::NonEscapingParam => (
                "non_escaping",
                vec![ExternParamDecl {
                    ty: function_ty,
                    mode: ParamMode::Value,
                    escape: ParamEscape::NonEscaping,
                }],
                void,
            ),
            Self::EscapingParam => (
                "escaping",
                vec![ExternParamDecl {
                    ty: function_ty,
                    mode: ParamMode::Value,
                    escape: ParamEscape::Escaping,
                }],
                void,
            ),
            Self::Return => ("returns_lambda", vec![], function_ty),
        };
        let callback =
            anvyx_runtime::ExternTypeExpr::Callback(anvyx_runtime::ExternCallbackSignature {
                params: vec![],
                ret: Box::new(anvyx_runtime::ExternTypeExpr::Void),
                policy: anvyx_runtime::CallbackPolicy {
                    escape: anvyx_runtime::CallbackEscape::NonEscaping,
                    thread: anvyx_runtime::CallbackThread::SameThread,
                },
            });
        let abi = air::ExternAbi {
            params: params.iter().map(|_| callback.clone()).collect(),
            ret: anvyx_runtime::ExternTypeExpr::Void,
        };
        let ext = program.alloc_extern(ExternDecl {
            name: Ident::new(name),
            module,
            member: ExternMember::FreeFunction,
            params,
            return_type,
            abi,
            binding: None,
            effects: anvyx_runtime::ExternEffects::default(),
        });
        program.module_mut(module).externs.push(ext);
        (program, ext)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GlobalCase {
    Declaration,
    LambdaType,
    Root,
    ProjectedRoot,
    Ensure,
    SetRoot,
    SetRootLambda,
    UpdateRoot,
    UpdateRootLambda,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CollectionCase {
    Loan,
    SlotScope,
    MapEntry,
    LoanNestedLambda,
    SlotScopeNestedLambda,
    MapEntryNestedLambda,
    MapEntryGlobalKey,
}

fn global_program(
    case: GlobalCase,
) -> (
    Program,
    air::GlobalId,
    FunctionId,
    Option<FunctionId>,
    VmCompileErrorKind,
) {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let pair = program.alloc_type(TypeData::Tuple(vec![int, int]));
    let void = program.alloc_type(TypeData::Void);
    let function = function_ty(&mut program, void);
    let module = program.alloc_module(root_module());
    let one = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(1),
    });

    match case {
        GlobalCase::LambdaType => {
            let global = global_with_init(
                &mut program,
                module,
                "callback",
                function,
                Mutability::Immutable,
            );
            let init = program.globals[global.index()].init;
            (
                program,
                global,
                init,
                None,
                VmCompileErrorKind::UnsupportedLambdaType,
            )
        }
        GlobalCase::Declaration => {
            let global = global_with_init(&mut program, module, "g", int, Mutability::Immutable);
            let init = program.globals[global.index()].init;
            (
                program,
                global,
                init,
                None,
                VmCompileErrorKind::UnsupportedGlobal,
            )
        }
        GlobalCase::Root => {
            let global = global_with_init(&mut program, module, "g", int, Mutability::Immutable);
            let init = program.globals[global.index()].init;
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
            (
                program,
                global,
                init,
                Some(func),
                VmCompileErrorKind::UnsupportedGlobal,
            )
        }
        GlobalCase::ProjectedRoot => {
            let global = global_with_init(&mut program, module, "pair", pair, Mutability::Mutable);
            let init = program.globals[global.index()].init;
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
                    air::AirTail::Return(Some(Operand::Place(projected_root(
                        PlaceRoot::Global(global),
                        int,
                    )))),
                ),
            });
            program.module_mut(module).functions.push(func);
            (
                program,
                global,
                init,
                Some(func),
                VmCompileErrorKind::UnsupportedGlobal,
            )
        }
        GlobalCase::Ensure => {
            let global = global_with_init(&mut program, module, "g", int, Mutability::Mutable);
            let init = program.globals[global.index()].init;
            let func = program.alloc_function(Function {
                name: Ident::new("main"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![],
                body: structured_body(
                    vec![Statement::GlobalEnsure { global }],
                    air::AirTail::Return(None),
                ),
            });
            program.module_mut(module).functions.push(func);
            (
                program,
                global,
                init,
                Some(func),
                VmCompileErrorKind::UnsupportedGlobal,
            )
        }
        GlobalCase::SetRoot | GlobalCase::SetRootLambda => {
            let ty = if matches!(case, GlobalCase::SetRootLambda) {
                function
            } else {
                int
            };
            let global = global_with_init(&mut program, module, "g", ty, Mutability::Mutable);
            let init = program.globals[global.index()].init;
            let target = program.alloc_function(Function {
                name: Ident::new("target"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![],
                body: structured_body(vec![], air::AirTail::Return(None)),
            });
            program.module_mut(module).functions.push(target);
            let value = match case {
                GlobalCase::SetRootLambda => RValue::FunctionRef {
                    function: target,
                    ty: function,
                },
                _ => RValue::Use(Operand::Const(one)),
            };
            let func = program.alloc_function(Function {
                name: Ident::new("main"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![],
                body: structured_body(
                    vec![Statement::GlobalSetRoot {
                        global,
                        value,
                        init: air::GlobalInitEffect::StoreWithoutInit,
                    }],
                    air::AirTail::Return(None),
                ),
            });
            program.module_mut(module).functions.push(func);
            (
                program,
                global,
                init,
                Some(func),
                if matches!(case, GlobalCase::SetRootLambda) {
                    VmCompileErrorKind::UnsupportedLambdaType
                } else {
                    VmCompileErrorKind::UnsupportedGlobal
                },
            )
        }
        GlobalCase::UpdateRoot | GlobalCase::UpdateRootLambda => {
            let ty = if matches!(case, GlobalCase::UpdateRootLambda) {
                function
            } else {
                int
            };
            let global = global_with_init(&mut program, module, "g", ty, Mutability::Mutable);
            let init = program.globals[global.index()].init;
            let target = program.alloc_function(Function {
                name: Ident::new("target"),
                module,
                kind: FunctionKind::Normal,
                owner: None,
                specialization: None,
                signature: Signature::new(vec![], void),
                locals: vec![],
                body: structured_body(vec![], air::AirTail::Return(None)),
            });
            program.module_mut(module).functions.push(target);
            let value = match case {
                GlobalCase::UpdateRootLambda => RValue::FunctionRef {
                    function: target,
                    ty: function,
                },
                _ => RValue::Use(Operand::Const(one)),
            };
            let func = program.alloc_function(Function {
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
                        Statement::GlobalUpdateRoot { global, value },
                    ],
                    air::AirTail::Return(None),
                ),
            });
            program.module_mut(module).functions.push(func);
            (
                program,
                global,
                init,
                Some(func),
                if matches!(case, GlobalCase::UpdateRootLambda) {
                    VmCompileErrorKind::UnsupportedLambdaType
                } else {
                    VmCompileErrorKind::UnsupportedGlobal
                },
            )
        }
    }
}

fn collection_program(case: CollectionCase) -> (Program, FunctionId, Vec<VmCompileErrorKind>) {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let list = program.alloc_type(TypeData::List(int));
    let map = program.alloc_type(TypeData::Map {
        key: int,
        value: int,
        order: air::MapOrder::Insertion,
    });
    let void = program.alloc_type(TypeData::Void);
    let lambda_ty = function_ty(&mut program, void);
    let module = program.alloc_module(root_module());
    let root = air::LocalId::from_index(0);
    let key = program.alloc_const(air::ConstData {
        ty: int,
        value: air::ConstValue::Int(0),
    });
    let nested_body = matches!(
        case,
        CollectionCase::LoanNestedLambda
            | CollectionCase::SlotScopeNestedLambda
            | CollectionCase::MapEntryNestedLambda
    );
    let lambda_body = if nested_body {
        let lambda_body = FunctionId::from_index(0);
        let owner = if matches!(case, CollectionCase::MapEntryNestedLambda) {
            FunctionId::from_index(2)
        } else {
            FunctionId::from_index(1)
        };
        let lambda = program.alloc_lambda(LambdaDecl {
            source: ExprId(0),
            module,
            body: lambda_body,
            owner,
            signature: air::SignatureType::new(vec![], air::ReturnMode::Value(void)),
            escape: LambdaEscape::Escaping,
            captures: vec![],
        });
        let body_fn = program.alloc_function(Function {
            name: Ident::new("lambda"),
            module,
            kind: FunctionKind::Lambda(lambda),
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![],
            body: structured_body(vec![], air::AirTail::Return(None)),
        });
        debug_assert_eq!(body_fn, lambda_body);
        program.module_mut(module).functions.push(body_fn);
        Some(air::AirBlock {
            stmts: vec![Statement::Eval(RValue::MakeLambda {
                lambda,
                captures: vec![],
                ty: lambda_ty,
            })],
            tail: air::AirTail::None,
        })
    } else {
        None
    };
    let alternate_body = if matches!(case, CollectionCase::MapEntryNestedLambda) {
        let target = program.alloc_function(Function {
            name: Ident::new("target"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![],
            body: structured_body(vec![], air::AirTail::Return(None)),
        });
        program.module_mut(module).functions.push(target);
        Some(air::AirBlock {
            stmts: vec![Statement::Eval(RValue::FunctionRef {
                function: target,
                ty: lambda_ty,
            })],
            tail: air::AirTail::None,
        })
    } else {
        None
    };
    let empty_body = air::AirBlock {
        stmts: vec![],
        tail: air::AirTail::None,
    };
    let map_key = match case {
        CollectionCase::MapEntryGlobalKey => {
            let global = global_with_init(&mut program, module, "key", int, Mutability::Immutable);
            Operand::Place(Place {
                root: PlaceRoot::Global(global),
                projection: vec![],
                ty: int,
            })
        }
        _ => Operand::Const(key),
    };
    let stmt = match case {
        CollectionCase::Loan | CollectionCase::LoanNestedLambda => {
            Statement::CollectionLoan(AirCollectionLoan {
                root: place(root, list),
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::ReadonlySequence,
                body: lambda_body.clone().unwrap_or_else(|| empty_body.clone()),
            })
        }
        CollectionCase::SlotScope | CollectionCase::SlotScopeNestedLambda => {
            Statement::CollectionSlotScope(AirCollectionSlotScope {
                root: place(root, list),
                index: air::LocalId::from_index(1),
                slots: vec![],
                body: lambda_body.clone().unwrap_or_else(|| empty_body.clone()),
            })
        }
        CollectionCase::MapEntry
        | CollectionCase::MapEntryNestedLambda
        | CollectionCase::MapEntryGlobalKey => Statement::MapEntryMatch(AirMapEntryMatch {
            map: Place {
                root: PlaceRoot::Local(root),
                projection: vec![],
                ty: map,
            },
            key: map_key,
            payload: None,
            payload_escapes: false,
            some_block: lambda_body.clone().unwrap_or_else(|| empty_body.clone()),
            none_block: alternate_body
                .clone()
                .or_else(|| lambda_body.clone())
                .unwrap_or_else(|| empty_body.clone()),
        }),
    };
    let (params, locals, nested_gaps) = match case {
        CollectionCase::SlotScope | CollectionCase::SlotScopeNestedLambda => (
            vec![
                param("root", list, ParamMode::Value, root),
                param("index", int, ParamMode::Value, air::LocalId::from_index(1)),
            ],
            vec![
                local(list, Mutability::Mutable, LocalKind::Arg),
                local(int, Mutability::Immutable, LocalKind::Arg),
            ],
            if matches!(case, CollectionCase::SlotScopeNestedLambda) {
                vec![VmCompileErrorKind::UnsupportedLambdaValue]
            } else {
                vec![]
            },
        ),
        CollectionCase::MapEntry
        | CollectionCase::MapEntryNestedLambda
        | CollectionCase::MapEntryGlobalKey => (
            vec![param("root", map, ParamMode::Value, root)],
            vec![local(map, Mutability::Mutable, LocalKind::Arg)],
            match case {
                CollectionCase::MapEntryNestedLambda => {
                    vec![VmCompileErrorKind::UnsupportedLambdaValue]
                }
                CollectionCase::MapEntryGlobalKey => vec![VmCompileErrorKind::UnsupportedGlobal],
                _ => vec![],
            },
        ),
        CollectionCase::LoanNestedLambda => (
            vec![param("root", list, ParamMode::Value, root)],
            vec![local(list, Mutability::Mutable, LocalKind::Arg)],
            vec![VmCompileErrorKind::UnsupportedLambdaValue],
        ),
        CollectionCase::Loan => (
            vec![param("root", list, ParamMode::Value, root)],
            vec![local(list, Mutability::Mutable, LocalKind::Arg)],
            vec![],
        ),
    };
    let func = program.alloc_function(Function {
        name: Ident::new("collection"),
        module,
        kind: FunctionKind::Normal,
        owner: None,
        specialization: None,
        signature: Signature::new(params, void),
        locals,
        body: structured_body(vec![stmt], air::AirTail::Return(None)),
    });
    program.module_mut(module).functions.push(func);
    (program, func, nested_gaps)
}

#[derive(Default)]
struct VmTestSourceLoader;

impl PackageSourceLoader for VmTestSourceLoader {
    type FatalError = Infallible;

    fn load(
        &mut self,
        _module: &ModuleId,
    ) -> Result<Option<PackageModuleInput>, SourceLoadError<Self::FatalError>> {
        Ok(None)
    }
}

fn lower_source_air(source: &str) -> air::OwnedVerifiedProgram {
    let root = PackageId::synthetic_root();
    let mut loader = VmTestSourceLoader;
    let input = PackageProgramInput {
        root_package: root.clone(),
        main: PackageModuleInput {
            module: ModuleId::root(root.clone()),
            source: Source {
                code: source.to_string(),
                label: "main.anv".to_string(),
                path: None,
            },
        },
        system: SystemPackages::default(),
        packages: HashMap::from([(root, PackageSourceInput::default())]),
        preloaded_modules: vec![],
        source_loader: &mut loader,
    };
    match build_air_packages(
        input,
        FrontendConfig::default(),
        &AirRootConfig::entry_main(),
    ) {
        Ok(output) => output.air,
        Err(AirBuildError::Diagnostic(output)) => {
            panic!(
                "source should lower successfully: {:?}",
                output.report.diagnostics()
            )
        }
        Err(error) => panic!("source AIR build failed: {error:?}"),
    }
}

fn named_function(program: &Program, name: &str) -> FunctionId {
    program
        .functions
        .iter()
        .position(|function| function.name.as_str() == name)
        .map_or_else(|| panic!("missing function {name}"), FunctionId::from_index)
}

fn first_lambda_function(program: &Program) -> FunctionId {
    program
        .functions
        .iter()
        .position(|function| matches!(function.kind, FunctionKind::Lambda(_)))
        .map(FunctionId::from_index)
        .expect("missing lambda function")
}

fn compile_verified_errors(program: &air::OwnedVerifiedProgram) -> Vec<VmCompileError> {
    VmCompiler::compile(program.as_verified()).expect_err("VM compile should fail")
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

fn count_function_errors(
    errors: &[VmCompileError],
    function: FunctionId,
    kind: VmCompileErrorKind,
) -> usize {
    errors
        .iter()
        .filter(|error| error.site == VmCompileErrorSite::Function(function) && error.kind == kind)
        .count()
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
