use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, CallArg, Callee, ExternDecl, ExternMember, ExternParamDecl, ExternReceiverDecl,
        ExternRep, ExternTypeDecl, Function, FunctionKind, LocalKind, Mutability, Operand,
        ParamMode, Program, RValue, Signature, TypeData,
    },
    ast::Ident,
};

use super::{
    compile::{VmCompileError, VmCompileErrorKind, VmCompiler},
    runtime::{ExternDispatcher, NoExterns, unsupported_callback},
};
use crate::test_support::{local, param, place, root_module, structured_body};

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

fn compile_errors(program: &Program) -> Vec<VmCompileError> {
    let verified = air::verify(program).expect("AIR verify failed");
    VmCompiler::compile(verified).expect_err("VM compile should fail")
}

fn has_compile_error(
    errors: &[VmCompileError],
    function: air::FunctionId,
    kind: VmCompileErrorKind,
) -> bool {
    errors
        .iter()
        .any(|error| error.function == function && error.kind == kind)
}
