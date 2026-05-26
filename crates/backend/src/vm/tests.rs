use air::AirStmt as Statement;
use anvyx_frontend::{
    air::{
        self, AirBody, CallArg, Callee, Function, FunctionKind, Local, LocalKind, Mutability,
        Operand, Param, ParamMode, ParamRole, Place, Program, Projection, RValue, Signature,
        TypeData,
    },
    ast::Ident,
};

fn structured_body(stmts: Vec<Statement>, tail: air::AirTail) -> AirBody {
    AirBody {
        block: air::AirBlock { stmts, tail },
    }
}

use super::{
    compile::{VmCompileError, VmCompileErrorKind, VmCompiler},
    runtime::CallFrame,
    vir::{VirCall, VirCallArg, VirParam},
};

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
    assert_eq!(vir.functions[0].params[0].mode, ParamMode::Value);
    assert_eq!(vir.functions[0].params[1].mode, ParamMode::SharedBorrow);
    assert!(matches!(
        vir.functions[1].calls[0].args[1],
        VirCallArg::SharedBorrow(_)
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
    let projected = projected_place(caller_local, int);
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
    assert_eq!(vir.functions[0].params[0].mode, ParamMode::MutBorrow);
    assert_eq!(vir.functions[1].calls[0].callee, Callee::Function(callee));
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
        body: AirBody {
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

#[test]
fn call_frame_binds_shared_string_consts() {
    let string = air::TypeId::from_index(0);
    let konst = air::ConstId::from_index(0);
    let params = vec![VirParam {
        local: air::LocalId::from_index(0),
        ty: string,
        mode: ParamMode::SharedBorrow,
    }];
    let call = VirCall {
        callee: Callee::Function(air::FunctionId::from_index(0)),
        args: vec![VirCallArg::SharedStringConst(konst)],
    };

    let frame = CallFrame::bind(&params, &call).expect("call frame should bind");
    assert_eq!(frame.bindings, vec![VirCallArg::SharedStringConst(konst)]);
}

#[test]
fn call_frame_binds_mut_borrows_to_caller_places() {
    let int = air::TypeId::from_index(0);
    let place = projected_place(air::LocalId::from_index(0), int);
    let params = vec![VirParam {
        local: air::LocalId::from_index(0),
        ty: int,
        mode: ParamMode::MutBorrow,
    }];
    let call = VirCall {
        callee: Callee::Function(air::FunctionId::from_index(0)),
        args: vec![VirCallArg::MutBorrow(place.clone())],
    };

    let frame = CallFrame::bind(&params, &call).expect("call frame should bind");
    assert_eq!(frame.bindings, vec![VirCallArg::MutBorrow(place.clone())]);
    assert_eq!(frame.mut_borrows().collect::<Vec<_>>(), vec![&place]);
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

fn param(name: &str, ty: air::TypeId, mode: ParamMode, local_id: air::LocalId) -> Param {
    Param {
        name: Some(Ident::new(name)),
        ty,
        mode,
        role: ParamRole::Normal,
        local_id,
    }
}

fn local(ty: air::TypeId, mutability: Mutability, kind: LocalKind) -> Local {
    Local {
        name: None,
        ty,
        mutability,
        kind,
    }
}

fn place(local: air::LocalId, ty: air::TypeId) -> Place {
    Place {
        root: local,
        projection: vec![],
        ty,
    }
}

fn projected_place(local: air::LocalId, ty: air::TypeId) -> Place {
    Place {
        root: local,
        projection: vec![Projection::TupleField(0)],
        ty,
    }
}

fn root_module() -> air::Module {
    air::Module {
        path: vec![],
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    }
}
