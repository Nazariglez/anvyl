use anvyx_frontend::{
    air::{
        self, BasicBlock, Callee, ConstData, ConstValue, ExternDecl, ExternMember, ExternRep,
        ExternTypeDecl, Function, FunctionKind, Local, LocalKind, Mutability, Operand, Param,
        ParamRole, Place, Program, RValue, Signature, Statement, Terminator, TypeData,
    },
    ast::{BinaryOp, Ident},
};

use super::profile::{ProfileErrorKind, RustBackendProfile, RustBackendProfileError};

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
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: vec![BasicBlock {
            statements: vec![Statement::Init {
                local: tmp,
                value: RValue::Binary {
                    op: BinaryOp::Add,
                    lhs: Operand::Const(one),
                    rhs: Operand::Const(two),
                    ty: int,
                },
            }],
            terminator: Terminator::Return(Some(Operand::Place(place(tmp, int)))),
        }],
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
        signature: Signature::new(vec![], float),
        locals: vec![local(float, LocalKind::Temp)],
        body: vec![BasicBlock {
            statements: vec![Statement::Init {
                local: tmp,
                value: RValue::Cast {
                    value: Operand::Const(one),
                    target: float,
                },
            }],
            terminator: Terminator::Return(Some(Operand::Place(place(tmp, float)))),
        }],
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
        signature: Signature::new(vec![], int),
        locals: vec![],
        body: vec![BasicBlock {
            statements: vec![],
            terminator: Terminator::Return(Some(Operand::Const(one))),
        }],
    });
    let tmp = air::LocalId::from_index(0);
    let main = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], int),
        locals: vec![local(int, LocalKind::Temp)],
        body: vec![BasicBlock {
            statements: vec![Statement::Init {
                local: tmp,
                value: RValue::Call {
                    callee: Callee::Function(helper),
                    args: vec![],
                },
            }],
            terminator: Terminator::Return(Some(Operand::Place(place(tmp, int)))),
        }],
    });
    program.module_mut(module).functions.extend([helper, main]);

    check(program);
}

#[test]
fn profile_accepts_core_println_extern_call_shape() {
    let mut program = Program::default();
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let extern_id = runtime_extern(&mut program, "_println", vec![string], void);
    let message = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ok".into()),
    });
    let module = program.alloc_module(root_module());
    let main = program.alloc_function(void_function(
        module,
        "main",
        void,
        vec![Statement::Eval(RValue::Call {
            callee: Callee::Extern(extern_id),
            args: vec![Operand::Const(message)],
        })],
    ));
    program.module_mut(module).functions.push(main);

    check(program);
}

#[test]
fn profile_accepts_core_assert_extern_call_shape() {
    let mut program = Program::default();
    let bool_ty = program.alloc_type(TypeData::Bool);
    let string = program.alloc_type(TypeData::String);
    let void = program.alloc_type(TypeData::Void);
    let extern_id = runtime_extern(&mut program, "_assert", vec![bool_ty, string], void);
    let condition = program.const_arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let message = program.const_arena.alloc(ConstData {
        ty: string,
        value: ConstValue::String("ok".into()),
    });
    let module = program.alloc_module(root_module());
    let main = program.alloc_function(void_function(
        module,
        "main",
        void,
        vec![Statement::Eval(RValue::Call {
            callee: Callee::Extern(extern_id),
            args: vec![Operand::Const(condition), Operand::Const(message)],
        })],
    ));
    program.module_mut(module).functions.push(main);

    check(program);
}

#[test]
fn profile_rejects_branching_terminator() {
    let mut program = Program::default();
    let bool_ty = program.alloc_type(TypeData::Bool);
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let cond = program.const_arena.alloc(ConstData {
        ty: bool_ty,
        value: ConstValue::Bool(true),
    });
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: vec![
            BasicBlock {
                statements: vec![],
                terminator: Terminator::If {
                    cond: Operand::Const(cond),
                    then_bb: air::BlockId::from_index(1),
                    else_bb: air::BlockId::from_index(2),
                },
            },
            BasicBlock {
                statements: vec![],
                terminator: Terminator::Return(None),
            },
            BasicBlock {
                statements: vec![],
                terminator: Terminator::Return(None),
            },
        ],
    });
    program.module_mut(module).functions.push(func);

    expect_reject(program, ProfileErrorKind::UnsupportedTerminator);
}

#[test]
fn profile_rejects_unreachable_terminator() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(root_module());
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], void),
        locals: vec![],
        body: vec![BasicBlock {
            statements: vec![],
            terminator: Terminator::Unreachable,
        }],
    });
    program.module_mut(module).functions.push(func);

    expect_reject(program, ProfileErrorKind::UnsupportedTerminator);
}

#[test]
fn profile_rejects_deferred_types() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    program.alloc_type(TypeData::List(int));

    expect_reject(program, ProfileErrorKind::UnsupportedType);
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
        kind: FunctionKind::Method,
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("self")),
                ty: void,
                role: ParamRole::Receiver,
                local_id: self_local,
            }],
            void,
        ),
        locals: vec![local(void, LocalKind::Arg)],
        body: vec![BasicBlock {
            statements: vec![],
            terminator: Terminator::Return(None),
        }],
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
        signature: Signature::new(vec![], void),
        locals: vec![
            local(void, LocalKind::Capture),
            local(void, LocalKind::PatternBinding),
            local(void, LocalKind::Return),
        ],
        body: vec![BasicBlock {
            statements: vec![Statement::Eval(RValue::MakeClosure {
                func: air::FunctionId::from_index(0),
                captures: vec![],
                ty: void,
            })],
            terminator: Terminator::Return(None),
        }],
    });
    program.module_mut(module).functions.push(func);

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedLocalKind));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedRValue));
}

#[test]
fn profile_rejects_place_projections() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let tuple = program.alloc_type(TypeData::Tuple(vec![int]));
    let module = program.alloc_module(root_module());
    let tuple_local = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], int),
        locals: vec![local(tuple, LocalKind::User)],
        body: vec![BasicBlock {
            statements: vec![],
            terminator: Terminator::Return(Some(Operand::Place(Place {
                root: tuple_local,
                projection: vec![air::Projection::TupleField(0)],
                ty: int,
            }))),
        }],
    });
    program.module_mut(module).functions.push(func);

    let errors = profile_errors(program);
    assert!(has_error(
        &errors,
        ProfileErrorKind::UnsupportedPlaceProjection
    ));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedType));
}

#[test]
fn profile_rejects_closure_callees() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let closure_ty = program.alloc_type(TypeData::Function(air::SignatureType::new(vec![], void)));
    let module = program.alloc_module(root_module());
    let closure_local = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], void),
        locals: vec![local(closure_ty, LocalKind::User)],
        body: vec![BasicBlock {
            statements: vec![Statement::Eval(RValue::Call {
                callee: Callee::Closure(Operand::Place(place(closure_local, closure_ty))),
                args: vec![],
            })],
            terminator: Terminator::Return(None),
        }],
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
    let list = program.alloc_type(TypeData::List(int));
    let module = program.alloc_module(root_module());
    let list_local = air::LocalId::from_index(0);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], list),
        locals: vec![local(list, LocalKind::Temp)],
        body: vec![BasicBlock {
            statements: vec![Statement::Init {
                local: list_local,
                value: RValue::Aggregate {
                    kind: air::AggregateCtor::List,
                    fields: vec![],
                    ty: list,
                },
            }],
            terminator: Terminator::Return(Some(Operand::Place(place(list_local, list)))),
        }],
    });
    program.module_mut(module).functions.push(func);

    let errors = profile_errors(program);
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedRValue));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedType));
}

#[test]
fn profile_rejects_void_stringify_source() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let string = program.alloc_type(TypeData::String);
    let module = program.alloc_module(root_module());
    let value = air::LocalId::from_index(0);
    let tmp = air::LocalId::from_index(1);
    let func = program.alloc_function(Function {
        name: Ident::new("main"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], void),
        locals: vec![local(void, LocalKind::User), local(string, LocalKind::Temp)],
        body: vec![BasicBlock {
            statements: vec![Statement::Init {
                local: tmp,
                value: RValue::Stringify {
                    value: Operand::Place(place(value, void)),
                    source_ty: void,
                },
            }],
            terminator: Terminator::Return(None),
        }],
    });
    program.module_mut(module).functions.push(func);

    expect_reject(program, ProfileErrorKind::UnsupportedRValue);
}

#[test]
fn profile_rejects_core_runtime_extern_with_wrong_signature() {
    let mut program = Program::default();
    let int = program.alloc_type(TypeData::Int);
    let void = program.alloc_type(TypeData::Void);
    runtime_extern(&mut program, "_println", vec![int], void);

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
        vec![string],
        void,
        ExternMember::FreeFunction,
    );

    expect_reject(program, ProfileErrorKind::UnsupportedExtern);
}

#[test]
fn profile_rejects_non_free_extern_members() {
    let mut program = Program::default();
    let void = program.alloc_type(TypeData::Void);
    let module = program.alloc_module(runtime_module());
    let owner = program.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Host"),
        module,
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
    });
    program.module_mut(module).externs.push(id);

    let errors = profile_errors(program);
    assert!(has_error(
        &errors,
        ProfileErrorKind::UnsupportedExternMember
    ));
    assert!(has_error(&errors, ProfileErrorKind::UnsupportedExtern));
}

fn check(program: Program) {
    let verified = air::verify(&program).expect("AIR verify failed");
    RustBackendProfile::check(verified).expect("profile rejected AIR");
}

fn expect_reject(program: Program, kind: ProfileErrorKind) {
    let errors = profile_errors(program);
    assert!(has_error(&errors, kind), "missing expected profile error");
}

fn profile_errors(program: Program) -> Vec<RustBackendProfileError> {
    let verified = air::verify(&program).expect("AIR verify failed");
    RustBackendProfile::check(verified).expect_err("profile accepted invalid AIR")
}

fn has_error(errors: &[RustBackendProfileError], kind: ProfileErrorKind) -> bool {
    errors.iter().any(|error| error.kind == kind)
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

fn void_function(
    module: air::ModuleId,
    name: &str,
    return_type: air::TypeId,
    statements: Vec<Statement>,
) -> Function {
    Function {
        name: Ident::new(name),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], return_type),
        locals: vec![],
        body: vec![BasicBlock {
            statements,
            terminator: Terminator::Return(None),
        }],
    }
}

fn runtime_extern(
    program: &mut Program,
    name: &str,
    params: Vec<air::TypeId>,
    return_type: air::TypeId,
) -> air::ExternId {
    extern_in_module(
        program,
        &["core_runtime"],
        name,
        params,
        return_type,
        ExternMember::FreeFunction,
    )
}

fn extern_in_module(
    program: &mut Program,
    path: &[&str],
    name: &str,
    params: Vec<air::TypeId>,
    return_type: air::TypeId,
    member: ExternMember,
) -> air::ExternId {
    let module = program.alloc_module(air_module(path));
    let id = program.alloc_extern(ExternDecl {
        name: Ident::new(name),
        module,
        member,
        params,
        return_type,
    });
    program.module_mut(module).externs.push(id);
    id
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
