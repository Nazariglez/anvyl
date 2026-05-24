use super::{super::verify::verify_structured_body, *};
use crate::ast::Ident;

#[test]
fn empty_program() {
    expect_verified(&Program::default());
}

#[test]
fn returns_verified_wrapper() {
    let program = Program::default();
    let verified = verify(&program).unwrap();
    assert_eq!(
        std::ptr::from_ref(verified.program()),
        std::ptr::from_ref(&program)
    );
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
fn structured_exhaustive_unit_enum_match() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);
    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Choice"),
        module,
        type_args: vec![],
        const_args: vec![],
        variants: vec![
            VariantDecl {
                name: Ident::new("A"),
                shape: VariantShape::Unit,
            },
            VariantDecl {
                name: Ident::new("B"),
                shape: VariantShape::Unit,
            },
        ],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));
    let mut fb = FunctionBuilder::new("match_enum", module, FunctionKind::Normal, void_ty);
    let discr = fb.push_param("value", enum_ty, ParamRole::Normal);
    fb.push_block(term_return_void());
    let func_id = builder.alloc_function(fb.finish());
    let arm = |variant| AirEnumMatchArm {
        variant,
        block: AirBlock {
            stmts: vec![],
            tail: AirTail::Return(None),
        },
    };
    let body = AirBody {
        block: AirBlock {
            stmts: vec![AirStmt::EnumMatch(AirEnumMatch {
                discr: place(discr, enum_ty),
                arms: vec![arm(VariantId::from_index(0)), arm(VariantId::from_index(1))],
                else_block: None,
            })],
            tail: AirTail::Unreachable,
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
        root: local_agg,
        projection: vec![Projection::Field(FieldId::from_index(0))],
        ty: int_ty,
    };
    let y_proj = Place {
        root: local_agg,
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
fn enum_switch() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.void_ty();
    let module = test_module(&mut builder);

    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Color"),
        module,
        type_args: vec![],
        const_args: vec![],
        variants: vec![
            VariantDecl {
                name: Ident::new("Red"),
                shape: VariantShape::Unit,
            },
            VariantDecl {
                name: Ident::new("Green"),
                shape: VariantShape::Unit,
            },
            VariantDecl {
                name: Ident::new("Blue"),
                shape: VariantShape::Unit,
            },
        ],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));

    let mut fb = FunctionBuilder::new("switch_color", module, FunctionKind::Normal, void_ty);
    let p_c = fb.push_param("c", enum_ty, ParamRole::Normal);
    let block = fb.push_block(term_return_void());
    fb.add_statement(
        block,
        AirStmt::EnumMatch(AirEnumMatch {
            discr: place(p_c, enum_ty),
            arms: vec![
                AirEnumMatchArm {
                    variant: VariantId::from_index(0),
                    block: AirBlock {
                        stmts: vec![],
                        tail: term_return_void(),
                    },
                },
                AirEnumMatchArm {
                    variant: VariantId::from_index(1),
                    block: AirBlock {
                        stmts: vec![],
                        tail: term_return_void(),
                    },
                },
            ],
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
        type_args: vec![],
        const_args: vec![],
        rep: ExternRep::Shared,
        has_init: false,
        fields: vec![],
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
        }],
        return_type: void_ty,
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
