use super::*;
use crate::ast::Ident;

#[test]
fn empty_program() {
    expect_verified(&Program::default());
}

#[test]
fn returns_verified_wrapper() {
    let program = Program::default();
    let verified = verify(&program).unwrap();
    assert!(std::ptr::eq(verified.program(), &program));
}

#[test]
fn fn_return() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("ret42", module, FunctionKind::Normal, int_ty);
    let local_ret = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Return);
    let c42 = builder.alloc_const(ConstData {
        ty: int_ty,
        value: ConstValue::Int(42),
    });
    fb.push_block(term_return(op_place(local_ret, int_ty)));
    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    let program = builder.finish();
    expect_verified(&program);
    assert_eq!(program.entry(), Some(func_id));
    let _ = c42;
}

#[test]
fn fn_params() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("add", module, FunctionKind::Normal, int_ty);
    let p_a = fb.push_param("a", int_ty, ParamRole::Normal);
    let p_b = fb.push_param("b", int_ty, ParamRole::Normal);
    let _ret = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Return);
    fb.push_block(term_return(op_place(p_a, int_ty)));
    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    expect_verified(&builder.finish());
    let _ = (p_a, p_b, func_id);
}

#[test]
fn fn_aggregate() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let agg_id = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Point"),
        module,
        kind: AggregateKind::Struct,
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
        stmt_assign(x_proj.clone(), RValue::Use(op_place(p_x, int_ty))),
    );
    fb.add_statement(
        block0,
        stmt_assign(y_proj.clone(), RValue::Use(op_place(p_y, int_ty))),
    );
    fb.add_statement(
        block0,
        stmt_assign(
            place(local_agg, agg_ty),
            RValue::Aggregate {
                kind: AggregateCtor::Struct(agg_id),
                fields: vec![op_place(p_x, int_ty), op_place(p_y, int_ty)],
                ty: agg_ty,
            },
        ),
    );
    fb.add_statement(block0, stmt_eval(RValue::Use(op_place(local_agg, agg_ty))));
    fb.add_statement(
        block0,
        stmt_assign(
            place(local_agg, agg_ty),
            RValue::Use(op_place(local_agg, agg_ty)),
        ),
    );

    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);
    expect_verified(&builder.finish());
}

#[test]
fn enum_switch() {
    let mut builder = ProgramBuilder::new();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Color"),
        module,
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
    let bb_then = fb.push_block(term_return_void());
    let bb_else = fb.push_block(term_return_void());
    fb.push_block(term_switch_enum(
        place(p_c, enum_ty),
        vec![
            (VariantId::from_index(0), bb_then),
            (VariantId::from_index(1), bb_then),
        ],
        Some(bb_else),
    ));

    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);
    expect_verified(&builder.finish());
}

#[test]
fn extern_call() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let _ext_ty_id = builder.alloc_extern_type(ExternTypeDecl {
        name: Ident::new("Console"),
        module,
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
        params: vec![int_ty],
        return_type: void_ty,
    });

    let mut fb = FunctionBuilder::new("call_ext", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![op_place(p_n, int_ty)],
        }),
    );

    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);
    expect_verified(&builder.finish());
}

#[test]
fn if_bool() {
    let mut builder = ProgramBuilder::new();
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("branch", module, FunctionKind::Normal, void_ty);
    let p_cond = fb.push_param("cond", bool_ty, ParamRole::Normal);
    let bb_then = fb.push_block(term_return_void());
    let bb_else = fb.push_block(term_return_void());
    fb.push_block(term_if(op_place(p_cond, bool_ty), bb_then, bb_else));

    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);
    expect_verified(&builder.finish());
}

#[test]
fn multi_block_goto() {
    let mut builder = ProgramBuilder::new();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("multi_block", module, FunctionKind::Normal, void_ty);
    let bb_end = fb.push_block(term_return_void());
    let bb_mid = fb.push_block(term_goto(bb_end));
    let _bb_start = fb.push_block(term_goto(bb_mid));

    let func_id = builder.alloc_function(fb.finish());
    builder.set_entry(func_id);

    let program = builder.finish();
    expect_verified(&program);
    assert_eq!(program.function(func_id).body.len(), 3);
}

#[test]
fn unreachable_fn() {
    let mut builder = ProgramBuilder::new();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("unreachable_fn", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_unreachable());

    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);
    expect_verified(&builder.finish());
}

#[test]
fn tuple_and_list() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let tuple_ty = builder.alloc_type(TypeData::Tuple(vec![int_ty, bool_ty]));
    let _list_ty = builder.alloc_type(TypeData::List(int_ty));
    let _map_ty = builder.alloc_type(TypeData::Map {
        key: int_ty,
        value: bool_ty,
    });

    let mut fb = FunctionBuilder::new(
        "types_ok",
        ModuleId::from_index(0),
        FunctionKind::Normal,
        int_ty,
    );
    let _local_t = fb.push_local(None, tuple_ty, Mutability::Immutable, LocalKind::User);
    let _local_l = fb.push_local(None, _list_ty, Mutability::Mutable, LocalKind::User);
    let local_ret = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Return);
    fb.push_block(term_return(op_place(local_ret, int_ty)));

    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);
    expect_verified(&builder.finish());
}

#[test]
fn closure_make_call() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);

    // inner closure body
    let mut inner_fb = FunctionBuilder::new("inner", module, FunctionKind::Closure, int_ty);
    let _p_e = inner_fb.push_param("e", int_ty, ParamRole::CaptureEnv);
    let ret_local = inner_fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Return);
    inner_fb.push_block(term_return(op_place(ret_local, int_ty)));
    let inner_id = builder.alloc_function(inner_fb.finish());

    let sig_type = builder.alloc_type(TypeData::Function(SignatureType::new(vec![int_ty], int_ty)));
    let _fn_ty = builder.alloc_type(TypeData::Function(SignatureType::new(
        vec![sig_type, int_ty],
        int_ty,
    )));

    // outer function: calls closure, makes closure
    let mut outer_fb = FunctionBuilder::new("outer", module, FunctionKind::Normal, int_ty);
    let p_closure = outer_fb.push_param("f", sig_type, ParamRole::Normal);
    let p_arg = outer_fb.push_param("arg", int_ty, ParamRole::Normal);
    let call_result = outer_fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::Temp);

    let bb0 = outer_fb.push_block(Terminator::Return(Some(op_place(call_result, int_ty))));
    outer_fb.add_statement(
        bb0,
        stmt_assign(
            place(call_result, int_ty),
            RValue::Call {
                callee: Callee::Closure(op_place(p_closure, sig_type)),
                args: vec![op_place(p_arg, int_ty)],
            },
        ),
    );
    outer_fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeClosure {
            func: inner_id,
            captures: vec![op_place(p_arg, int_ty)],
            ty: sig_type,
        }),
    );

    let outer_id = builder.alloc_function(outer_fb.finish());
    builder.set_entry(outer_id);

    expect_verified(&builder.finish());
    let _ = (inner_id, outer_id);
}

#[test]
fn multi_module() {
    let mut builder = ProgramBuilder::new();
    let void_ty = builder.alloc_type(TypeData::Void);

    let m0 = builder.alloc_module(Module {
        path: vec![Ident::new("a")],
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    });
    let m1 = builder.alloc_module(Module {
        path: vec![Ident::new("b")],
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    });

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
    let _ = f1;
}
