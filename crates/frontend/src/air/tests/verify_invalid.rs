use verify::{
    BadCall, BadConst, BadFunction, BadModule, BadPlace, BadRValue, BadReference, BadStatement,
    BadType, ModuleItem, PrimitiveKind, VerifyError, VerifyErrorKind as EK,
};

use super::*;
use crate::ast::Ident;

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
fn fn_no_blocks() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let func = Function {
        name: Ident::new("empty"),
        module,
        kind: FunctionKind::Normal,
        signature: Signature::new(vec![], void_ty),
        locals: vec![],
        body: vec![],
    };
    let func_id = builder.alloc_function(func);
    builder.set_entry(func_id);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, EK::BadFunction(BadFunction::FunctionHasNoBlocks)))
    );
}

#[test]
fn if_not_bool() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_cond", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let bb_then = fb.push_block(term_return_void());
    let bb_else = fb.push_block(term_return_void());
    fb.push_block(term_if(op_place(p_n, int_ty), bb_then, bb_else));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::IfCondMustBeBool(t)) if t == int_ty
    )));
}

#[test]
fn switch_not_enum() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_switch", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let bb_end = fb.push_block(term_return_void());
    fb.push_block(term_switch_enum(place(p_n, int_ty), vec![], Some(bb_end)));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SwitchDiscriminantMustBeEnum(t)) if t == int_ty
    )));
}

#[test]
fn switch_bad_variant() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Color"),
        module,
        variants: vec![VariantDecl {
            name: Ident::new("Red"),
            shape: VariantShape::Unit,
        }],
    });
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));

    let mut fb = FunctionBuilder::new("wrong_variant", module, FunctionKind::Normal, void_ty);
    let p_c = fb.push_param("c", enum_ty, ParamRole::Normal);
    let bb_end = fb.push_block(term_return_void());
    fb.push_block(term_switch_enum(
        place(p_c, enum_ty),
        vec![(VariantId::from_index(99), bb_end)],
        None,
    ));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SwitchArmVariantMismatch { expected_enum, variant: VariantId(v) })
            if expected_enum == enum_id && v == 99
    )));
}

#[test]
fn switch_invalid_enum_type_does_not_panic() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let enum_id = EnumId::from_index(99);
    let enum_ty = builder.alloc_type(TypeData::Enum(enum_id));
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_switch_enum", module, FunctionKind::Normal, void_ty);
    let value = fb.push_param("value", enum_ty, ParamRole::Normal);
    let bb_end = fb.push_block(term_return_void());
    fb.push_block(term_switch_enum(
        place(value, enum_ty),
        vec![(VariantId::from_index(0), bb_end)],
        None,
    ));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidEnum(id)) if id == enum_id
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
fn goto_bad_block() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_goto", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_goto(BlockId::from_index(99)));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidBlock(BlockId(id))) if id == 99
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
        signature: Signature::new(vec![], TypeId::from_index(999)),
        locals: vec![],
        body: vec![BasicBlock {
            statements: vec![],
            terminator: Terminator::Return(None),
        }],
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
fn aggregate_bad_field_type() {
    let mut builder = ProgramBuilder::default();
    let module = test_module(&mut builder);

    builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("BadAgg"),
        module,
        kind: AggregateKind::Struct,
        fields: vec![field("f", TypeId::from_index(999))],
        cycle_capable: false,
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
        variants: vec![VariantDecl {
            name: Ident::new("V"),
            shape: VariantShape::Tuple(vec![TypeId::from_index(888)]),
        }],
    });

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(TypeId(id))) if id == 888
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
            args: vec![op_place(p_arg, bool_ty)],
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
fn call_arity_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let ext_id = builder.alloc_extern(ExternDecl {
        name: Ident::new("ext_add"),
        module,
        member: ExternMember::FreeFunction,
        params: vec![int_ty, int_ty],
        return_type: int_ty,
    });

    let mut fb = FunctionBuilder::new("arity_bad", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![op_place(p_n, int_ty)],
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
        params: vec![int_ty, int_ty],
        return_type: int_ty,
    });

    let mut fb = FunctionBuilder::new("arg_type_bad", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let p_b = fb.push_param("b", bool_ty, ParamRole::Normal);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::Call {
            callee: Callee::Extern(ext_id),
            args: vec![op_place(p_n, int_ty), op_place(p_b, bool_ty)],
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
fn field_proj_non_aggregate() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("field_on_int", module, FunctionKind::Normal, void_ty);
    let local_int = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    let bad_place = Place {
        root: local_int,
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
        root: local_t,
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
        root: local_int,
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
fn duplicate_switch_arm() {
    let mut builder = ProgramBuilder::default();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let enum_id = builder.alloc_enum(EnumDecl {
        name: Ident::new("Dup"),
        module,
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

    let mut fb = FunctionBuilder::new("dup_arm", module, FunctionKind::Normal, void_ty);
    let p_c = fb.push_param("c", enum_ty, ParamRole::Normal);
    let bb_end = fb.push_block(term_return_void());
    fb.push_block(term_switch_enum(
        place(p_c, enum_ty),
        vec![
            (VariantId::from_index(0), bb_end),
            (VariantId::from_index(0), bb_end),
        ],
        None,
    ));
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateSwitchArm(VariantId(v))) if v == 0
    )));
}

#[test]
fn closure_bad_fn() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);

    let sig_type = builder.alloc_type(TypeData::Function(SignatureType::new(vec![int_ty], int_ty)));

    let mut fb = FunctionBuilder::new("bad_closure", module, FunctionKind::Normal, int_ty);
    let bb0 = fb.push_block(term_return_void());
    fb.add_statement(
        bb0,
        stmt_eval(RValue::MakeClosure {
            func: FunctionId::from_index(999),
            captures: vec![],
            ty: sig_type,
        }),
    );
    let fid = builder.alloc_function(fb.finish());
    builder.set_entry(fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidFunction(FunctionId(id))) if id == 999
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
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadStatement(BadStatement::AssignImmutableLocal(local)) if local == imm)));
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
        root: list,
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
        signature: Signature::new(
            vec![Param {
                name: Some(Ident::new("p")),
                ty: int_ty,
                role: ParamRole::Normal,
                local_id: local,
            }],
            int_ty,
        ),
        locals: vec![Local {
            name: Some(Ident::new("p")),
            ty: bool_ty,
            mutability: Mutability::Immutable,
            kind: LocalKind::User,
        }],
        body: vec![BasicBlock {
            statements: vec![],
            terminator: term_return(op_place(local, bool_ty)),
        }],
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
        fields: vec![],
        cycle_capable: false,
    });
    let missing_enum = builder.alloc_enum_raw(EnumDecl {
        name: Ident::new("MissingEnum"),
        module: m0,
        variants: vec![],
    });
    let missing_ext_ty = builder.alloc_extern_type_raw(ExternTypeDecl {
        name: Ident::new("MissingExtType"),
        module: m0,
        rep: ExternRep::Shared,
        has_init: false,
        fields: vec![],
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
    });

    let mut fb = FunctionBuilder::new("wrong", m1, FunctionKind::Normal, void_ty);
    fb.push_block(term_return_void());
    let wrong = builder.alloc_function_raw(fb.finish());
    let module = builder.module_mut(m0);
    module.functions.push(wrong);
    module.functions.push(wrong);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Function(id))) if id == missing)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Aggregate(id))) if id == missing_agg)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Enum(id))) if id == missing_enum)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::ExternType(id))) if id == missing_ext_ty)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::MissingItem(ModuleItem::Extern(id))) if id == missing_ext)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::ItemWrongModule { item: ModuleItem::Function(id), expected, found }) if id == wrong && expected == m0 && found == m1)));
    assert!(errors.iter().any(|e| matches!(e.kind, EK::BadModule(BadModule::DuplicateItem(ModuleItem::Function(id))) if id == wrong)));
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
        stmt_eval(RValue::ToString {
            value: op_place(local_bool, bool_ty),
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
        fields: vec![field("z", int_ty), field("a", bool_ty)],
        cycle_capable: false,
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
        variants: vec![VariantDecl {
            name: Ident::new("Hit"),
            shape: VariantShape::Struct(vec![field("z", int_ty), field("a", bool_ty)]),
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
        fields: vec![field("value", int_ty)],
        cycle_capable: true,
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
                root: local,
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
        fields: vec![field("a", int_ty), field("b", int_ty)],
        cycle_capable: false,
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
fn aggregate_ctor_result_and_kind_mismatch() {
    let mut builder = ProgramBuilder::default();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);
    let aggregate = builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("Node"),
        module,
        kind: AggregateKind::DataRef,
        fields: vec![field("value", int_ty)],
        cycle_capable: true,
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
        variants: vec![
            VariantDecl {
                name: Ident::new("Unit"),
                shape: VariantShape::Unit,
            },
            VariantDecl {
                name: Ident::new("Tuple"),
                shape: VariantShape::Tuple(vec![int_ty]),
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
        fields: vec![field("value", int_ty)],
        cycle_capable: false,
    });
    let dataref_ty = builder.alloc_type(TypeData::DataRef(aggregate));

    let errors = verify_void_entry(builder, "bad_dataref_field", module, void_ty, |fb, bb0| {
        let local = fb.push_local(None, dataref_ty, Mutability::Immutable, LocalKind::User);
        let bad_kind = Place {
            root: local,
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
        fields: vec![field("value", int_ty)],
        cycle_capable: true,
    });
    let dataref_ty = builder.alloc_type(TypeData::DataRef(dataref));

    let errors = verify_void_entry(builder, "dataref_oob", module, void_ty, |fb, bb0| {
        let local = fb.push_local(None, dataref_ty, Mutability::Immutable, LocalKind::User);
        let bad_place = Place {
            root: local,
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
