use verify::{BadCall, BadFunction, BadPlace, BadReference, VerifyErrorKind as EK};

use super::*;
use crate::ast::Ident;

#[test]
fn entry_function_out_of_range() {
    let mut builder = ProgramBuilder::new();
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
    let mut builder = ProgramBuilder::new();
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
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_cond", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let bb_then = fb.push_block(term_return_void());
    let bb_else = fb.push_block(term_return_void());
    fb.push_block(term_if(op_place(p_n, int_ty), bb_then, bb_else));
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::IfCondMustBeBool(t)) if t == int_ty
    )));
}

#[test]
fn switch_not_enum() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_switch", module, FunctionKind::Normal, void_ty);
    let p_n = fb.push_param("n", int_ty, ParamRole::Normal);
    let bb_end = fb.push_block(term_return_void());
    fb.push_block(term_switch_enum(place(p_n, int_ty), vec![], Some(bb_end)));
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SwitchDiscriminantMustBeEnum(t)) if t == int_ty
    )));
}

#[test]
fn switch_bad_variant() {
    let mut builder = ProgramBuilder::new();
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
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::SwitchArmVariantMismatch { expected_enum, variant: VariantId(v) })
            if expected_enum == enum_id && v == 99
    )));
}

#[test]
fn return_type_mismatch() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let bool_ty = builder.alloc_type(TypeData::Bool);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_return", module, FunctionKind::Normal, int_ty);
    let local_b = fb.push_local(None, bool_ty, Mutability::Immutable, LocalKind::Return);
    fb.push_block(term_return(op_place(local_b, bool_ty)));
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::ReturnedTypeMismatch { expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn void_fn_returns_value() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("void_bad", module, FunctionKind::Normal, void_ty);
    let local_i = fb.push_local(None, int_ty, Mutability::Immutable, LocalKind::User);
    fb.push_block(term_return(op_place(local_i, int_ty)));
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::VoidFunctionMustReturnNone)
    )));
}

#[test]
fn nonvoid_fn_returns_none() {
    let mut builder = ProgramBuilder::new();
    let int_ty = builder.alloc_type(TypeData::Int);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("non_void_bad", module, FunctionKind::Normal, int_ty);
    fb.push_block(term_return_void());
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::NonVoidFunctionMustReturnValue(ret)) if ret == int_ty
    )));
}

#[test]
fn goto_bad_block() {
    let mut builder = ProgramBuilder::new();
    let void_ty = builder.alloc_type(TypeData::Void);
    let module = test_module(&mut builder);

    let mut fb = FunctionBuilder::new("bad_goto", module, FunctionKind::Normal, void_ty);
    fb.push_block(term_goto(BlockId::from_index(99)));
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidBlock(BlockId(id))) if id == 99
    )));
}

#[test]
fn fn_bad_type() {
    let mut builder = ProgramBuilder::new();
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
    let _fid = builder.alloc_function(func);
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidType(TypeId(id))) if id == 999
    )));
}

#[test]
fn aggregate_bad_field_type() {
    let mut builder = ProgramBuilder::new();
    let module = test_module(&mut builder);

    builder.alloc_aggregate(AggregateDecl {
        name: Ident::new("BadAgg"),
        module,
        kind: AggregateKind::Struct,
        fields: vec![FieldDecl {
            name: Ident::new("f"),
            ty: TypeId::from_index(999),
        }],
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
    let mut builder = ProgramBuilder::new();
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
fn call_arity_mismatch() {
    let mut builder = ProgramBuilder::new();
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
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

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
    let mut builder = ProgramBuilder::new();
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
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadCall(BadCall::ArgTypeMismatch { index: 1, expected, found })
            if expected == int_ty && found == bool_ty
    )));
}

#[test]
fn field_proj_non_aggregate() {
    let mut builder = ProgramBuilder::new();
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
    fb.add_statement(
        bb0,
        stmt_assign(bad_place, RValue::Use(op_place(local_int, int_ty))),
    );
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::FieldProjectionOnNonAggregate(t)) if t == int_ty
    )));
}

#[test]
fn tuple_field_out_of_range() {
    let mut builder = ProgramBuilder::new();
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
    fb.add_statement(
        bb0,
        stmt_assign(bad_place, RValue::Use(op_place(local_t, tuple_ty))),
    );
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::TupleFieldOutOfRange { ty, index: 99, len: 2 }) if ty == tuple_ty
    )));
}

#[test]
fn index_proj_non_indexable() {
    let mut builder = ProgramBuilder::new();
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
    fb.add_statement(
        bb0,
        stmt_assign(bad_place, RValue::Use(op_place(local_int, int_ty))),
    );
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadPlace(BadPlace::IndexProjectionOnNonIndexable(t)) if t == int_ty
    )));
}

#[test]
fn duplicate_switch_arm() {
    let mut builder = ProgramBuilder::new();
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
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadFunction(BadFunction::DuplicateSwitchArm(VariantId(v))) if v == 0
    )));
}

#[test]
fn closure_bad_fn() {
    let mut builder = ProgramBuilder::new();
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
    let _fid = builder.alloc_function(fb.finish());
    builder.set_entry(_fid);

    let errors = verify(&builder.finish()).unwrap_err();
    assert!(errors.iter().any(|e| matches!(
        e.kind,
        EK::BadReference(BadReference::InvalidFunction(FunctionId(id))) if id == 999
    )));
}
