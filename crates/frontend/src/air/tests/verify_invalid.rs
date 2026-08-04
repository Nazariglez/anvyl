use crate::air::{BadType, Program, TypeData, TypeId, VerifyErrorKind, verify};

#[test]
fn recursive_type_is_invalid() {
    let mut program = Program::default();
    let ty = program.alloc_type(TypeData::Optional(TypeId::from_index(999)));
    *program.type_arena.data_mut(ty) = TypeData::Optional(ty);

    let errors = verify(&program).unwrap_err();
    assert!(errors.iter().any(
        |error| matches!(error.kind, VerifyErrorKind::BadType(BadType::Recursive(id)) if id == ty)
    ));
}
