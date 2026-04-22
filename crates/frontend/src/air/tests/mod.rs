mod support;

pub(super) use support::{
    FunctionBuilder, ProgramBuilder, expect_verified, op_place, place, stmt_assign, stmt_eval,
    term_goto, term_if, term_return, term_return_void, term_switch_enum, term_unreachable,
    test_module,
};

pub(super) use super::{
    AggregateCtor, AggregateDecl, AggregateKind, BasicBlock, BlockId, Callee, ConstArena,
    ConstData, ConstId, ConstValue, EnumDecl, ExternDecl, ExternMember, ExternRep, ExternTypeDecl,
    FieldDecl, FieldId, Function, FunctionId, FunctionKind, LocalId, LocalKind, Module, ModuleId,
    Mutability, ParamRole, Place, Program, Projection, RValue, Signature, SignatureType,
    Terminator, TypeArena, TypeData, TypeId, VariantDecl, VariantId, VariantShape, verify,
};

mod structure;
mod verify_invalid;
mod verify_valid;
