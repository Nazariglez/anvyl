mod support;

pub(super) use support::{
    FunctionBuilder, ProgramBuilder, empty_module, expect_verified, op_const, op_place, place,
    stmt_assign, stmt_eval, stmt_init, term_goto, term_if, term_return, term_return_void,
    term_switch_enum, term_unreachable, test_module,
};

pub(super) use super::{
    AggregateCtor, AggregateDecl, AggregateKind, BasicBlock, BlockId, Callee, ConstArena,
    ConstData, ConstId, ConstValue, EnumDecl, EnumId, ExternDecl, ExternMember, ExternRep,
    ExternTypeDecl, FieldDecl, FieldId, Function, FunctionId, FunctionKind, Local, LocalId,
    LocalKind, ModuleId, Mutability, Operand, Param, ParamRole, Place, Program, Projection, RValue,
    Signature, SignatureType, Terminator, TypeArena, TypeData, TypeId, VariantDecl, VariantId,
    VariantShape, verify,
};

mod structure;
mod verify_invalid;
mod verify_valid;
