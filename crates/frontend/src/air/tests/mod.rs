mod support;

pub(super) use support::{
    FunctionBuilder, ProgramBuilder, body_from_block, empty_module, expect_verified, op_const,
    op_place, place, stmt_assign, stmt_eval, stmt_init, term_return, term_return_void,
    term_unreachable, test_module,
};

pub(super) use super::{
    AggregateCtor, AggregateDecl, AggregateKind, AirBlock, AirBody, AirEnumMatch, AirEnumMatchArm,
    AirIf, AirLoop, AirLoopId, AirOptionalMatch, AirStmt, AirTail, BlockId, CallArg, Callee,
    ConstArena, ConstData, ConstId, ConstValue, EnumDecl, EnumId, ExternDecl, ExternMember,
    ExternParamDecl, ExternReceiverDecl, ExternRep, ExternTypeDecl, FieldDecl, FieldId, Function,
    FunctionId, FunctionKind, Local, LocalId, LocalKind, MapOrder, ModuleId, Mutability, Operand,
    Param, ParamMode, ParamRole, ParamType, Place, Program, Projection, RValue, RawEnumValue,
    ReturnMode, Signature, SignatureType, TypeArena, TypeData, TypeId, VariantDecl, VariantId,
    VariantShape, verify,
};

mod structure;
mod verify_invalid;
mod verify_valid;
