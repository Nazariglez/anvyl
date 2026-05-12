use std::collections::HashMap;

use super::{
    ContractKey, GenericArgs, MethodMode, MethodReceiver, Type,
    decls::{CallableId, ExtendId},
};
use crate::{
    ast::{ExprId, Ident},
    externs::catalog::{
        ExternFieldRef, ExternFunctionId, ExternMethodRef, ExternOperatorRef, ExternStaticRef,
        ExternTypeId,
    },
    span::SourceSpan,
};

pub(crate) type CallMap = HashMap<ExprId, CallTarget>;
pub(crate) type ExternUseMap = HashMap<ExprId, Vec<ExternUseTarget>>;
pub(crate) type MemberPathMap = HashMap<ExprId, MemberPathFact>;
pub(crate) type ArgumentProjectionMap = HashMap<(ExprId, usize), ArgumentProjectionFact>;
pub(crate) type ContractWitnessMap = HashMap<WitnessId, ContractWitnessFact>;
pub(crate) type DynConversionMap = HashMap<ExprId, DynConversionFact>;
pub(crate) type DynCallMap = HashMap<ExprId, DynCallFact>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallTarget {
    pub(crate) id: CallableId,
    pub(crate) args: GenericArgs,
    pub(crate) form: CallForm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CallForm {
    Normal,
    QualifiedExtend { receiver: ExprId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MemberPathKind {
    Field,
    MethodReceiver,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MemberPathFact {
    pub(crate) expr_id: ExprId,
    pub(crate) kind: MemberPathKind,
    pub(crate) path: Vec<Ident>,
    pub(crate) origin_owner: Type,
    pub(crate) origin_member: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ArgumentProjectionFact {
    pub(crate) call_id: ExprId,
    pub(crate) arg_index: usize,
    pub(crate) path: Vec<Ident>,
    pub(crate) target_ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct WitnessId(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ContractWitnessFact {
    pub(crate) id: WitnessId,
    pub(crate) key: ContractWitnessKey,
    pub(crate) span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ContractWitnessKey {
    pub(crate) concrete_ty: Type,
    pub(crate) contract: ContractKey,
    pub(crate) slots: Vec<WitnessSlot>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct WitnessSlot {
    pub(crate) name: Ident,
    pub(crate) required_receiver: MethodReceiver,
    pub(crate) target: WitnessSlotTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum WitnessSlotTarget {
    Direct {
        callable: CallableId,
        owner_args: GenericArgs,
        receiver_mode: MethodMode,
    },
    Extend {
        extend: ExtendId,
        callable: CallableId,
        owner_args: GenericArgs,
        receiver_mode: MethodMode,
    },
    Extern {
        method: ExternMethodRef,
        receiver: anvyx_externs::ReceiverMode,
    },
    Promoted {
        path: Vec<Ident>,
        origin_owner: Type,
        origin_method: Ident,
        target: Box<WitnessSlotTarget>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DynConversionFact {
    pub(crate) expr_id: ExprId,
    pub(crate) witness: WitnessId,
    pub(crate) span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct DynCallFact {
    pub(crate) call_id: ExprId,
    pub(crate) receiver_id: ExprId,
    pub(crate) contract: ContractKey,
    pub(crate) method: Ident,
    pub(crate) arg_count: usize,
    pub(crate) requires_mutable: bool,
    pub(crate) span: SourceSpan,
}

impl CallTarget {
    pub(crate) fn new(id: CallableId, args: GenericArgs) -> Self {
        Self {
            id,
            args,
            form: CallForm::Normal,
        }
    }

    pub(crate) fn qualified_extend(id: CallableId, args: GenericArgs, receiver: ExprId) -> Self {
        Self {
            id,
            args,
            form: CallForm::QualifiedExtend { receiver },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExternUseTarget {
    Function(ExternFunctionId),
    FieldRead(ExternFieldRef),
    FieldWrite(ExternFieldRef),
    Method(ExternMethodRef),
    Static(ExternStaticRef),
    Init(ExternTypeId),
    UnaryOperator(ExternOperatorRef),
    BinaryOperator(ExternOperatorRef),
}
