use std::collections::HashMap;

use super::{GenericArgs, Type, decls::CallableId};
use crate::{
    ast::{ExprId, Ident},
    externs::catalog::{
        ExternFieldRef, ExternFunctionId, ExternMethodRef, ExternOperatorRef, ExternStaticRef,
        ExternTypeId,
    },
};

pub(crate) type CallMap = HashMap<ExprId, CallTarget>;
pub(crate) type ExternUseMap = HashMap<ExprId, Vec<ExternUseTarget>>;
pub(crate) type MemberPathMap = HashMap<ExprId, MemberPathFact>;
pub(crate) type ArgumentProjectionMap = HashMap<(ExprId, usize), ArgumentProjectionFact>;

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
