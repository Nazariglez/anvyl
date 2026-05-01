use std::collections::HashMap;

use super::{GenericArgs, decls::CallableId};
use crate::{
    ast::ExprId,
    externs::catalog::{
        ExternFieldRef, ExternFunctionId, ExternMethodRef, ExternOperatorRef, ExternStaticRef,
        ExternTypeId,
    },
};

pub(crate) type CallMap = HashMap<ExprId, CallTarget>;
pub(crate) type ExternUseMap = HashMap<ExprId, Vec<ExternUseTarget>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallTarget {
    pub(crate) id: CallableId,
    pub(crate) args: GenericArgs,
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
