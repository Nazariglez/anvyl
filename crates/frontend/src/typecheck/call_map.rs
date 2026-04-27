use std::collections::HashMap;

use super::{
    GenericArgs,
    decls::{CallableId, ModuleScope, NominalKey},
};
use crate::ast::{ExprId, Ident, Type};

pub(crate) type CallMap = HashMap<ExprId, CallTarget>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CallTarget {
    Direct {
        module: ModuleScope,
        name: Ident,
    },
    GenericDirect {
        module: ModuleScope,
        name: Ident,
        type_args: Vec<Type>,
        const_args: Vec<usize>,
    },
    ModuleFunction {
        module: crate::resolve::ModulePath,
        name: Ident,
    },
    Method {
        owner: NominalKey,
        name: Ident,
        type_args: Vec<Type>,
        const_args: Vec<usize>,
    },
    Extend {
        target: CallableId,
        receiver: Type,
        args: GenericArgs,
    },
    EnumVariant {
        enum_key: NominalKey,
        variant: Ident,
    },
}
