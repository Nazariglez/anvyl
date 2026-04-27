use std::collections::HashMap;

use super::{GenericArgs, decls::CallableId};
use crate::ast::ExprId;

pub(crate) type CallMap = HashMap<ExprId, CallTarget>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CallTarget {
    Callable { id: CallableId, args: GenericArgs },
}
