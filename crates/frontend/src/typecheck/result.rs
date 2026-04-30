use std::collections::HashMap;

use super::{ModuleScope, call_map::CallMap, decls::DeclarationIndex};
use crate::{
    ast::{ConstValue, ExprId, Ident, Type},
    externs::catalog::ExternCatalog,
    span::Span,
};

pub(crate) struct TypecheckResult {
    pub(crate) types: HashMap<ExprId, (Span, Type)>,
    pub(crate) calls: CallMap,
    pub(crate) decls: DeclarationIndex,
    pub(crate) externs: ExternCatalog,
    pub(crate) consts: HashMap<(ModuleScope, Ident), ConstValue>,
}

impl TypecheckResult {
    pub(crate) fn types(&self) -> impl Iterator<Item = (&ExprId, &(Span, Type))> {
        self.types.iter()
    }

    pub(crate) fn calls(&self) -> &CallMap {
        &self.calls
    }

    pub(crate) fn decls(&self) -> &DeclarationIndex {
        &self.decls
    }

    pub(crate) fn externs(&self) -> &ExternCatalog {
        &self.externs
    }

    pub(crate) fn consts(&self) -> &HashMap<(ModuleScope, Ident), ConstValue> {
        &self.consts
    }
}
