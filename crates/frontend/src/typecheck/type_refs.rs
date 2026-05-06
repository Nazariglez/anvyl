use std::collections::{HashMap, HashSet};

use crate::ast::{ConstParam, ConstParamId, Ident, TypeParam, TypeVarId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct GenericParamError {
    name: Ident,
}

impl GenericParamError {
    fn duplicate(name: Ident) -> Self {
        Self { name }
    }

    pub(crate) fn name(self) -> Ident {
        self.name
    }
}

pub(crate) fn validate_generic_params(
    type_params: &[TypeParam],
    const_params: &[ConstParam],
) -> Result<(), GenericParamError> {
    let mut types = HashSet::new();
    for param in type_params {
        if !types.insert(param.name) {
            return Err(GenericParamError::duplicate(param.name));
        }
    }

    let mut consts = HashSet::new();
    for param in const_params {
        if types.contains(&param.name) {
            return Err(GenericParamError::duplicate(param.name));
        }
        if !consts.insert(param.name) {
            return Err(GenericParamError::duplicate(param.name));
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct GenericTypeContext {
    type_params: HashMap<Ident, TypeVarId>,
    const_params: HashMap<Ident, ConstParamId>,
}

impl GenericTypeContext {
    pub(crate) fn try_from_params(
        type_params: &[TypeParam],
        const_params: &[ConstParam],
    ) -> Result<Self, GenericParamError> {
        validate_generic_params(type_params, const_params)?;
        let mut ctx = Self::default();
        ctx.extend_shadowing_unchecked(type_params, const_params);
        Ok(ctx)
    }

    pub(crate) fn try_with_shadowing_params(
        &self,
        type_params: &[TypeParam],
        const_params: &[ConstParam],
    ) -> Result<Self, GenericParamError> {
        validate_generic_params(type_params, const_params)?;
        let mut ctx = self.clone();
        ctx.extend_shadowing_unchecked(type_params, const_params);
        Ok(ctx)
    }

    fn extend_shadowing_unchecked(
        &mut self,
        type_params: &[TypeParam],
        const_params: &[ConstParam],
    ) {
        for param in type_params {
            self.const_params.remove(&param.name);
            self.type_params.insert(param.name, param.id);
        }
        for param in const_params {
            self.type_params.remove(&param.name);
            self.const_params.insert(param.name, param.id);
        }
    }

    pub(crate) fn type_param(&self, name: Ident) -> Option<TypeVarId> {
        self.type_params.get(&name).copied()
    }

    pub(crate) fn const_param(&self, name: Ident) -> Option<ConstParamId> {
        self.const_params.get(&name).copied()
    }

    pub(crate) fn has_type_param(&self, name: Ident) -> bool {
        self.type_params.contains_key(&name)
    }

    pub(crate) fn has_const_param(&self, name: Ident) -> bool {
        self.const_params.contains_key(&name)
    }

    pub(crate) fn type_param_name(&self, id: TypeVarId) -> Option<Ident> {
        self.type_params
            .iter()
            .find_map(|(name, binding)| (*binding == id).then_some(*name))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn type_param(name: &str, id: u32) -> TypeParam {
        TypeParam {
            name: ident(name),
            id: TypeVarId(id),
        }
    }

    fn const_param(name: &str, id: u32) -> ConstParam {
        ConstParam {
            name: ident(name),
            id: ConstParamId(id),
        }
    }

    #[test]
    fn shadow_type_with_type() {
        let owner = GenericTypeContext::try_from_params(&[type_param("T", 0)], &[]).unwrap();
        let inner = owner
            .try_with_shadowing_params(&[type_param("T", 1)], &[])
            .unwrap();

        assert_eq!(inner.type_param(ident("T")), Some(TypeVarId(1)));
        assert_eq!(inner.type_param_name(TypeVarId(0)), None);
    }

    #[test]
    fn shadow_type_with_const() {
        let owner = GenericTypeContext::try_from_params(&[type_param("T", 0)], &[]).unwrap();
        let inner = owner
            .try_with_shadowing_params(&[], &[const_param("T", 1)])
            .unwrap();

        assert_eq!(inner.type_param(ident("T")), None);
        assert_eq!(inner.const_param(ident("T")), Some(ConstParamId(1)));
    }

    #[test]
    fn shadow_const_with_type() {
        let owner = GenericTypeContext::try_from_params(&[], &[const_param("N", 0)]).unwrap();
        let inner = owner
            .try_with_shadowing_params(&[type_param("N", 1)], &[])
            .unwrap();

        assert_eq!(inner.const_param(ident("N")), None);
        assert_eq!(inner.type_param(ident("N")), Some(TypeVarId(1)));
    }

    #[test]
    fn shadow_const_with_const() {
        let owner = GenericTypeContext::try_from_params(&[], &[const_param("N", 0)]).unwrap();
        let inner = owner
            .try_with_shadowing_params(&[], &[const_param("N", 1)])
            .unwrap();

        assert_eq!(inner.const_param(ident("N")), Some(ConstParamId(1)));
    }
}
