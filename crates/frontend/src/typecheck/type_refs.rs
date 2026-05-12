use std::collections::{HashMap, HashSet};

use super::{
    const_term::ConstTerm,
    decls::{
        ContractKey, DeclarationIndex, ModuleScope, NominalKey, TypeAliasDef, TypeAliasKey,
        TypeBinding, nominal_type_with_args,
    },
    generic::{GenericArgs, GenericParams, substitute},
    type_ops::bare_type_name,
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstParam, ConstParamId, ContractRef, FuncParam, GenericArg, Ident,
        Type, TypeParam, TypeVarId,
    },
    span::Span,
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeRefError {
    Unknown {
        qualifier: Option<Ident>,
        name: Ident,
    },
    GenericArity {
        expected: usize,
        found: usize,
    },
    GenericArgKindMismatch {
        expected: &'static str,
    },
    AliasCycle {
        name: Ident,
    },
    ContractAsType {
        name: Ident,
    },
    UnknownContract {
        qualifier: Option<Ident>,
        name: Ident,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeRefWarning {
    pub(crate) name: Ident,
    pub(crate) reason: Option<String>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FinalizedTypeRef {
    pub(crate) ty: Type,
    pub(crate) warnings: Vec<TypeRefWarning>,
}

pub(crate) type LocalTypeAliasKey = Span;

#[derive(Clone)]
pub(crate) struct LocalTypeAlias {
    pub(crate) key: LocalTypeAliasKey,
    pub(crate) def: TypeAliasDef,
    pub(crate) visible_depth: usize,
}

#[derive(Clone, Default)]
pub(crate) struct LocalTypeScopes {
    scopes: Vec<HashMap<Ident, LocalTypeAlias>>,
}

impl LocalTypeScopes {
    pub(crate) fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn depth(&self) -> usize {
        self.scopes.len()
    }

    pub(crate) fn insert(&mut self, alias: LocalTypeAlias) -> bool {
        let Some(scope) = self.scopes.last_mut() else {
            return false;
        };
        match scope.entry(alias.def.name) {
            std::collections::hash_map::Entry::Occupied(_) => false,
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(alias);
                true
            }
        }
    }

    pub(crate) fn visible(&self, name: Ident, depth: Option<usize>) -> Option<&LocalTypeAlias> {
        let depth = depth.unwrap_or(self.scopes.len()).min(self.scopes.len());
        self.scopes[..depth]
            .iter()
            .rev()
            .find_map(|scope| scope.get(&name))
    }

    pub(crate) fn by_key(&self, key: LocalTypeAliasKey) -> Option<&LocalTypeAlias> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.values())
            .find(|alias| alias.key == key)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AliasExpansionKey {
    Module(TypeAliasKey),
    Local(LocalTypeAliasKey),
}

struct AliasRef<'a> {
    key: AliasExpansionKey,
    def: &'a TypeAliasDef,
    local_depth: Option<usize>,
}

pub(crate) struct TypeRefResolver<'a> {
    decls: &'a DeclarationIndex,
    local_types: Option<&'a LocalTypeScopes>,
}

struct FinalizeState {
    local_depth: Option<usize>,
    stack: Vec<AliasExpansionKey>,
    site: Option<Span>,
    warnings: Vec<TypeRefWarning>,
}

impl FinalizeState {
    fn new(site: Option<Span>) -> Self {
        Self {
            local_depth: None,
            stack: vec![],
            site,
            warnings: vec![],
        }
    }

    fn alias_target(alias: &AliasRef<'_>, site: Option<Span>) -> Self {
        Self {
            local_depth: alias.local_depth,
            stack: vec![alias.key.clone()],
            site,
            warnings: vec![],
        }
    }

    fn warn_deprecated_alias(&mut self, alias: &AliasRef<'_>, use_name: Ident) {
        if alias.def.policy.has_deprecated()
            && let Some(span) = self.site
        {
            self.warnings.push(TypeRefWarning {
                name: use_name,
                reason: alias.def.policy.deprecated_reason().map(str::to_string),
                span,
            });
        }
    }

    fn finish(self, ty: Type) -> FinalizedTypeRef {
        FinalizedTypeRef {
            ty,
            warnings: self.warnings,
        }
    }
}

impl<'a> TypeRefResolver<'a> {
    pub(crate) fn module_only(decls: &'a DeclarationIndex) -> Self {
        Self {
            decls,
            local_types: None,
        }
    }

    pub(crate) fn with_local_types(
        decls: &'a DeclarationIndex,
        local_types: &'a LocalTypeScopes,
    ) -> Self {
        Self {
            decls,
            local_types: Some(local_types),
        }
    }

    pub(crate) fn finalize(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        ty: &Type,
    ) -> Result<Type, TypeRefError> {
        Ok(self.finalize_at(module, generics, ty, None)?.ty)
    }

    pub(crate) fn finalize_at(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        ty: &Type,
        site: Option<Span>,
    ) -> Result<FinalizedTypeRef, TypeRefError> {
        let mut state = FinalizeState::new(site);
        let ty = self.finalize_inner(module, generics, ty, &mut state)?;
        Ok(state.finish(ty))
    }

    fn finalize_inner(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        ty: &Type,
        state: &mut FinalizeState,
    ) -> Result<Type, TypeRefError> {
        match ty {
            Type::UnresolvedName(name) => {
                self.finalize_unresolved_name(module, generics, *name, state)
            }
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => self.finalize_unresolved_nominal(
                module,
                generics,
                *qualifier,
                *name,
                generic_args,
                state,
            ),
            Type::Func { params, ret } => Ok(Type::Func {
                params: params
                    .iter()
                    .map(|param| {
                        Ok(FuncParam::new(
                            self.finalize_inner(module, generics, &param.ty, state)?,
                            param.mutable,
                            param.cast_accept,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
                ret: Box::new(self.finalize_inner(module, generics, ret, state)?),
            }),
            Type::Dyn(contract) => {
                let key = self.resolve_contract_ref(module, contract)?;
                Ok(Type::Dyn(canonical_contract_ref(&key)))
            }
            Type::Tuple(elems) => elems
                .iter()
                .map(|ty| self.finalize_inner(module, generics, ty, state))
                .collect::<Result<Vec<_>, _>>()
                .map(Type::Tuple),
            Type::Nominal(nominal) => {
                let type_args = nominal
                    .type_args
                    .iter()
                    .map(|ty| self.finalize_inner(module, generics, ty, state))
                    .collect::<Result<Vec<_>, _>>()?;
                let const_args = nominal
                    .const_args
                    .iter()
                    .map(|arg| finalize_const_arg(generics, arg))
                    .collect::<Result<Vec<_>, _>>()?;
                if nominal.origin.is_none()
                    && let Some(key) = self
                        .decls
                        .resolve_visible_nominal_key(module, None, nominal.name)
                        .filter(|key| key.kind == nominal.kind)
                {
                    return Ok(nominal_type_with_args(&key, &type_args, &const_args));
                }
                Ok(Type::nominal_with_origin(
                    nominal.kind,
                    nominal.name,
                    type_args,
                    const_args,
                    nominal.origin.clone(),
                ))
            }
            Type::List { elem } => Ok(Type::List {
                elem: Box::new(self.finalize_inner(module, generics, elem, state)?),
            }),
            Type::Slice { elem } => Ok(Type::Slice {
                elem: Box::new(self.finalize_inner(module, generics, elem, state)?),
            }),
            Type::Array { elem, len } => Ok(Type::Array {
                elem: Box::new(self.finalize_inner(module, generics, elem, state)?),
                len: finalize_array_len(generics, *len)?,
            }),
            Type::Map { key, value } => Ok(Type::Map {
                key: Box::new(self.finalize_inner(module, generics, key, state)?),
                value: Box::new(self.finalize_inner(module, generics, value, state)?),
            }),
            Type::Infer
            | Type::InferReturn
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_) => Ok(ty.clone()),
        }
    }

    fn finalize_unresolved_name(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        name: Ident,
        state: &mut FinalizeState,
    ) -> Result<Type, TypeRefError> {
        if let Some(id) = generics.type_param(name) {
            return Ok(Type::Var(id));
        }
        if generics.has_const_param(name) {
            return Err(TypeRefError::Unknown {
                qualifier: None,
                name,
            });
        }
        if let Some(alias) = self.local_alias(name, state.local_depth) {
            let alias_ref = Self::local_alias_ref(alias);
            return self.expand_alias_ref(module, generics, &alias_ref, &[], state, name);
        }
        let binding = self
            .decls
            .resolve_visible_type_binding(module, None, name)
            .ok_or(TypeRefError::Unknown {
                qualifier: None,
                name,
            })?;
        self.finalize_binding(module, generics, binding, &[], state, name)
    }

    fn finalize_unresolved_nominal(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
        state: &mut FinalizeState,
    ) -> Result<Type, TypeRefError> {
        if qualifier.is_none() && generic_args.is_empty() {
            if let Some(id) = generics.type_param(name) {
                return Ok(Type::Var(id));
            }
            if generics.has_const_param(name) {
                return Err(TypeRefError::Unknown {
                    qualifier: None,
                    name,
                });
            }
        }
        if qualifier.is_none()
            && let Some(alias) = self.local_alias(name, state.local_depth)
        {
            let alias_ref = Self::local_alias_ref(alias);
            return self.expand_alias_ref(module, generics, &alias_ref, generic_args, state, name);
        }
        let binding = self
            .decls
            .resolve_visible_type_binding(module, qualifier, name)
            .ok_or(TypeRefError::Unknown { qualifier, name })?;
        self.finalize_binding(module, generics, binding, generic_args, state, name)
    }

    fn local_alias(&self, name: Ident, depth: Option<usize>) -> Option<&LocalTypeAlias> {
        self.local_types?.visible(name, depth)
    }

    fn local_alias_ref(alias: &LocalTypeAlias) -> AliasRef<'_> {
        AliasRef {
            key: AliasExpansionKey::Local(alias.key),
            def: &alias.def,
            local_depth: Some(alias.visible_depth),
        }
    }

    fn module_alias_ref(&self, key: &TypeAliasKey) -> Result<AliasRef<'a>, TypeRefError> {
        let schema = self.decls.type_alias(key).ok_or(TypeRefError::Unknown {
            qualifier: None,
            name: key.name,
        })?;
        Ok(AliasRef {
            key: AliasExpansionKey::Module(key.clone()),
            def: &schema.def,
            local_depth: None,
        })
    }

    pub(crate) fn finalize_type_binding_at(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        binding: TypeBinding,
        args: &[GenericArg],
        site: Option<Span>,
        use_name: Ident,
    ) -> Result<FinalizedTypeRef, TypeRefError> {
        let mut state = FinalizeState::new(site);
        let ty = self.finalize_binding(module, generics, binding, args, &mut state, use_name)?;
        Ok(state.finish(ty))
    }

    pub(crate) fn finalize_local_alias_target_at(
        &self,
        alias: &LocalTypeAlias,
        site: Option<Span>,
        use_name: Ident,
    ) -> Result<FinalizedTypeRef, TypeRefError> {
        let alias_ref = Self::local_alias_ref(alias);
        self.finalize_alias_ref_target_at(&alias_ref, site, use_name)
    }

    pub(crate) fn finalize_module_alias_target_at(
        &self,
        key: &TypeAliasKey,
        site: Option<Span>,
        use_name: Ident,
    ) -> Result<FinalizedTypeRef, TypeRefError> {
        let alias_ref = self.module_alias_ref(key)?;
        self.finalize_alias_ref_target_at(&alias_ref, site, use_name)
    }

    fn finalize_alias_ref_target_at(
        &self,
        alias: &AliasRef<'_>,
        site: Option<Span>,
        use_name: Ident,
    ) -> Result<FinalizedTypeRef, TypeRefError> {
        let mut state = FinalizeState::alias_target(alias, site);
        state.warn_deprecated_alias(alias, use_name);
        state.site = None;
        let ty = self.finalize_inner(
            &alias.def.module,
            &alias.def.generic_context,
            &alias.def.aliased,
            &mut state,
        )?;
        Ok(state.finish(ty))
    }

    fn finalize_binding(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        binding: TypeBinding,
        args: &[GenericArg],
        state: &mut FinalizeState,
        use_name: Ident,
    ) -> Result<Type, TypeRefError> {
        match binding {
            TypeBinding::Nominal(key) => self.finalize_nominal(module, generics, &key, args, state),
            TypeBinding::Alias(key) => {
                let alias_ref = self.module_alias_ref(&key)?;
                self.expand_alias_ref(module, generics, &alias_ref, args, state, use_name)
            }
            TypeBinding::Contract(_) => Err(TypeRefError::ContractAsType { name: use_name }),
        }
    }

    fn expand_alias_ref(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        alias: &AliasRef<'_>,
        args: &[GenericArg],
        state: &mut FinalizeState,
        use_name: Ident,
    ) -> Result<Type, TypeRefError> {
        if state.stack.contains(&alias.key) {
            return Err(TypeRefError::AliasCycle {
                name: alias.def.name,
            });
        }
        state.warn_deprecated_alias(alias, use_name);
        let args = self.finalize_decl_generic_args_inner(
            module,
            generics,
            &alias.def.generics,
            args,
            state,
        )?;
        let (type_subst, const_subst) = alias.def.generics.substitutions(&args);
        let aliased = substitute(&alias.def.aliased, &type_subst, &const_subst);

        let outer_depth = state.local_depth;
        let outer_site = state.site;
        state.local_depth = alias.local_depth;
        state.site = None;
        state.stack.push(alias.key.clone());
        let result = self.finalize_inner(
            &alias.def.module,
            &alias.def.generic_context,
            &aliased,
            state,
        );
        state.stack.pop();
        state.site = outer_site;
        state.local_depth = outer_depth;
        result
    }

    pub(crate) fn resolve_contract_ref(
        &self,
        module: &ModuleScope,
        contract: &ContractRef,
    ) -> Result<ContractKey, TypeRefError> {
        let ContractRef::Named {
            qualifier,
            name,
            origin,
        } = contract
        else {
            return Err(TypeRefError::UnknownContract {
                qualifier: None,
                name: Ident::new(contract.to_string()),
            });
        };

        if let Some(origin) = origin {
            let module = ModuleScope::from_nominal_origin(origin);
            return match self.decls.local_type_binding(&module, *name) {
                Some(TypeBinding::Contract(key)) => Ok(key),
                _ => Err(TypeRefError::UnknownContract {
                    qualifier: None,
                    name: *name,
                }),
            };
        }

        match self
            .decls
            .resolve_visible_type_binding(module, *qualifier, *name)
        {
            Some(TypeBinding::Contract(key)) => Ok(key),
            _ => Err(TypeRefError::UnknownContract {
                qualifier: *qualifier,
                name: *name,
            }),
        }
    }

    pub(crate) fn finalize_nominal_args(
        &self,
        module: &ModuleScope,
        key: &NominalKey,
        args: &[GenericArg],
    ) -> Result<Type, TypeRefError> {
        let mut state = FinalizeState::new(None);
        self.finalize_nominal(
            module,
            &GenericTypeContext::default(),
            key,
            args,
            &mut state,
        )
    }

    fn finalize_nominal(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        key: &NominalKey,
        args: &[GenericArg],
        state: &mut FinalizeState,
    ) -> Result<Type, TypeRefError> {
        let params = self.decls.nominal_generics(key).unwrap_or_default();
        let args = self.finalize_decl_generic_args_inner(module, generics, &params, args, state)?;
        let const_args = ConstTerm::to_args_no_infer(&args.const_args)
            .expect("type reference finalization does not create const inference terms");
        Ok(nominal_type_with_args(key, &args.type_args, &const_args))
    }

    fn finalize_decl_generic_args_inner(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        params: &GenericParams,
        args: &[GenericArg],
        state: &mut FinalizeState,
    ) -> Result<GenericArgs, TypeRefError> {
        let type_len = params.type_params.len();
        let expected = type_len + params.const_params.len();
        if args.len() != expected {
            return Err(TypeRefError::GenericArity {
                expected,
                found: args.len(),
            });
        }

        let mut type_args = Vec::with_capacity(type_len);
        let mut const_args = Vec::with_capacity(params.const_params.len());
        for (index, arg) in args.iter().enumerate() {
            if index < type_len {
                let GenericArg::Type(ty) = arg else {
                    return Err(TypeRefError::GenericArgKindMismatch { expected: "type" });
                };
                type_args.push(self.finalize_inner(module, generics, ty, state)?);
            } else {
                let arg = self.finalize_generic_const_arg(module, generics, arg, state)?;
                const_args.push(ConstTerm::from_arg(&arg));
            }
        }
        Ok(GenericArgs {
            type_args,
            const_args,
        })
    }

    fn finalize_generic_const_arg(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        arg: &GenericArg,
        state: &mut FinalizeState,
    ) -> Result<ConstArg, TypeRefError> {
        match arg {
            GenericArg::Const(arg) => finalize_const_arg(generics, arg),
            GenericArg::Type(ty) => match bare_type_name(ty) {
                Some(name) => finalize_const_name_arg(generics, name),
                None => {
                    let ty = self.finalize_inner(module, generics, ty, state)?;
                    match ty {
                        Type::Var(id) => {
                            let name = generics.type_param_name(id).unwrap_or(Ident::new("_"));
                            Err(TypeRefError::Unknown {
                                qualifier: None,
                                name,
                            })
                        }
                        _ => Err(TypeRefError::GenericArgKindMismatch { expected: "const" }),
                    }
                }
            },
        }
    }
}

fn canonical_contract_ref(key: &ContractKey) -> ContractRef {
    ContractRef::Named {
        qualifier: None,
        name: key.name,
        origin: key.module.nominal_origin(),
    }
}

fn finalize_const_arg(
    generics: &GenericTypeContext,
    arg: &ConstArg,
) -> Result<ConstArg, TypeRefError> {
    match arg {
        ConstArg::Name(name) => finalize_const_name_arg(generics, *name),
        ConstArg::Value(_) | ConstArg::Param(_) => Ok(arg.clone()),
    }
}

fn finalize_const_name_arg(
    generics: &GenericTypeContext,
    name: Ident,
) -> Result<ConstArg, TypeRefError> {
    Ok(finalize_const_name(generics, name)?.map_or(ConstArg::Name(name), ConstArg::Param))
}

fn finalize_const_name(
    generics: &GenericTypeContext,
    name: Ident,
) -> Result<Option<ConstParamId>, TypeRefError> {
    if generics.has_type_param(name) {
        return Err(TypeRefError::Unknown {
            qualifier: None,
            name,
        });
    }
    Ok(generics.const_param(name))
}

fn finalize_array_len(
    generics: &GenericTypeContext,
    len: ArrayLen,
) -> Result<ArrayLen, TypeRefError> {
    match len {
        ArrayLen::Named(name) => {
            Ok(finalize_const_name(generics, name)?.map_or(ArrayLen::Named(name), ArrayLen::Param))
        }
        ArrayLen::Fixed(_) | ArrayLen::Infer | ArrayLen::Param(_) => Ok(len),
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
