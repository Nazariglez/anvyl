use std::collections::{HashMap, HashSet};

use super::{
    const_term::ConstTerm,
    decls::{
        DeclarationIndex, ModuleScope, NominalKey, TypeAliasKey, TypeBinding,
        nominal_type_with_args,
    },
    generic::{GenericArgs, GenericParams, substitute},
    type_ops::bare_type_name,
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstParam, ConstParamId, FuncParam, GenericArg, Ident, Type,
        TypeParam, TypeVarId,
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
}

pub(crate) type LocalTypeAliasKey = Span;

#[derive(Clone)]
pub(crate) struct LocalTypeAlias {
    pub(crate) key: LocalTypeAliasKey,
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
    pub(crate) generics: GenericParams,
    pub(crate) generic_context: GenericTypeContext,
    pub(crate) aliased: Type,
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
        match scope.entry(alias.name) {
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AliasExpansionKey {
    Module(TypeAliasKey),
    Local(LocalTypeAliasKey),
}

pub(crate) struct TypeRefResolver<'a> {
    decls: &'a DeclarationIndex,
    local_types: Option<&'a LocalTypeScopes>,
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
        self.finalize_inner(module, generics, ty, None, &mut vec![])
    }

    fn finalize_inner(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        ty: &Type,
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
    ) -> Result<Type, TypeRefError> {
        match ty {
            Type::UnresolvedName(name) => {
                self.finalize_unresolved_name(module, generics, *name, local_depth, stack)
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
                local_depth,
                stack,
            ),
            Type::Func { params, ret } => Ok(Type::Func {
                params: params
                    .iter()
                    .map(|param| {
                        Ok(FuncParam::new(
                            self.finalize_inner(module, generics, &param.ty, local_depth, stack)?,
                            param.mutable,
                            param.cast_accept,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
                ret: Box::new(self.finalize_inner(module, generics, ret, local_depth, stack)?),
            }),
            Type::Tuple(elems) => elems
                .iter()
                .map(|ty| self.finalize_inner(module, generics, ty, local_depth, stack))
                .collect::<Result<Vec<_>, _>>()
                .map(Type::Tuple),
            Type::Nominal(nominal) => {
                let type_args = nominal
                    .type_args
                    .iter()
                    .map(|ty| self.finalize_inner(module, generics, ty, local_depth, stack))
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
                elem: Box::new(self.finalize_inner(module, generics, elem, local_depth, stack)?),
            }),
            Type::Slice { elem } => Ok(Type::Slice {
                elem: Box::new(self.finalize_inner(module, generics, elem, local_depth, stack)?),
            }),
            Type::Array { elem, len } => Ok(Type::Array {
                elem: Box::new(self.finalize_inner(module, generics, elem, local_depth, stack)?),
                len: finalize_array_len(generics, *len)?,
            }),
            Type::Map { key, value } => Ok(Type::Map {
                key: Box::new(self.finalize_inner(module, generics, key, local_depth, stack)?),
                value: Box::new(self.finalize_inner(
                    module,
                    generics,
                    value,
                    local_depth,
                    stack,
                )?),
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
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
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
        if let Some(alias) = self.local_alias(name, local_depth) {
            return self.expand_local_alias(module, generics, alias, &[], local_depth, stack);
        }
        let binding = self
            .decls
            .resolve_visible_type_binding(module, None, name)
            .ok_or(TypeRefError::Unknown {
                qualifier: None,
                name,
            })?;
        self.finalize_binding(module, generics, binding, &[], local_depth, stack)
    }

    fn finalize_unresolved_nominal(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
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
            && let Some(alias) = self.local_alias(name, local_depth)
        {
            return self.expand_local_alias(
                module,
                generics,
                alias,
                generic_args,
                local_depth,
                stack,
            );
        }
        let binding = self
            .decls
            .resolve_visible_type_binding(module, qualifier, name)
            .ok_or(TypeRefError::Unknown { qualifier, name })?;
        self.finalize_binding(module, generics, binding, generic_args, local_depth, stack)
    }

    fn local_alias(&self, name: Ident, depth: Option<usize>) -> Option<&LocalTypeAlias> {
        self.local_types?.visible(name, depth)
    }

    pub(crate) fn finalize_type_binding(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        binding: TypeBinding,
        args: &[GenericArg],
    ) -> Result<Type, TypeRefError> {
        self.finalize_binding(module, generics, binding, args, None, &mut vec![])
    }

    pub(crate) fn finalize_module_alias_target(
        &self,
        key: &TypeAliasKey,
    ) -> Result<Type, TypeRefError> {
        let schema = self.decls.type_alias(key).ok_or(TypeRefError::Unknown {
            qualifier: None,
            name: key.name,
        })?;
        self.finalize_inner(
            &key.module,
            &GenericTypeContext::default(),
            &schema.aliased,
            None,
            &mut vec![AliasExpansionKey::Module(key.clone())],
        )
    }

    pub(crate) fn finalize_local_alias_target(
        &self,
        alias: &LocalTypeAlias,
    ) -> Result<Type, TypeRefError> {
        self.finalize_inner(
            &alias.module,
            &alias.generic_context,
            &alias.aliased,
            Some(alias.visible_depth),
            &mut vec![AliasExpansionKey::Local(alias.key)],
        )
    }

    fn finalize_binding(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        binding: TypeBinding,
        args: &[GenericArg],
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
    ) -> Result<Type, TypeRefError> {
        match binding {
            TypeBinding::Nominal(key) => {
                self.finalize_nominal(module, generics, &key, args, local_depth, stack)
            }
            TypeBinding::Alias(key) => {
                self.expand_alias(module, generics, &key, args, local_depth, stack)
            }
        }
    }

    fn expand_alias(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        key: &TypeAliasKey,
        args: &[GenericArg],
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
    ) -> Result<Type, TypeRefError> {
        let stack_key = AliasExpansionKey::Module(key.clone());
        if stack.contains(&stack_key) {
            return Err(TypeRefError::AliasCycle { name: key.name });
        }
        let schema = self.decls.type_alias(key).ok_or(TypeRefError::Unknown {
            qualifier: None,
            name: key.name,
        })?;
        let args = self.finalize_decl_generic_args_inner(
            module,
            generics,
            &schema.generics,
            args,
            local_depth,
            stack,
        )?;
        let (type_subst, const_subst) = schema.generics.substitutions(&args);
        let aliased = substitute(&schema.aliased, &type_subst, &const_subst);
        stack.push(stack_key);
        let result = self.finalize_inner(
            &key.module,
            &GenericTypeContext::default(),
            &aliased,
            None,
            stack,
        );
        stack.pop();
        result
    }

    fn expand_local_alias(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        alias: &LocalTypeAlias,
        args: &[GenericArg],
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
    ) -> Result<Type, TypeRefError> {
        let stack_key = AliasExpansionKey::Local(alias.key);
        if stack.contains(&stack_key) {
            return Err(TypeRefError::AliasCycle { name: alias.name });
        }
        let args = self.finalize_decl_generic_args_inner(
            module,
            generics,
            &alias.generics,
            args,
            local_depth,
            stack,
        )?;
        let (type_subst, const_subst) = alias.generics.substitutions(&args);
        let aliased = substitute(&alias.aliased, &type_subst, &const_subst);
        stack.push(stack_key);
        let result = self.finalize_inner(
            &alias.module,
            &alias.generic_context,
            &aliased,
            Some(alias.visible_depth),
            stack,
        );
        stack.pop();
        result
    }

    pub(crate) fn finalize_nominal_args(
        &self,
        module: &ModuleScope,
        key: &NominalKey,
        args: &[GenericArg],
    ) -> Result<Type, TypeRefError> {
        self.finalize_nominal(
            module,
            &GenericTypeContext::default(),
            key,
            args,
            None,
            &mut vec![],
        )
    }

    fn finalize_nominal(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        key: &NominalKey,
        args: &[GenericArg],
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
    ) -> Result<Type, TypeRefError> {
        let params = self.decls.nominal_generics(key).unwrap_or_default();
        let args = self.finalize_decl_generic_args_inner(
            module,
            generics,
            &params,
            args,
            local_depth,
            stack,
        )?;
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
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
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
                type_args.push(self.finalize_inner(module, generics, ty, local_depth, stack)?);
            } else {
                let arg =
                    self.finalize_generic_const_arg(module, generics, arg, local_depth, stack)?;
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
        local_depth: Option<usize>,
        stack: &mut Vec<AliasExpansionKey>,
    ) -> Result<ConstArg, TypeRefError> {
        match arg {
            GenericArg::Const(arg) => finalize_const_arg(generics, arg),
            GenericArg::Type(ty) => match bare_type_name(ty) {
                Some(name) => finalize_const_name_arg(generics, name),
                None => {
                    let ty = self.finalize_inner(module, generics, ty, local_depth, stack)?;
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
