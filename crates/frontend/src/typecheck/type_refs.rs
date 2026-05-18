use std::collections::{HashMap, HashSet};

use super::{
    ArityError, DeclError, DeprecatedUseKind, TypeChecker, TypeError,
    annotation::deprecated_lint,
    const_term::ConstTerm,
    contracts,
    decls::{
        ContractKey, DeclTypeSite, DeclarationIndex, ImportId, ModuleMemberLookup, ModuleScope,
        NominalKey, TypeAliasDef, TypeAliasKey, TypeBinding, nominal_generic_args,
        nominal_type_with_args,
    },
    dyn_infer::DynInference,
    generic::{GenericArgs, GenericParams, substitute},
    type_ops::{TypeVisitor, bare_type_name},
};
use crate::{
    ast::{
        AnonymousContractRequirement, ArrayLen, ConstArg, ConstParam, ConstParamId, ContractRef,
        FuncParam, GenericArg, Ident, Type, TypeParam, TypeVarId,
    },
    span::{SourceSpan, Span},
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeRefError {
    Unknown {
        qualifier: Option<Ident>,
        name: Ident,
        import: Option<ImportId>,
    },
    PrivateModuleMember {
        module: ModuleScope,
        name: Ident,
        import: Option<ImportId>,
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
        import: Option<ImportId>,
    },
    DuplicateContractRequirement {
        name: Ident,
    },
    ConflictingContractRequirement {
        name: Ident,
    },
    UnsupportedContractComposition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeRefWarningKind {
    TypeAlias,
    Contract,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TypeRefWarning {
    pub(crate) kind: TypeRefWarningKind,
    pub(crate) name: Ident,
    pub(crate) reason: Option<String>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FinalizedTypeRef {
    pub(crate) ty: Type,
    pub(crate) warnings: Vec<TypeRefWarning>,
    pub(crate) used_imports: Vec<ImportId>,
}

impl TypeRefError {
    pub(crate) fn import(&self) -> Option<&ImportId> {
        match self {
            Self::Unknown { import, .. }
            | Self::UnknownContract { import, .. }
            | Self::PrivateModuleMember { import, .. } => import.as_ref(),
            Self::GenericArity { .. }
            | Self::GenericArgKindMismatch { .. }
            | Self::AliasCycle { .. }
            | Self::ContractAsType { .. }
            | Self::DuplicateContractRequirement { .. }
            | Self::ConflictingContractRequirement { .. }
            | Self::UnsupportedContractComposition => None,
        }
    }
}

pub(super) fn type_ref_error(error: TypeRefError, span: Option<SourceSpan>) -> TypeError {
    match error {
        TypeRefError::Unknown {
            qualifier, name, ..
        } => TypeError::UnknownType {
            qualifier,
            name,
            span,
        },
        TypeRefError::PrivateModuleMember { module, name, .. } => {
            TypeError::PrivateModuleMember { module, name, span }
        }
        TypeRefError::GenericArity { expected, found } => {
            TypeError::GenericArity(ArityError::TypeArgs { expected, found })
        }
        TypeRefError::GenericArgKindMismatch { expected } => {
            TypeError::GenericArgKindMismatch { expected, span }
        }
        TypeRefError::AliasCycle { name } => TypeError::CompileError {
            message: format!("type alias '{name}' depends on itself"),
            span,
        },
        TypeRefError::ContractAsType { name } => TypeError::CompileError {
            message: format!(
                "contract '{name}' is not a concrete type; use 'dyn {name}' or a generic bound"
            ),
            span,
        },
        TypeRefError::UnknownContract {
            qualifier, name, ..
        } => TypeError::CompileError {
            message: match qualifier {
                Some(qualifier) => format!("unknown contract '{qualifier}.{name}'"),
                None => format!("unknown contract '{name}'"),
            },
            span,
        },
        TypeRefError::DuplicateContractRequirement { name } => TypeError::CompileError {
            message: format!("duplicate contract requirement '{name}'"),
            span,
        },
        TypeRefError::ConflictingContractRequirement { name } => TypeError::CompileError {
            message: format!("conflicting contract requirement '{name}'"),
            span,
        },
        TypeRefError::UnsupportedContractComposition => TypeError::CompileError {
            message: "inferred dynamic contracts are not supported yet".to_string(),
            span,
        },
    }
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

#[derive(Clone, Copy)]
struct TypeUsePolicy {
    warn_extern_deprecated: bool,
    validate_param_escape: bool,
}

impl TypeChecker {
    pub(super) fn finalize_decl_type(
        &mut self,
        decls: &DeclarationIndex,
        site: DeclTypeSite,
        ty: Type,
    ) -> Type {
        let resolver = TypeRefResolver::module_only(decls);
        match resolver.finalize_at(&site.module, &site.generics, &ty, Some(site.span)) {
            Ok(finalized) => {
                self.used_imports.extend(finalized.used_imports);
                self.push_type_ref_warnings(finalized.warnings);
                finalized.ty
            }
            Err(TypeRefError::Unknown {
                qualifier,
                name,
                import,
            }) => {
                self.mark_import_used(import);
                self.push_error(TypeError::Decl(DeclError::UnknownType {
                    module: site.module,
                    qualifier,
                    name,
                    span: Some(self.source_span(site.span)),
                }));
                Type::Infer
            }
            Err(error) => {
                self.mark_import_used(error.import().cloned());
                self.push_error(type_ref_error(error, self.error_span(site.span)));
                Type::Infer
            }
        }
    }

    pub(super) fn resolve_type_subject(&mut self, ty: &Type, span: Span) -> Option<Type> {
        let ty = self.resolve_type_for_tc_at(ty, span);
        (!matches!(ty, Type::Infer)).then_some(ty)
    }

    pub(super) fn resolve_type_for_tc_at(&mut self, ty: &Type, span: Span) -> Type {
        let result = self.type_ref_resolver().finalize_at(
            &self.current_module,
            &self.current_generic_context(),
            ty,
            Some(span),
        );
        self.finish_source_type_ref(result, span)
    }

    pub(super) fn resolve_callable_param_type(
        &mut self,
        ty: &Type,
        span: Span,
        exported: bool,
    ) -> Type {
        if matches!(ty, Type::Dyn(ContractRef::Infer)) {
            let result = self.type_ref_resolver().finalize_at(
                &self.current_module,
                &self.current_generic_context(),
                ty,
                Some(span),
            );
            let ty = self.finish_type_ref_result(result, span);
            return self.dyn_infer.assign_holes(
                &self.current_module,
                &ty,
                self.source_span(span),
                exported,
            );
        }
        self.resolve_type_for_tc_at(ty, span)
    }

    pub(super) fn resolve_type_binding_for_tc_at(
        &mut self,
        binding: TypeBinding,
        args: &[GenericArg],
        span: Span,
        use_name: Ident,
    ) -> Type {
        let result = self.type_ref_resolver().finalize_type_binding_at(
            &self.current_module,
            &self.current_generic_context(),
            binding,
            args,
            Some(span),
            use_name,
        );
        self.finish_source_type_ref(result, span)
    }

    pub(super) fn resolve_module_alias_target_for_tc_at(
        &mut self,
        key: &TypeAliasKey,
        span: Span,
        use_name: Ident,
    ) -> Type {
        let result =
            self.type_ref_resolver()
                .finalize_module_alias_target_at(key, Some(span), use_name);
        self.finish_source_type_ref(result, span)
    }

    pub(super) fn resolve_local_alias_target_for_tc_at(
        &mut self,
        alias: &LocalTypeAlias,
        span: Span,
        use_name: Ident,
    ) -> Type {
        let result =
            self.type_ref_resolver()
                .finalize_local_alias_target_at(alias, Some(span), use_name);
        self.finish_source_type_ref(result, span)
    }

    fn finish_source_type_ref(
        &mut self,
        result: Result<FinalizedTypeRef, TypeRefError>,
        span: Span,
    ) -> Type {
        let ty = self.finish_type_ref_result(result, span);
        self.reject_source_dyn_contracts(ty, span)
    }

    fn current_generic_context(&self) -> GenericTypeContext {
        self.generic_contexts.last().cloned().unwrap_or_default()
    }

    fn type_ref_resolver(&self) -> TypeRefResolver<'_> {
        TypeRefResolver::with_local_types(&self.decls, &self.local_type_scopes)
    }

    pub(super) fn validate_nominal_uses(&mut self, ty: &Type, span: Span) {
        let decls = self.decls.clone();
        self.validate_type_uses(
            &decls,
            ty,
            span,
            TypeUsePolicy {
                warn_extern_deprecated: true,
                validate_param_escape: false,
            },
        );
    }

    pub(super) fn validate_nominal_uses_in(
        &mut self,
        decls: &DeclarationIndex,
        ty: &Type,
        span: Span,
    ) {
        self.validate_type_uses(
            decls,
            ty,
            span,
            TypeUsePolicy {
                warn_extern_deprecated: false,
                validate_param_escape: true,
            },
        );
    }

    fn validate_type_uses(
        &mut self,
        decls: &DeclarationIndex,
        ty: &Type,
        span: Span,
        policy: TypeUsePolicy,
    ) {
        match ty {
            Type::Nominal(nominal) => {
                for arg in &nominal.type_args {
                    self.validate_type_uses(decls, arg, span, policy);
                }
                let Some(key) = decls.key_for_type(ty) else {
                    return;
                };
                if policy.warn_extern_deprecated {
                    self.warn_extern_type_deprecated(&key, span);
                }
                let Some(generics) = decls.nominal_generics(&key) else {
                    return;
                };
                let args = nominal_generic_args(ty).expect("nominal type");
                self.validate_nominal_args(decls, &key, &generics, &args, span);
            }
            Type::Func { params, ret } => {
                for param in params {
                    if policy.validate_param_escape {
                        self.validate_func_param_escape(
                            param.escape,
                            param.mutable,
                            param.cast_accept,
                            &param.ty,
                            span,
                        );
                    }
                    self.validate_type_uses(decls, &param.ty, span, policy);
                }
                self.validate_type_uses(decls, &ret.ty, span, policy);
            }
            Type::Dyn(contract) => self.validate_contract_ref_uses(decls, contract, span, policy),
            Type::Tuple(elems) => {
                for elem in elems {
                    self.validate_type_uses(decls, elem, span, policy);
                }
            }
            Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
                self.validate_type_uses(decls, elem, span, policy);
            }
            Type::Map { key, value } => {
                self.validate_type_uses(decls, key, span, policy);
                if let Some(err) = map_key_type_error(decls, key, self.error_span(span)) {
                    self.push_error(err);
                }
                self.validate_type_uses(decls, value, span, policy);
            }
            Type::Infer
            | Type::InferReturn
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => {}
        }
    }

    fn validate_contract_ref_uses(
        &mut self,
        decls: &DeclarationIndex,
        contract: &ContractRef,
        span: Span,
        policy: TypeUsePolicy,
    ) {
        if let Some(name) = contract_surface_conflict(decls, &self.current_module, contract) {
            self.push_error(TypeError::CompileError {
                message: format!("conflicting contract requirement '{name}'"),
                span: self.error_span(span),
            });
        }
        match contract {
            ContractRef::Anonymous(surface) => {
                for req in &surface.requirements {
                    for param in &req.params {
                        if policy.validate_param_escape {
                            self.validate_func_param_escape(
                                param.escape,
                                param.mutable,
                                false,
                                &param.ty,
                                span,
                            );
                        }
                        self.validate_type_uses(decls, &param.ty, span, policy);
                    }
                    self.validate_type_uses(decls, &req.ret.ty, span, policy);
                }
            }
            ContractRef::Intersection(contracts) => {
                for contract in contracts {
                    self.validate_contract_ref_uses(decls, contract, span, policy);
                }
            }
            ContractRef::Named { .. } | ContractRef::Infer | ContractRef::Hole(_) => {}
        }
    }

    fn finish_type_ref_result(
        &mut self,
        result: Result<FinalizedTypeRef, TypeRefError>,
        span: Span,
    ) -> Type {
        match result {
            Ok(finalized) => {
                self.used_imports.extend(finalized.used_imports);
                self.push_type_ref_warnings(finalized.warnings);
                self.finish_resolved_type(finalized.ty, span)
            }
            Err(error) => {
                self.mark_import_used(error.import().cloned());
                self.push_error_once(type_ref_error(error, self.error_span(span)));
                Type::Infer
            }
        }
    }

    fn reject_source_dyn_contracts(&mut self, ty: Type, span: Span) -> Type {
        if type_contains_anonymous_contract(&ty) {
            self.push_error(TypeError::CompileError {
                message: "anonymous dynamic contract syntax is not supported; declare a named contract or use dyn _ in a callable parameter".to_string(),
                span: self.error_span(span),
            });
            return Type::Infer;
        }
        self.reject_raw_dyn_infer(ty, span)
    }

    fn reject_raw_dyn_infer(&mut self, ty: Type, span: Span) -> Type {
        if !DynInference::has_raw_hole(&ty) {
            return ty;
        }
        let message = if type_contains_raw_dyn_infer_func(&ty) {
            "inferred dynamic contracts are not allowed in nested function types because they have no body that can own inference"
        } else {
            "inferred dynamic contracts are only allowed as direct parameters of callables with bodies"
        };
        self.push_error(TypeError::CompileError {
            message: message.to_string(),
            span: self.error_span(span),
        });
        Type::Infer
    }

    fn push_type_ref_warnings(&mut self, warnings: Vec<TypeRefWarning>) {
        for warning in warnings {
            let kind = match warning.kind {
                TypeRefWarningKind::TypeAlias => DeprecatedUseKind::TypeAlias,
                TypeRefWarningKind::Contract => DeprecatedUseKind::Contract,
            };
            self.push_lint_event(deprecated_lint(
                kind,
                warning.name,
                warning.reason.as_deref(),
                self.source_span(warning.span),
            ));
        }
    }
}

pub(super) fn map_key_type_error(
    decls: &DeclarationIndex,
    ty: &Type,
    span: Option<SourceSpan>,
) -> Option<TypeError> {
    let err = decls.map_key_error(ty)?;
    Some(TypeError::NonKeyableMapKey {
        ty: err.ty,
        field: err.field,
        span,
    })
}

fn contract_surface_conflict(
    decls: &DeclarationIndex,
    module: &ModuleScope,
    contract: &ContractRef,
) -> Option<Ident> {
    match contracts::requirements_for_ref(decls, module, contract) {
        Err(contracts::ContractSetError::ConflictingRequirement(name)) => Some(name),
        Ok(_) | Err(contracts::ContractSetError::UnknownContract) => None,
    }
}

fn type_contains_anonymous_contract(ty: &Type) -> bool {
    struct AnonymousContractVisitor;

    impl TypeVisitor for AnonymousContractVisitor {
        fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
            matches!(contract, ContractRef::Anonymous(_))
        }
    }

    let mut visitor = AnonymousContractVisitor;
    visitor.visit_type(ty)
}

fn type_contains_raw_dyn_infer_func(ty: &Type) -> bool {
    struct RawDynInferFunc;

    impl TypeVisitor for RawDynInferFunc {
        fn visit_type(&mut self, ty: &Type) -> bool {
            match ty {
                Type::Func { params, ret } => {
                    params
                        .iter()
                        .any(|param| DynInference::has_raw_hole(&param.ty))
                        || DynInference::has_raw_hole(&ret.ty)
                }
                _ => self.visit_type_children(ty),
            }
        }
    }

    let mut visitor = RawDynInferFunc;
    visitor.visit_type(ty)
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

#[derive(Default)]
struct FinalizeState {
    local_depth: Option<usize>,
    stack: Vec<AliasExpansionKey>,
    site: Option<Span>,
    warnings: Vec<TypeRefWarning>,
    used_imports: Vec<ImportId>,
}

impl FinalizeState {
    fn new(site: Option<Span>) -> Self {
        Self {
            site,
            ..Self::default()
        }
    }

    fn alias_target(alias: &AliasRef<'_>, site: Option<Span>) -> Self {
        Self {
            local_depth: alias.local_depth,
            stack: vec![alias.key.clone()],
            site,
            ..Self::default()
        }
    }

    fn mark_import_used(&mut self, import: Option<ImportId>) {
        if let Some(import) = import
            && !self.used_imports.contains(&import)
        {
            self.used_imports.push(import);
        }
    }

    fn warn_deprecated_alias(&mut self, alias: &AliasRef<'_>, use_name: Ident) {
        if alias.def.policy.has_deprecated()
            && let Some(span) = self.site
        {
            self.warnings.push(TypeRefWarning {
                kind: TypeRefWarningKind::TypeAlias,
                name: use_name,
                reason: alias.def.policy.deprecated_reason().map(str::to_string),
                span,
            });
        }
    }

    fn warn_deprecated_contract(&mut self, schema: &super::decls::ContractSchema, use_name: Ident) {
        if schema.policy.has_deprecated()
            && let Some(span) = self.site
        {
            self.warnings.push(TypeRefWarning {
                kind: TypeRefWarningKind::Contract,
                name: use_name,
                reason: schema.policy.deprecated_reason().map(str::to_string),
                span,
            });
        }
    }

    fn finish(self, ty: Type) -> FinalizedTypeRef {
        FinalizedTypeRef {
            ty,
            warnings: self.warnings,
            used_imports: self.used_imports,
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
                            param.escape,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
                ret: Box::new(ret.with_ty(self.finalize_inner(module, generics, &ret.ty, state)?)),
            }),
            Type::Dyn(contract) => self
                .finalize_contract_ref(module, generics, contract, state)
                .map(Type::Dyn),
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
                        .visible_type_binding_with_import(module, nominal.name)
                        .and_then(|(binding, _)| binding.into_nominal())
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
                import: None,
            });
        }
        if let Some(alias) = self.local_alias(name, state.local_depth) {
            let alias_ref = Self::local_alias_ref(alias);
            return self.expand_alias_ref(module, generics, &alias_ref, &[], state, name);
        }
        let (binding, import) = self.resolve_type_binding(module, None, name)?;
        state.mark_import_used(import);
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
                    import: None,
                });
            }
        }
        if qualifier.is_none()
            && let Some(alias) = self.local_alias(name, state.local_depth)
        {
            let alias_ref = Self::local_alias_ref(alias);
            return self.expand_alias_ref(module, generics, &alias_ref, generic_args, state, name);
        }
        let (binding, import) = self.resolve_type_binding(module, qualifier, name)?;
        state.mark_import_used(import);
        self.finalize_binding(module, generics, binding, generic_args, state, name)
    }

    fn resolve_type_binding(
        &self,
        module: &ModuleScope,
        qualifier: Option<Ident>,
        name: Ident,
    ) -> Result<(TypeBinding, Option<ImportId>), TypeRefError> {
        let lookup = self.decls.resolve_type_member(module, qualifier, name);
        match lookup.result {
            ModuleMemberLookup::Found(binding) => Ok((binding, lookup.import)),
            ModuleMemberLookup::Private => Err(TypeRefError::PrivateModuleMember {
                module: lookup.target.unwrap_or_else(|| module.clone()),
                name,
                import: lookup.import,
            }),
            ModuleMemberLookup::Missing => Err(TypeRefError::Unknown {
                qualifier,
                name,
                import: lookup.import,
            }),
        }
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
            import: None,
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

    fn finalize_contract_ref(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        contract: &ContractRef,
        state: &mut FinalizeState,
    ) -> Result<ContractRef, TypeRefError> {
        let mut refs = vec![];
        self.collect_canonical_contract_refs(module, generics, contract, state, &mut refs)?;
        refs.sort_by_key(ToString::to_string);
        let mut unique = vec![];
        for contract in refs {
            if !unique.contains(&contract) {
                unique.push(contract);
            }
        }
        let mut refs = unique;
        Ok(if refs.len() == 1 {
            refs.pop().expect("one contract ref")
        } else {
            ContractRef::Intersection(refs)
        })
    }

    fn collect_canonical_contract_refs(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        contract: &ContractRef,
        state: &mut FinalizeState,
        refs: &mut Vec<ContractRef>,
    ) -> Result<(), TypeRefError> {
        match contract {
            ContractRef::Named {
                name, origin: None, ..
            } => {
                let (key, import) = self.resolve_contract_ref_with_import(module, contract)?;
                state.mark_import_used(import);
                if let Some(schema) = self.decls.contract(&key) {
                    state.warn_deprecated_contract(schema, *name);
                }
                refs.push(canonical_contract_ref(&key));
                Ok(())
            }
            ContractRef::Named { .. } => {
                let (key, import) = self.resolve_contract_ref_with_import(module, contract)?;
                state.mark_import_used(import);
                refs.push(canonical_contract_ref(&key));
                Ok(())
            }
            ContractRef::Anonymous(surface) => {
                let requirements = surface
                    .requirements
                    .iter()
                    .map(|req| self.finalize_anonymous_requirement(module, generics, req, state))
                    .collect::<Result<Vec<_>, _>>()?;
                refs.push(ContractRef::Anonymous(crate::ast::AnonymousContract {
                    requirements: canonical_anonymous_requirements(requirements)?,
                }));
                Ok(())
            }
            ContractRef::Intersection(contracts) => {
                for contract in contracts {
                    self.collect_canonical_contract_refs(module, generics, contract, state, refs)?;
                }
                Ok(())
            }
            ContractRef::Infer | ContractRef::Hole(_) => {
                refs.push(contract.clone());
                Ok(())
            }
        }
    }

    fn finalize_anonymous_requirement(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        req: &AnonymousContractRequirement,
        state: &mut FinalizeState,
    ) -> Result<AnonymousContractRequirement, TypeRefError> {
        let params = req
            .params
            .iter()
            .map(|param| {
                Ok(crate::ast::AnonymousContractParam {
                    mutable: param.mutable,
                    escape: param.escape,
                    name: param.name,
                    ty: self.finalize_inner(module, generics, &param.ty, state)?,
                })
            })
            .collect::<Result<_, _>>()?;
        let ret = req
            .ret
            .with_ty(self.finalize_inner(module, generics, &req.ret.ty, state)?);
        Ok(AnonymousContractRequirement {
            receiver: req.receiver,
            name: req.name,
            params,
            ret,
        })
    }

    pub(crate) fn resolve_contract_ref(
        &self,
        module: &ModuleScope,
        contract: &ContractRef,
    ) -> Result<ContractKey, TypeRefError> {
        self.resolve_contract_ref_with_import(module, contract)
            .map(|(key, _)| key)
    }

    pub(crate) fn resolve_contract_ref_with_import(
        &self,
        module: &ModuleScope,
        contract: &ContractRef,
    ) -> Result<(ContractKey, Option<ImportId>), TypeRefError> {
        let ContractRef::Named {
            qualifier,
            name,
            origin,
        } = contract
        else {
            return Err(TypeRefError::UnsupportedContractComposition);
        };

        if let Some(origin) = origin {
            let module = ModuleScope::from_nominal_origin(origin);
            return match self.decls.local_type_binding(&module, *name) {
                Some(TypeBinding::Contract(key)) => Ok((key, None)),
                _ => Err(TypeRefError::UnknownContract {
                    qualifier: None,
                    name: *name,
                    import: None,
                }),
            };
        }

        let lookup = self.decls.resolve_type_member(module, *qualifier, *name);
        match lookup.result {
            ModuleMemberLookup::Found(TypeBinding::Contract(key)) => Ok((key, lookup.import)),
            ModuleMemberLookup::Private => Err(TypeRefError::PrivateModuleMember {
                module: lookup.target.unwrap_or_else(|| module.clone()),
                name: *name,
                import: lookup.import,
            }),
            ModuleMemberLookup::Found(_) | ModuleMemberLookup::Missing => {
                Err(TypeRefError::UnknownContract {
                    qualifier: *qualifier,
                    name: *name,
                    import: lookup.import,
                })
            }
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
                                import: None,
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

fn canonical_anonymous_requirements(
    requirements: Vec<AnonymousContractRequirement>,
) -> Result<Vec<AnonymousContractRequirement>, TypeRefError> {
    let mut by_name = HashMap::<Ident, AnonymousContractRequirement>::new();
    for requirement in requirements {
        match by_name.get(&requirement.name) {
            Some(prev) if same_anonymous_requirement(prev, &requirement) => {
                return Err(TypeRefError::DuplicateContractRequirement {
                    name: requirement.name,
                });
            }
            Some(_) => {
                return Err(TypeRefError::ConflictingContractRequirement {
                    name: requirement.name,
                });
            }
            None => {
                by_name.insert(requirement.name, requirement);
            }
        }
    }
    let mut requirements = by_name.into_values().collect::<Vec<_>>();
    requirements.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
    Ok(requirements)
}

fn same_anonymous_requirement(
    left: &AnonymousContractRequirement,
    right: &AnonymousContractRequirement,
) -> bool {
    left.receiver == right.receiver
        && left.ret == right.ret
        && left.params.len() == right.params.len()
        && left
            .params
            .iter()
            .zip(&right.params)
            .all(|(left, right)| left.mutable == right.mutable && left.ty == right.ty)
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
            import: None,
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
