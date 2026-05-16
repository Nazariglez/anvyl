use std::collections::{HashMap, HashSet};

use super::{
    CallableKind, CastConversionSchema, ContractKey, DeclError, DeclarationIndex, DynInference,
    ExtendSchema, GenericContextError, GenericOwnerFrame, GenericParamKind, GenericParams,
    MethodKey, MethodSurface, ModuleScope, NominalKey, TypeAliasDef, TypeBinding, TypeChecker,
    TypeError, TypeRefResolver, ValueDecl, VariantPayload, contracts, same_extend_target,
    type_ops::TypeVisitor, type_refs::GenericParamError,
};
use crate::{
    ast::{
        AggregateKind, ArrayLen, ConstArg, ConstParam, ConstParamId, ContractRef, EscapeMode, Func,
        FuncParam, Ident, MethodReceiver, MethodSig, Mutability, NominalKind, Param, Program,
        ReturnSpec, Stmt, StructDecl, Type, TypeParam, TypeVarId, Visibility,
    },
    source::SourceId,
    span::{SourceSpan, Span},
};

pub(super) fn check_finite_size_cycles(tc: &mut TypeChecker) {
    let graph = finite_size_graph(&tc.decls);
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    let mut reported = HashSet::new();
    let mut keys = graph.keys().cloned().collect::<Vec<_>>();
    keys.sort_by_key(super::decls::nominal_key_sort_key);
    for key in keys {
        check_finite_size_key(&key, &graph, &mut visiting, &mut visited, &mut reported, tc);
    }
}

fn check_finite_size_key(
    key: &NominalKey,
    graph: &HashMap<NominalKey, Vec<NominalKey>>,
    visiting: &mut HashSet<NominalKey>,
    visited: &mut HashSet<NominalKey>,
    reported: &mut HashSet<NominalKey>,
    tc: &mut TypeChecker,
) {
    if visited.contains(key) {
        return;
    }
    if !visiting.insert(key.clone()) {
        if reported.insert(key.clone()) {
            let span = tc
                .decls
                .type_span(key)
                .and_then(|span| tc.module_error_span(&key.module, span));
            tc.push_error(TypeError::InfiniteSize {
                name: key.name,
                span,
            });
        }
        return;
    }
    if let Some(edges) = graph.get(key) {
        for edge in edges {
            check_finite_size_key(edge, graph, visiting, visited, reported, tc);
        }
    }
    visiting.remove(key);
    visited.insert(key.clone());
}

fn finite_size_graph(decls: &DeclarationIndex) -> HashMap<NominalKey, Vec<NominalKey>> {
    let mut graph = HashMap::new();
    for (key, schema) in decls.aggregates() {
        let mut edges = vec![];
        for field in schema.fields.values() {
            finite_size_edges(&field.ty, decls, &mut edges);
        }
        sort_finite_size_edges(&mut edges);
        graph.insert(key.clone(), edges);
    }
    for (key, schema) in decls.enums() {
        let mut edges = vec![];
        for variant in schema.variants.values() {
            match &variant.payload {
                VariantPayload::Unit => {}
                VariantPayload::Tuple(types) => {
                    for ty in types {
                        finite_size_edges(ty, decls, &mut edges);
                    }
                }
                VariantPayload::Struct(fields) => {
                    for field in fields.values() {
                        finite_size_edges(&field.ty, decls, &mut edges);
                    }
                }
            }
        }
        sort_finite_size_edges(&mut edges);
        graph.insert(key.clone(), edges);
    }
    graph
}

fn sort_finite_size_edges(edges: &mut Vec<NominalKey>) {
    edges.sort_by_key(super::decls::nominal_key_sort_key);
    edges.dedup();
}

fn finite_size_edges(ty: &Type, decls: &DeclarationIndex, edges: &mut Vec<NominalKey>) {
    match ty {
        Type::Nominal(nominal) => {
            if let Some(key) = decls.key_for_type(ty)
                && matches!(key.kind, NominalKind::Struct | NominalKind::Enum)
            {
                edges.push(key);
            }
            if !matches!(nominal.kind, NominalKind::DataRef | NominalKind::Extern) {
                for arg in &nominal.type_args {
                    finite_size_edges(arg, decls, edges);
                }
            }
        }
        Type::Array { elem, .. } => finite_size_edges(elem, decls, edges),
        Type::Tuple(types) => {
            for ty in types {
                finite_size_edges(ty, decls, edges);
            }
        }
        Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Func { .. }
        | Type::Dyn(_)
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. }
        | Type::List { .. }
        | Type::Map { .. }
        | Type::Slice { .. } => {}
    }
}

fn generic_param_decl_type_error(
    error: GenericContextError,
    source: Option<SourceId>,
) -> TypeError {
    TypeError::Decl(DeclError::DuplicateGenericParam {
        module: error.module,
        name: error.error.name(),
        span: source.map(|source| SourceSpan {
            source,
            span: error.span,
        }),
    })
}

pub(super) fn generic_param_type_error(
    error: GenericParamError,
    span: Option<SourceSpan>,
) -> TypeError {
    TypeError::DuplicateGenericParam {
        name: error.name(),
        span,
    }
}

fn validate_dyn_infer_decls(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    for (_key, aggregate) in decls.aggregates() {
        for field in aggregate.fields.values() {
            push_invalid_dyn_infer_decl(&field.ty, field.span, errors);
        }
    }
    for (_key, schema) in decls.enums() {
        for variant in schema.variants.values() {
            match &variant.payload {
                VariantPayload::Unit => {}
                VariantPayload::Tuple(types) => {
                    for ty in types {
                        push_invalid_dyn_infer_decl(ty, None, errors);
                    }
                }
                VariantPayload::Struct(fields) => {
                    for field in fields.values() {
                        push_invalid_dyn_infer_decl(&field.ty, field.span, errors);
                    }
                }
            }
        }
    }
    for alias in decls.type_aliases() {
        push_invalid_dyn_infer_decl(&alias.def.aliased, Some(alias.def.span), errors);
    }
    for value in decls.values() {
        match &value.decl {
            ValueDecl::Const(sig) => push_invalid_dyn_infer_decl(&sig.ty, None, errors),
            ValueDecl::Global(sig) => {
                push_invalid_dyn_infer_decl(&sig.ty, Some(sig.initializer_span), errors);
            }
            ValueDecl::Func(sig) if sig.kind == CallableKind::ExternFunction => {
                push_invalid_dyn_infer_decl(&sig.ty, None, errors);
            }
            ValueDecl::Func(_) => {}
        }
    }
}

fn push_invalid_dyn_infer_decl(ty: &Type, span: Option<SourceSpan>, errors: &mut Vec<TypeError>) {
    if DynInference::has_raw_hole(ty) {
        errors.push(TypeError::CompileError {
            message: "inferred dynamic contracts are only allowed as direct parameters of callables with bodies"
                .to_string(),
            span,
        });
    }
}

fn validate_type_alias_decls(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    for alias in decls.type_aliases() {
        validate_type_alias_def(
            decls,
            &alias.def,
            &alias.def.aliased,
            matches!(alias.visibility, Visibility::Public),
            errors,
        );
    }
}

pub(super) fn validate_public_value_surfaces(
    decls: &DeclarationIndex,
    errors: &mut Vec<TypeError>,
) {
    for value in decls.values() {
        if !matches!(value.visibility, Visibility::Public) {
            continue;
        }
        if let Some(ty) = private_exposed_type(decls, value.decl.ty()) {
            errors.push(TypeError::Decl(DeclError::PublicValuePrivateType {
                kind: value.decl.public_kind(),
                name: value.name,
                ty,
                span: value.decl.diagnostic_span(),
            }));
        }
    }
}

fn validate_public_contract_types(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    for contract in decls.contracts() {
        if !matches!(contract.visibility, Visibility::Public) {
            continue;
        }
        for (include, span) in &contract.includes {
            if private_included_contract(decls, &contract.key.module, include).is_some() {
                errors.push(TypeError::Decl(DeclError::PublicContractPrivateType {
                    name: contract.key.name,
                    ty: Type::Dyn(include.clone()),
                    span: Some(*span),
                }));
            }
        }
        for req in &contract.requirements {
            let exposed = req
                .params
                .iter()
                .find_map(|param| private_exposed_type(decls, &param.ty))
                .or_else(|| private_exposed_type(decls, &req.ret.ty));
            if let Some(ty) = exposed {
                errors.push(TypeError::Decl(DeclError::PublicContractPrivateType {
                    name: contract.key.name,
                    ty,
                    span: req.span,
                }));
            }
        }
    }
}

pub(super) fn validate_type_alias_def(
    decls: &DeclarationIndex,
    alias: &TypeAliasDef,
    target: &Type,
    public: bool,
    errors: &mut Vec<TypeError>,
) {
    if matches!(target, Type::Infer) {
        return;
    }
    push_unused_alias_params(&alias.generics, target, alias.span, errors);
    if public && let Some(ty) = private_exposed_type(decls, target) {
        errors.push(TypeError::Decl(DeclError::PublicAliasPrivateType {
            name: alias.name,
            ty,
            span: Some(alias.span),
        }));
    }
}

fn push_unused_alias_params(
    generics: &GenericParams,
    ty: &Type,
    span: SourceSpan,
    errors: &mut Vec<TypeError>,
) {
    let facts = target_facts(ty);
    for param in &generics.type_params {
        if !facts.type_params.contains(&param.id) {
            errors.push(TypeError::Decl(DeclError::UnusedAliasTypeParam {
                name: param.name,
                span: Some(span),
            }));
        }
    }
    for param in &generics.const_params {
        if !facts.const_params.contains(&param.id) {
            errors.push(TypeError::Decl(DeclError::UnusedAliasConstParam {
                name: param.name,
                span: Some(span),
            }));
        }
    }
}

fn private_exposed_type(decls: &DeclarationIndex, ty: &Type) -> Option<Type> {
    match ty {
        Type::Nominal(nominal) => {
            let key = decls.key_for_type(ty)?;
            let exported = decls
                .exported_nominal_type(&key.module, key.name)
                .is_some_and(|exported| exported == key);
            if !exported {
                return Some(ty.clone());
            }
            nominal
                .type_args
                .iter()
                .find_map(|ty| private_exposed_type(decls, ty))
        }
        Type::Func { params, ret } => params
            .iter()
            .find_map(|param| private_exposed_type(decls, &param.ty))
            .or_else(|| private_exposed_type(decls, &ret.ty)),
        Type::Dyn(contract) => private_contract_type(decls, contract),
        Type::Tuple(elems) => elems.iter().find_map(|ty| private_exposed_type(decls, ty)),
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            private_exposed_type(decls, elem)
        }
        Type::Map { key, value } => {
            private_exposed_type(decls, key).or_else(|| private_exposed_type(decls, value))
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
        | Type::UnresolvedNominal { .. } => None,
    }
}

fn private_included_contract(
    decls: &DeclarationIndex,
    module: &ModuleScope,
    contract: &ContractRef,
) -> Option<ContractKey> {
    match contract {
        ContractRef::Named { .. } => {
            let resolver = TypeRefResolver::module_only(decls);
            let key = resolver.resolve_contract_ref(module, contract).ok()?;
            let exported = matches!(
                decls.exported_type_binding(&key.module, key.name),
                Some(TypeBinding::Contract(exported)) if exported == key
            );
            (!exported).then_some(key)
        }
        ContractRef::Intersection(contracts) => contracts
            .iter()
            .find_map(|contract| private_included_contract(decls, module, contract)),
        ContractRef::Anonymous(_) | ContractRef::Infer | ContractRef::Hole(_) => None,
    }
}

fn private_contract_type(decls: &DeclarationIndex, contract: &ContractRef) -> Option<Type> {
    match contract {
        ContractRef::Named { name, origin, .. } => {
            let module = origin
                .as_ref()
                .map_or(ModuleScope::Root, ModuleScope::from_nominal_origin);
            let key = ContractKey {
                module,
                name: *name,
            };
            let exported = matches!(
                decls.exported_type_binding(&key.module, key.name),
                Some(TypeBinding::Contract(exported)) if exported == key
            );
            (!exported).then(|| Type::Dyn(contract.clone()))
        }
        ContractRef::Anonymous(surface) => surface.requirements.iter().find_map(|req| {
            req.params
                .iter()
                .find_map(|param| private_exposed_type(decls, &param.ty))
                .or_else(|| private_exposed_type(decls, &req.ret.ty))
        }),
        ContractRef::Intersection(contracts) => contracts
            .iter()
            .find_map(|contract| private_contract_type(decls, contract)),
        ContractRef::Infer | ContractRef::Hole(_) => None,
    }
}

fn validate_extend_decls(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    validate_duplicate_extend_methods(decls, errors);
    for extend in decls.extends() {
        if matches!(extend.target, Type::Infer) {
            continue;
        }
        let facts = target_facts(&extend.target);
        if unsupported_extend_target(&extend.target, &facts) {
            errors.push(TypeError::Decl(DeclError::UnsupportedExtendTarget {
                ty: extend.target.clone(),
                span: Some(extend.span),
            }));
        }
        for param in &extend.generics.type_params {
            if !facts.type_params.contains(&param.id) {
                errors.push(TypeError::Decl(DeclError::UnusedExtendTypeParam {
                    name: param.name,
                    span: Some(extend.span),
                }));
            }
        }
        for param in &extend.generics.const_params {
            if !facts.const_params.contains(&param.id) {
                errors.push(TypeError::Decl(DeclError::UnusedExtendConstParam {
                    name: param.name,
                    span: Some(extend.span),
                }));
            }
        }
        validate_extend_method_conflicts(decls, extend, errors);
        validate_cast_froms(decls, extend, errors);
    }
}

fn validate_duplicate_extend_methods(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    let extends = decls.extends().collect::<Vec<_>>();
    for (index, extend) in extends.iter().enumerate() {
        for prior in &extends[..index] {
            if prior.origin != extend.origin
                || !same_extend_target(
                    &prior.target,
                    &prior.generics,
                    &extend.target,
                    &extend.generics,
                )
            {
                continue;
            }
            for method_key in extend.methods.keys() {
                if prior.methods.contains_key(method_key) {
                    errors.push(TypeError::Decl(DeclError::DuplicateExtendMethod {
                        name: method_key.name,
                        surface: method_key.surface,
                        span: Some(extend.span),
                    }));
                }
            }
        }
    }
}

pub(super) fn validate_param_escape(
    errors: &mut Vec<TypeError>,
    escape: EscapeMode,
    mutable: bool,
    cast_accept: bool,
    ty: &Type,
    span: Option<SourceSpan>,
) {
    if !escape.is_escaping() {
        return;
    }
    if mutable {
        errors.push(TypeError::CompileError {
            message: "`escaping` cannot be combined with `var`".to_string(),
            span,
        });
    }
    if cast_accept {
        errors.push(TypeError::CompileError {
            message: "`escaping` cannot be combined with `as`".to_string(),
            span,
        });
    }
    if !matches!(ty, Type::Func { .. }) {
        errors.push(TypeError::CompileError {
            message: "`escaping` is only valid on function-typed parameters".to_string(),
            span,
        });
    }
}

fn validate_cast_froms(
    decls: &DeclarationIndex,
    extend: &ExtendSchema,
    errors: &mut Vec<TypeError>,
) {
    for cast in &extend.cast_froms {
        validate_cast_from_param(cast, errors);
        if same_extend_target(
            &cast.param.ty,
            &extend.generics,
            &extend.target,
            &extend.generics,
        ) {
            errors.push(TypeError::Decl(DeclError::PointlessCastFrom {
                ty: cast.param.ty.clone(),
                span: Some(cast.span),
            }));
        }
        if let Some(ret) = &cast.ret
            && !same_extend_target(&ret.ty, &extend.generics, &extend.target, &extend.generics)
        {
            errors.push(TypeError::Decl(DeclError::CastFromReturnMismatch {
                expected: extend.target.clone(),
                found: ret.ty.clone(),
                span: Some(cast.span),
            }));
        }
        if has_duplicate_cast_from(decls, extend, cast) {
            errors.push(TypeError::Decl(DeclError::DuplicateCastFrom {
                target: extend.target.clone(),
                source: cast.param.ty.clone(),
                span: Some(cast.span),
            }));
        }
    }
}

fn validate_cast_from_param(cast: &CastConversionSchema, errors: &mut Vec<TypeError>) {
    validate_param_escape(
        errors,
        cast.param.escape,
        cast.param.mutable,
        cast.param.cast_accept,
        &cast.param.ty,
        Some(cast.span),
    );
}

fn has_duplicate_cast_from(
    decls: &DeclarationIndex,
    extend: &ExtendSchema,
    cast: &CastConversionSchema,
) -> bool {
    for other_extend in decls.extends() {
        if other_extend.id == extend.id {
            for other in &other_extend.cast_froms {
                if std::ptr::eq(other, cast) {
                    return false;
                }
                if same_extend_target(
                    &other.param.ty,
                    &other_extend.generics,
                    &cast.param.ty,
                    &extend.generics,
                ) {
                    return true;
                }
            }
            continue;
        }
        if other_extend.origin != extend.origin
            || !same_extend_target(
                &other_extend.target,
                &other_extend.generics,
                &extend.target,
                &extend.generics,
            )
        {
            continue;
        }
        if other_extend.cast_froms.iter().any(|other| {
            same_extend_target(
                &other.param.ty,
                &other_extend.generics,
                &cast.param.ty,
                &extend.generics,
            )
        }) {
            return true;
        }
    }
    false
}

fn unsupported_extend_target(ty: &Type, facts: &TargetFacts) -> bool {
    matches!(ty, Type::Void | Type::Func { .. } | Type::InferReturn) || facts.contains_void
}

fn validate_extend_method_conflicts(
    decls: &DeclarationIndex,
    extend: &ExtendSchema,
    errors: &mut Vec<TypeError>,
) {
    let Some(key) = decls.key_for_type(&extend.target) else {
        return;
    };
    if key.module != extend.origin {
        return;
    }
    if let Some(aggregate) = decls.aggregate(&key) {
        for method_key in extend.methods.keys() {
            if aggregate.methods.contains_key(method_key) {
                push_extend_method_conflict(errors, extend, *method_key);
            }
        }
    }
    if let Some(enum_schema) = decls.enum_schema(&key) {
        for method_key in extend.methods.keys() {
            if method_key.surface == MethodSurface::Static
                && enum_schema.variants.contains_key(&method_key.name)
            {
                push_extend_method_conflict(errors, extend, *method_key);
            }
        }
    }
}

fn push_extend_method_conflict(
    errors: &mut Vec<TypeError>,
    extend: &ExtendSchema,
    method_key: MethodKey,
) {
    errors.push(TypeError::Decl(DeclError::ExtendMethodConflict {
        ty: extend.target.clone(),
        name: method_key.name,
        surface: method_key.surface,
        span: Some(extend.span),
    }));
}

#[derive(Default)]
struct TargetFacts {
    contains_void: bool,
    type_params: HashSet<TypeVarId>,
    const_params: HashSet<ConstParamId>,
}

impl TypeVisitor for TargetFacts {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        match ty {
            Type::Void => self.contains_void = true,
            Type::Var(id) => {
                self.type_params.insert(*id);
            }
            _ => {}
        }
        false
    }

    fn visit_const_arg(&mut self, arg: &ConstArg) -> bool {
        if let ConstArg::Param(id) = arg {
            self.const_params.insert(*id);
        }
        false
    }

    fn visit_array_len(&mut self, len: ArrayLen) -> bool {
        if let ArrayLen::Param(id) = len {
            self.const_params.insert(id);
        }
        false
    }
}

fn target_facts(ty: &Type) -> TargetFacts {
    let mut facts = TargetFacts::default();
    facts.visit_type(ty);
    facts
}

impl TypeChecker {
    pub(super) fn validate_type_return_specs(&mut self, ty: &Type, span: Span) {
        ReturnSpecValidator { tc: self, span }.visit_type(ty);
    }

    pub(super) fn validate_func_param_escape(
        &mut self,
        escape: EscapeMode,
        mutable: bool,
        cast_accept: bool,
        ty: &Type,
        span: Span,
    ) {
        let span = self.error_span(span);
        validate_param_escape(&mut self.errors, escape, mutable, cast_accept, ty, span);
    }

    pub(super) fn validate_escaping_parameter_types(&mut self, ty: &Type, span: Span) {
        EscapingParamValidator { tc: self, span }.visit_type(ty);
    }
}

struct EscapingParamValidator<'a> {
    tc: &'a mut TypeChecker,
    span: Span,
}

impl TypeVisitor for EscapingParamValidator<'_> {
    fn visit_func_param(&mut self, param: &FuncParam) -> bool {
        self.tc.validate_func_param_escape(
            param.escape,
            param.mutable,
            param.cast_accept,
            &param.ty,
            self.span,
        );
        self.visit_type(&param.ty)
    }

    fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
        if let ContractRef::Anonymous(surface) = contract {
            for req in &surface.requirements {
                for param in &req.params {
                    self.tc.validate_func_param_escape(
                        param.escape,
                        param.mutable,
                        false,
                        &param.ty,
                        self.span,
                    );
                }
            }
        }
        false
    }
}

struct ReturnSpecValidator<'a> {
    tc: &'a mut TypeChecker,
    span: Span,
}

impl TypeVisitor for ReturnSpecValidator<'_> {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        if let Type::Func { params, ret } = ty {
            validate_return_spec(
                ret,
                false,
                has_mutable_func_param(params),
                self.span,
                self.tc,
            );
        }
        false
    }

    fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
        if let ContractRef::Anonymous(surface) = contract {
            for req in &surface.requirements {
                validate_unsupported_return_spec(
                    &req.ret,
                    "contract requirements cannot return mutable places",
                    self.span,
                    self.tc,
                );
            }
        }
        false
    }
}

pub(super) fn has_generics(type_params: &[TypeParam], const_params: &[ConstParam]) -> bool {
    !type_params.is_empty() || !const_params.is_empty()
}

pub(super) fn is_generic(func: &Func) -> bool {
    has_generics(&func.type_params, &func.const_params)
}

pub(super) fn method_sig_is_generic(sig: &MethodSig) -> bool {
    has_generics(&sig.type_params, &sig.const_params)
}

pub(super) fn check_infer_return_decls(program: &Program, tc: &mut TypeChecker) {
    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                validate_return_spec(
                    &func.ret,
                    is_generic(func),
                    has_mutable_param(&func.params),
                    func_node.span,
                    tc,
                );
            }
            Stmt::ExternFunc(func_node) => {
                validate_unsupported_return_spec(
                    &func_node.node.ret,
                    "extern functions cannot return mutable places",
                    func_node.span,
                    tc,
                );
            }
            Stmt::Aggregate(agg_node) => {
                let agg = &agg_node.node;
                let owner_is_generic = has_generics(&agg.type_params, &agg.const_params);
                for method in &agg.methods {
                    validate_return_spec(
                        &method.sig.ret,
                        owner_is_generic || method_sig_is_generic(&method.sig),
                        method_has_mutable_input(&method.sig),
                        agg_node.span,
                        tc,
                    );
                }
            }
            Stmt::Extend(extend_node) => {
                let extend = &extend_node.node;
                let owner_is_generic = has_generics(&extend.type_params, &extend.const_params);
                for method in &extend.methods {
                    validate_return_spec(
                        &method.node.sig.ret,
                        owner_is_generic || method_sig_is_generic(&method.node.sig),
                        method_has_mutable_input(&method.node.sig),
                        method.span,
                        tc,
                    );
                }
                for cast in &extend.cast_froms {
                    if let Some(ret) = &cast.node.ret {
                        validate_unsupported_return_spec(
                            ret,
                            "cast from declarations cannot return mutable places",
                            cast.span,
                            tc,
                        );
                    }
                }
            }
            Stmt::Contract(contract_node) => {
                for req in &contract_node.node.requirements {
                    validate_unsupported_return_spec(
                        &req.node.sig.ret,
                        "contract requirements cannot return mutable places",
                        req.span,
                        tc,
                    );
                }
            }
            _ => {}
        }
    }
}

pub(super) fn has_mutable_param(params: &[Param]) -> bool {
    params
        .iter()
        .any(|param| matches!(param.mutability, Mutability::Mutable))
}

pub(super) fn has_mutable_func_param(params: &[FuncParam]) -> bool {
    params.iter().any(|param| param.mutable)
}

fn method_has_mutable_input(sig: &MethodSig) -> bool {
    matches!(sig.receiver, Some(MethodReceiver::Var)) || has_mutable_param(&sig.params)
}

pub(super) fn validate_return_spec(
    ret: &ReturnSpec,
    generic: bool,
    first_input_mutable: bool,
    span: Span,
    tc: &mut TypeChecker,
) {
    if ret.is_infer() && !generic {
        tc.push_error(TypeError::InferReturnNonGeneric {
            span: tc.error_span(span),
        });
    }
    validate_place_return_spec(ret, first_input_mutable, span, tc);
}

fn validate_unsupported_return_spec(
    ret: &ReturnSpec,
    place_message: &'static str,
    span: Span,
    tc: &mut TypeChecker,
) {
    if ret.is_infer() {
        tc.push_error(TypeError::InferReturnExtern {
            span: tc.error_span(span),
        });
    }
    if ret.is_place() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: place_message,
            span: tc.error_span(span),
        });
    }
}

fn validate_place_return_spec(
    ret: &ReturnSpec,
    first_input_mutable: bool,
    span: Span,
    tc: &mut TypeChecker,
) {
    if !ret.is_place() {
        return;
    }
    if ret.is_void() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place returns cannot return void",
            span: tc.error_span(span),
        });
    }
    if !first_input_mutable {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place returns require a first mutable input",
            span: tc.error_span(span),
        });
    }
}

pub(super) fn check_decl_param_order(program: &Program, tc: &mut TypeChecker) {
    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func) => check_param_order(&func.node.params, func.span, tc),
            Stmt::Aggregate(agg) => {
                for method in &agg.node.methods {
                    check_param_order(&method.sig.params, agg.span, tc);
                }
            }
            Stmt::Extend(extend) => {
                for method in &extend.node.methods {
                    check_param_order(&method.node.sig.params, method.span, tc);
                }
            }
            _ => {}
        }
    }
}

pub(super) fn check_param_order(params: &[Param], span: Span, tc: &mut TypeChecker) {
    let mut saw_default = false;
    for param in params {
        if param.default.is_some() {
            saw_default = true;
        } else if saw_default {
            tc.push_error(TypeError::RequiredParamAfterDefault {
                name: param.name,
                span: tc.error_span(span),
            });
        }
    }
}

pub(super) fn check_method_generic_shadows(agg: &StructDecl, span: Span, tc: &mut TypeChecker) {
    let owner_params = agg
        .type_params
        .iter()
        .map(|param| (param.name, GenericParamKind::Type))
        .chain(
            agg.const_params
                .iter()
                .map(|param| (param.name, GenericParamKind::Const)),
        )
        .collect::<HashMap<_, _>>();

    for method in &agg.methods {
        let params = method
            .sig
            .type_params
            .iter()
            .map(|param| (GenericParamKind::Type, param.name))
            .chain(
                method
                    .sig
                    .const_params
                    .iter()
                    .map(|param| (GenericParamKind::Const, param.name)),
            );
        for (kind, name) in params {
            check_method_generic_shadow(agg.kind, &owner_params, kind, name, span, tc);
        }
    }
}

fn check_method_generic_shadow(
    owner_kind: AggregateKind,
    owner_params: &HashMap<Ident, GenericParamKind>,
    method_param: GenericParamKind,
    name: Ident,
    span: Span,
    tc: &mut TypeChecker,
) {
    let Some(owner_param) = owner_params.get(&name).copied() else {
        return;
    };
    tc.push_error(TypeError::MethodGenericShadow {
        owner_kind,
        method_param,
        owner_param,
        name,
        span: tc.error_span(span),
    });
}

impl TypeChecker {
    pub(super) fn finalize_declarations(&mut self) {
        let saved_module = self.current_module.clone();
        let mut decls = std::mem::take(&mut self.decls);
        let lookup = decls.clone();
        let generic_errors = decls.map_canonical_type_uses(|site, ty| {
            self.current_module = site.module.clone();
            let span = site.span;
            let ty = self.finalize_decl_type(&lookup, site, ty);
            let ty = self.normalize_type_consts(&ty, span);
            self.reject_user_any_type(&ty, span);
            ty
        });
        for error in generic_errors {
            let source = self.module_sources.get(&error.module).copied();
            self.push_error(generic_param_decl_type_error(error, source));
        }
        validate_type_alias_decls(&decls, &mut self.errors);
        contracts::finalize_contracts(&mut decls, &mut self.errors, &mut self.lint_events);
        validate_public_contract_types(&decls, &mut self.errors);
        validate_dyn_infer_decls(&decls, &mut self.errors);
        validate_extend_decls(&decls, &mut self.errors);
        for error in decls.build_projection_entries() {
            self.push_error(TypeError::Decl(error));
        }
        for error in decls.build_promoted_surfaces(&self.externs) {
            self.push_error(TypeError::Decl(error));
        }
        self.validate_final_decl_type_uses(&mut decls);
        self.current_module = saved_module;
        self.decls = decls;
    }

    fn validate_final_decl_type_uses(&mut self, decls: &mut DeclarationIndex) {
        let validation = decls.clone();
        self.decls = validation.clone();
        let _ = decls.map_canonical_type_uses(|site, ty| {
            self.current_module = site.module;
            self.push_generic_owner_frame(GenericOwnerFrame {
                params: GenericParams {
                    type_params: site.type_params,
                    const_params: vec![],
                },
                ..GenericOwnerFrame::default()
            });
            self.validate_nominal_uses_in(&validation, &ty, site.span);
            self.pop_generic_owner_frame();
            ty
        });
    }
}
