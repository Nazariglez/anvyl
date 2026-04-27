use std::collections::{HashMap, HashSet};

use super::{
    ConstSubst, GenericParams, Inference, Specificity, TypeSubst, bare_type_name,
    compare_specificity, const_arg_usize, infer, substitute, type_ops::TypeFolder,
};
use crate::{
    ast::{
        AggregateKind, ArrayLen, ConstArg, ConstParam, FuncParam, GenericArg, Ident,
        MethodReceiver, Mutability, NominalKind, Param, Program, Stmt, StmtNode, Type, TypeParam,
        VariantKind, Visibility,
    },
    resolve::{ModuleKey, ModulePath, ResolveResult},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ModuleScope {
    Root,
    Named(ModulePath),
}

impl ModuleScope {
    pub(crate) fn named_path(&self) -> Option<crate::ast::ModulePath> {
        match self {
            ModuleScope::Root => None,
            ModuleScope::Named(p) => Some(p.to_ast_path()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct NominalKey {
    pub(crate) module: ModuleScope,
    pub(crate) kind: NominalKind,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct CallableId {
    pub(crate) module: ModuleScope,
    pub(crate) parent: Option<CallableParent>,
    pub(crate) kind: CallableKind,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum CallableParent {
    Nominal(NominalKey),
    Extend(ExtendId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CallableKind {
    Function,
    ExternFunction,
    StaticMethod,
    InstanceMethod,
    ExtendMethod,
    EnumVariant,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ExtendId {
    pub(crate) module: ModuleScope,
    pub(crate) index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct MethodId {
    pub(crate) owner: NominalKey,
    pub(crate) name: Ident,
}

pub(crate) struct DeclarationIndex {
    modules: HashMap<ModuleScope, ModuleDecls>,
    aggregates: HashMap<NominalKey, AggregateSchema>,
    enums: HashMap<NominalKey, EnumSchema>,
    extends: Vec<ExtendSchema>,
}

#[derive(Default)]
pub(crate) struct ModuleDecls {
    pub(crate) values: HashMap<Ident, ValueDecl>,
    pub(crate) types: HashMap<Ident, NominalKey>,
    pub(crate) modules: HashMap<Ident, ModuleScope>,
}

#[derive(Debug, Clone)]
pub(crate) enum ValueDecl {
    Func(FuncSig),
    Const(Type),
}

impl ValueDecl {
    pub(crate) fn ty(&self) -> &Type {
        match self {
            ValueDecl::Func(sig) => &sig.ty,
            ValueDecl::Const(ty) => ty,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FuncSig {
    pub(crate) generics: GenericParams,
    pub(crate) ty: Type,
}

#[derive(Clone)]
pub(crate) struct AggregateSchema {
    pub(crate) key: NominalKey,
    pub(crate) kind: AggregateKind,
    pub(crate) generics: GenericParams,
    pub(crate) fields: HashMap<Ident, FieldSchema>,
    pub(crate) methods: HashMap<Ident, MethodSchema>,
}

#[derive(Clone)]
pub(crate) struct FieldSchema {
    pub(crate) index: usize,
    pub(crate) ty: Type,
    pub(crate) has_default: bool,
}

#[derive(Clone)]
pub(crate) struct MethodSchema {
    pub(crate) generics: GenericParams,
    pub(crate) receiver: Option<MethodReceiver>,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) ret: Type,
}

pub(crate) struct EnumSchema {
    pub(crate) key: NominalKey,
    pub(crate) generics: GenericParams,
    pub(crate) variants: HashMap<Ident, VariantSchema>,
}

#[derive(Clone)]
pub(crate) enum VariantSchema {
    Unit,
    Tuple(Vec<Type>),
    Struct(HashMap<Ident, FieldSchema>),
}

pub(crate) struct ExtendSchema {
    pub(crate) id: ExtendId,
    pub(crate) origin: ModuleScope,
    pub(crate) target: Type,
    pub(crate) generics: GenericParams,
    pub(crate) methods: HashMap<Ident, ExtendMethodSchema>,
}

pub(crate) struct ExtendMethodSchema {
    pub(crate) generics: GenericParams,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) ret: Type,
}

pub(crate) enum ExtendMethodMatch<'a> {
    Match {
        extend: &'a ExtendSchema,
        method: &'a ExtendMethodSchema,
    },
    Ambiguous,
}

impl DeclarationIndex {
    pub(crate) fn from_root(program: &Program) -> Self {
        let mut index = Self::new();
        index.collect_module(program, ModuleScope::Root, true);
        index.resolve_nominals();
        index
    }

    pub(crate) fn from_root_and_modules(root: &Program, resolved: &ResolveResult) -> Self {
        let mut index = Self::new();
        index.collect_module(root, ModuleScope::Root, true);

        for group in &resolved.module_groups {
            for module in group {
                let ModuleKey::Named(ref path) = module.key else {
                    continue;
                };
                let scope = ModuleScope::Named(path.clone());
                index.collect_module(&module.program, scope, false);
            }
        }

        index.resolve_nominals();
        index
    }

    fn new() -> Self {
        Self {
            modules: HashMap::new(),
            aggregates: HashMap::new(),
            enums: HashMap::new(),
            extends: vec![],
        }
    }

    fn resolve_nominals(&mut self) {
        let mut scoped = HashMap::new();
        let mut fallback = HashMap::new();
        let mut ambiguous = HashSet::new();
        let aggregate_items = self
            .aggregates
            .iter()
            .map(|(key, schema)| (key, schema.generics.clone()));
        let enum_items = self
            .enums
            .iter()
            .map(|(key, schema)| (key, schema.generics.clone()));

        for (key, generics) in aggregate_items.chain(enum_items) {
            let ty = nominal_type(key);
            let entry = (ty.clone(), generics);
            scoped.insert((key.module.clone(), key.name), entry.clone());
            let is_ambiguous_fallback =
                !ambiguous.contains(&key.name) && fallback.insert(key.name, entry).is_some();
            if is_ambiguous_fallback {
                fallback.remove(&key.name);
                ambiguous.insert(key.name);
            }
        }

        for agg in self.aggregates.values_mut() {
            for field in agg.fields.values_mut() {
                field.ty = resolve_nominal(&agg.key.module, &field.ty, &scoped, &fallback);
            }
            for method in agg.methods.values_mut() {
                method.ret = resolve_nominal(&agg.key.module, &method.ret, &scoped, &fallback);
                for param in &mut method.params {
                    param.ty = resolve_nominal(&agg.key.module, &param.ty, &scoped, &fallback);
                }
            }
        }
        for extend in &mut self.extends {
            extend.target = resolve_nominal(&extend.origin, &extend.target, &scoped, &fallback);
            for method in extend.methods.values_mut() {
                method.ret = resolve_nominal(&extend.origin, &method.ret, &scoped, &fallback);
                for param in &mut method.params {
                    param.ty = resolve_nominal(&extend.origin, &param.ty, &scoped, &fallback);
                }
            }
        }
        for enm in self.enums.values_mut() {
            for variant in enm.variants.values_mut() {
                match variant {
                    VariantSchema::Tuple(types) => {
                        for ty in types.iter_mut() {
                            *ty = resolve_nominal(&enm.key.module, ty, &scoped, &fallback);
                        }
                    }
                    VariantSchema::Struct(fields) => {
                        for field in fields.values_mut() {
                            field.ty =
                                resolve_nominal(&enm.key.module, &field.ty, &scoped, &fallback);
                        }
                    }
                    VariantSchema::Unit => {}
                }
            }
        }
        for (scope, decls) in &mut self.modules {
            for value in decls.values.values_mut() {
                match value {
                    ValueDecl::Func(sig) => {
                        sig.ty = resolve_nominal(scope, &sig.ty, &scoped, &fallback);
                    }
                    ValueDecl::Const(ty) => {
                        *ty = resolve_nominal(scope, ty, &scoped, &fallback);
                    }
                }
            }
        }
    }

    fn collect_module(&mut self, program: &Program, scope: ModuleScope, include_private: bool) {
        let mut decls = ModuleDecls::default();
        let mut extend_index = 0;

        for stmt in &program.stmts {
            let visible = include_private || matches!(stmt_visibility(stmt), Visibility::Public);
            match &stmt.node {
                Stmt::Func(func_node) => {
                    let func = &func_node.node;
                    if !visible {
                        continue;
                    }
                    let ty = func_type_from_params(&func.params, &func.ret);
                    decls.values.insert(
                        func.name,
                        ValueDecl::Func(FuncSig {
                            generics: generic_params(&func.type_params, &func.const_params),
                            ty,
                        }),
                    );
                }
                Stmt::Aggregate(agg_node) => {
                    let agg = &agg_node.node;
                    let kind = agg.kind.into();
                    let key = NominalKey {
                        module: scope.clone(),
                        kind,
                        name: agg.name,
                    };
                    let mut fields = HashMap::new();
                    for (i, field) in agg.fields.iter().enumerate() {
                        fields.insert(
                            field.name,
                            FieldSchema {
                                index: i,
                                ty: field.ty.clone(),
                                has_default: field.default.is_some(),
                            },
                        );
                    }
                    let mut methods = HashMap::new();
                    for method in &agg.methods {
                        methods.insert(
                            method.name,
                            MethodSchema {
                                generics: generic_params(&method.type_params, &method.const_params),
                                receiver: method.receiver,
                                params: resolve_func_params(&method.params),
                                ret: method.ret.clone(),
                            },
                        );
                    }
                    if visible {
                        decls.types.insert(agg.name, key.clone());
                    }
                    self.aggregates.insert(
                        key.clone(),
                        AggregateSchema {
                            key,
                            kind: agg.kind,
                            generics: generic_params(&agg.type_params, &agg.const_params),
                            fields,
                            methods,
                        },
                    );
                }
                Stmt::Enum(enum_node) => {
                    let enm = &enum_node.node;
                    let key = NominalKey {
                        module: scope.clone(),
                        kind: NominalKind::Enum,
                        name: enm.name,
                    };
                    let mut variants = HashMap::new();
                    for variant in &enm.variants {
                        let schema = match &variant.kind {
                            VariantKind::Unit => VariantSchema::Unit,
                            VariantKind::Tuple(types) => VariantSchema::Tuple(types.clone()),
                            VariantKind::Struct(fields) => {
                                let mut field_map = HashMap::new();
                                for (i, f) in fields.iter().enumerate() {
                                    field_map.insert(
                                        f.name,
                                        FieldSchema {
                                            index: i,
                                            ty: f.ty.clone(),
                                            has_default: f.default.is_some(),
                                        },
                                    );
                                }
                                VariantSchema::Struct(field_map)
                            }
                        };
                        variants.insert(variant.name, schema);
                    }
                    if visible {
                        decls.types.insert(enm.name, key.clone());
                    }
                    self.enums.insert(
                        key.clone(),
                        EnumSchema {
                            key,
                            generics: generic_params(&enm.type_params, &enm.const_params),
                            variants,
                        },
                    );
                }
                Stmt::ExternFunc(ext_node) => {
                    let ext = &ext_node.node;
                    if !visible {
                        continue;
                    }
                    let ty = func_type_from_params(&ext.params, &ext.ret);
                    decls.values.insert(
                        ext.name,
                        ValueDecl::Func(FuncSig {
                            generics: GenericParams::default(),
                            ty,
                        }),
                    );
                }
                Stmt::ExternType(ext_node) => {
                    let ext = &ext_node.node;
                    if !visible {
                        continue;
                    }
                    let key = NominalKey {
                        module: scope.clone(),
                        kind: NominalKind::Extern,
                        name: ext.name,
                    };
                    decls.types.insert(ext.name, key);
                }
                Stmt::Const(const_node) => {
                    let c = &const_node.node;
                    if !visible {
                        continue;
                    }
                    let ty = c.ty.clone().unwrap_or(Type::Infer);
                    decls.values.insert(c.name, ValueDecl::Const(ty));
                }
                Stmt::Extend(extend_node) => {
                    let id = ExtendId {
                        module: scope.clone(),
                        index: extend_index,
                    };
                    extend_index += 1;
                    if !visible {
                        continue;
                    }
                    let ext = &extend_node.node;
                    let target = ext.ty.clone();
                    let mut methods = HashMap::new();
                    for method_node in &ext.methods {
                        let m = &method_node.node;
                        let params = m.params.split_first().map_or(&[][..], |(_, params)| params);
                        methods.insert(
                            m.name,
                            ExtendMethodSchema {
                                generics: GenericParams::default(),
                                params: resolve_func_params(params),
                                ret: m.ret.clone(),
                            },
                        );
                    }
                    self.extends.push(ExtendSchema {
                        id,
                        origin: scope.clone(),
                        target,
                        generics: generic_params(&ext.type_params, &ext.const_params),
                        methods,
                    });
                }
                _ => {}
            }
        }

        self.modules.insert(scope, decls);
    }

    pub(crate) fn type_in_module(&self, scope: &ModuleScope, name: Ident) -> Option<&NominalKey> {
        self.modules.get(scope)?.types.get(&name)
    }

    pub(crate) fn value_in_module(&self, scope: &ModuleScope, name: Ident) -> Option<&ValueDecl> {
        self.modules.get(scope)?.values.get(&name)
    }

    pub(crate) fn set_const_type(&mut self, scope: &ModuleScope, name: Ident, ty: Type) {
        let Some(ValueDecl::Const(existing)) = self
            .modules
            .get_mut(scope)
            .and_then(|decls| decls.values.get_mut(&name))
        else {
            return;
        };
        *existing = ty;
    }

    pub(crate) fn aggregate(&self, key: &NominalKey) -> Option<&AggregateSchema> {
        self.aggregates.get(key)
    }

    pub(crate) fn enum_schema(&self, key: &NominalKey) -> Option<&EnumSchema> {
        self.enums.get(key)
    }

    pub(crate) fn key_for_type(&self, ty: &Type) -> Option<NominalKey> {
        let Type::Nominal(nominal) = ty else {
            return None;
        };
        let name = nominal.name;
        let origin = nominal.origin.clone();

        if let Some(origin) = origin {
            let scope = ModuleScope::Named(ModulePath::new(origin.iter().cloned().collect()));
            if let Some(key) = self.type_in_module(&scope, name) {
                return Some(key.clone());
            }
        }

        if let Some(key) = self.type_in_module(&ModuleScope::Root, name) {
            return Some(key.clone());
        }

        let mut found = None;
        for decls in self.modules.values() {
            let Some(key) = decls.types.get(&name) else {
                continue;
            };
            if found.is_some() {
                return None;
            }
            found = Some(key.clone());
        }
        found
    }

    pub(crate) fn aggregate_field_type(&self, receiver: &Type, name: Ident) -> Option<Type> {
        let key = self.key_for_type(receiver)?;
        let agg = self.aggregate(&key)?;
        let field = agg.fields.get(&name)?;
        Some(substitute_aggregate_member(
            receiver,
            &agg.generics,
            &field.ty,
        ))
    }

    pub(crate) fn extends(&self) -> impl Iterator<Item = &ExtendSchema> {
        self.extends.iter()
    }

    pub(crate) fn extends_for(&self, receiver: &Type) -> impl Iterator<Item = &ExtendSchema> {
        self.extends().filter(move |e| &e.target == receiver)
    }

    pub(crate) fn find_extend_method<F>(
        &self,
        receiver: &Type,
        name: Ident,
        mut visible: F,
    ) -> Option<ExtendMethodMatch<'_>>
    where
        F: FnMut(&ExtendSchema) -> bool,
    {
        let mut candidates = vec![];

        for ext in self.extends() {
            if !visible(ext) {
                continue;
            }
            let Some(method) = ext.methods.get(&name) else {
                continue;
            };
            let target = generic_template_type(&ext.target, &ext.generics);
            let mut inf = Inference::new();
            if infer(&target, receiver, &mut inf) {
                candidates.push((ext, method, target));
            }
        }

        match candidates.len() {
            0 => None,
            1 => {
                let (extend, method, _) = candidates.remove(0);
                Some(ExtendMethodMatch::Match { extend, method })
            }
            _ => Some(most_specific_extend(candidates)),
        }
    }
}

fn substitute_aggregate_member(receiver: &Type, generics: &GenericParams, ty: &Type) -> Type {
    let Some(receiver) = receiver.as_aggregate() else {
        return ty.clone();
    };
    let type_subst: TypeSubst = generics
        .type_params
        .iter()
        .zip(receiver.type_args)
        .map(|(param, arg)| (param.id, arg.clone()))
        .collect();
    let const_subst: ConstSubst = generics
        .const_params
        .iter()
        .zip(receiver.const_args)
        .filter_map(|(param, arg)| const_arg_usize(arg).map(|value| (param.id, value)))
        .collect();
    let has_substitutions = !type_subst.is_empty() || !const_subst.is_empty();
    if !has_substitutions {
        return ty.clone();
    }
    substitute(ty, &type_subst, &const_subst)
}

type ExtendCandidate<'a> = (&'a ExtendSchema, &'a ExtendMethodSchema, Type);

fn most_specific_extend(mut candidates: Vec<ExtendCandidate<'_>>) -> ExtendMethodMatch<'_> {
    let winner = (1..candidates.len()).fold(0, |best, i| {
        let target = &candidates[i].2;
        let best_target = &candidates[best].2;
        if more_specific(target, best_target) {
            i
        } else {
            best
        }
    });

    let winner_target = &candidates[winner].2;
    let dominates_all = candidates
        .iter()
        .enumerate()
        .all(|(i, candidate)| i == winner || more_specific(winner_target, &candidate.2));
    if !dominates_all {
        return ExtendMethodMatch::Ambiguous;
    }

    let (extend, method, _) = candidates.swap_remove(winner);
    ExtendMethodMatch::Match { extend, method }
}

fn more_specific(a: &Type, b: &Type) -> bool {
    compare_specificity(a, b) == Specificity::MoreSpecific
}

struct GenericTemplate<'a> {
    generics: &'a GenericParams,
}

impl TypeFolder for GenericTemplate<'_> {
    fn fold_unresolved_name(&mut self, name: Ident) -> Type {
        self.generics
            .type_params
            .iter()
            .find(|param| param.name == name)
            .map_or(Type::UnresolvedName(name), |param| Type::Var(param.id))
    }

    fn fold_unresolved_nominal(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        if qualifier.is_none()
            && generic_args.is_empty()
            && let Some(param) = self
                .generics
                .type_params
                .iter()
                .find(|param| param.name == name)
        {
            return Type::Var(param.id);
        }
        self.fold_unresolved_nominal_default(qualifier, name, generic_args)
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Name(name) => self
                .generics
                .const_params
                .iter()
                .find(|param| param.name == *name)
                .map_or_else(|| arg.clone(), |param| ConstArg::Param(param.id)),
            ConstArg::Value(_) | ConstArg::Param(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Named(name) => self
                .generics
                .const_params
                .iter()
                .find(|param| param.name == name)
                .map_or(ArrayLen::Named(name), |param| ArrayLen::Param(param.id)),
            other => other,
        }
    }
}

pub(crate) fn generic_template_type(ty: &Type, generics: &GenericParams) -> Type {
    GenericTemplate { generics }.fold_type(ty)
}

struct NominalResolver<'a> {
    scope: &'a ModuleScope,
    scoped: &'a HashMap<(ModuleScope, Ident), (Type, GenericParams)>,
    fallback: &'a HashMap<Ident, (Type, GenericParams)>,
}

impl TypeFolder for NominalResolver<'_> {
    fn fold_unresolved_name(&mut self, name: Ident) -> Type {
        self.scoped
            .get(&(self.scope.clone(), name))
            .or_else(|| self.fallback.get(&name))
            .map_or(Type::UnresolvedName(name), |(ty, _)| ty.clone())
    }

    fn fold_unresolved_nominal(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        let resolved = self
            .scoped
            .get(&(self.scope.clone(), name))
            .cloned()
            .or_else(|| self.fallback.get(&name).cloned());

        if let Some((base, generics)) = resolved {
            if let Some(ty) = self.merge_generic_args(base, &generics, generic_args) {
                return ty;
            }
        }

        self.fold_unresolved_nominal_default(qualifier, name, generic_args)
    }
}

impl NominalResolver<'_> {
    fn bind_nominal_args(
        &mut self,
        generics: &GenericParams,
        args: &[GenericArg],
    ) -> Option<(Vec<Type>, Vec<ConstArg>)> {
        if args.len() != (generics.type_params.len() + generics.const_params.len()) {
            return None;
        }

        let mut type_args = Vec::with_capacity(generics.type_params.len());
        let mut const_args = Vec::with_capacity(generics.const_params.len());
        for (index, arg) in args.iter().enumerate() {
            if index < generics.type_params.len() {
                let GenericArg::Type(ty) = arg else {
                    return None;
                };
                type_args.push(self.fold_type(ty));
            } else {
                match arg {
                    GenericArg::Const(arg) => const_args.push(arg.clone()),
                    GenericArg::Type(ty) => const_args.push(ConstArg::Name(bare_type_name(ty)?)),
                }
            }
        }
        Some((type_args, const_args))
    }

    fn merge_generic_args(
        &mut self,
        base: Type,
        generics: &GenericParams,
        args: &[GenericArg],
    ) -> Option<Type> {
        let (type_args, const_args) = self.bind_nominal_args(generics, args)?;
        Some(match base {
            Type::Nominal(nominal) if nominal.kind == NominalKind::Extern => {
                if !type_args.is_empty() || !const_args.is_empty() {
                    return None;
                }
                Type::nominal(nominal.kind, nominal.name, vec![], vec![], nominal.origin)
            }
            Type::Nominal(nominal) => Type::nominal(
                nominal.kind,
                nominal.name,
                type_args,
                const_args,
                nominal.origin,
            ),
            other if args.is_empty() => other,
            _ => return None,
        })
    }
}

fn generic_params(type_params: &[TypeParam], const_params: &[ConstParam]) -> GenericParams {
    GenericParams {
        type_params: type_params.to_vec(),
        const_params: const_params.to_vec(),
    }
}

fn func_type_from_params(params: &[Param], ret: &Type) -> Type {
    let resolved_params = params
        .iter()
        .map(|p| FuncParam::new(p.ty.clone(), matches!(p.mutability, Mutability::Mutable)))
        .collect();
    Type::Func {
        params: resolved_params,
        ret: Box::new(ret.clone()),
    }
}

fn resolve_func_params(params: &[Param]) -> Vec<FuncParam> {
    params
        .iter()
        .map(|p| FuncParam::new(p.ty.clone(), matches!(p.mutability, Mutability::Mutable)))
        .collect()
}

fn stmt_visibility(stmt: &StmtNode) -> Visibility {
    match &stmt.node {
        Stmt::ExternFunc(_) | Stmt::ExternType(_) => Visibility::Public,
        Stmt::Extend(n) => n.node.visibility,
        Stmt::Func(n) => n.node.visibility,
        Stmt::Aggregate(n) => n.node.visibility,
        Stmt::Enum(n) => n.node.visibility,
        Stmt::Const(n) => n.node.visibility,
        _ => Visibility::Private,
    }
}

fn resolve_nominal(
    scope: &ModuleScope,
    ty: &Type,
    scoped: &HashMap<(ModuleScope, Ident), (Type, GenericParams)>,
    fallback: &HashMap<Ident, (Type, GenericParams)>,
) -> Type {
    let mut resolver = NominalResolver {
        scope,
        scoped,
        fallback,
    };
    resolver.fold_type(ty)
}

pub(crate) fn nominal_type(key: &NominalKey) -> Type {
    nominal_type_with_args(key, &[], &[])
}

pub(crate) fn nominal_type_with_args(
    key: &NominalKey,
    type_args: &[Type],
    const_args: &[ConstArg],
) -> Type {
    Type::nominal(
        key.kind,
        key.name,
        type_args.to_vec(),
        const_args.to_vec(),
        key.module.named_path(),
    )
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use crate::ast::TypeVarId;

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    #[test]
    fn generic_template_keeps_nominal_with_args() {
        let generics = GenericParams {
            type_params: vec![TypeParam {
                name: ident("T"),
                id: TypeVarId(0),
            }],
            const_params: vec![],
        };
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: ident("Foo"),
            generic_args: vec![GenericArg::Type(Type::UnresolvedName(ident("T")))],
        };
        let result = generic_template_type(&ty, &generics);

        assert_eq!(
            result,
            Type::UnresolvedNominal {
                qualifier: None,
                name: ident("Foo"),
                generic_args: vec![GenericArg::Type(Type::Var(TypeVarId(0)))],
            }
        );
    }

    #[test]
    fn type_args_origin() {
        let name = ident("Box");
        let scope = ModuleScope::Named(ModulePath::new(vec!["tools".into()]));
        let key = NominalKey {
            module: scope.clone(),
            kind: NominalKind::Struct,
            name,
        };
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name,
            generic_args: vec![GenericArg::Type(Type::Int)],
        };
        let generics = GenericParams {
            type_params: vec![TypeParam {
                name: ident("T"),
                id: TypeVarId(0),
            }],
            const_params: vec![],
        };
        let scoped = HashMap::from([((scope.clone(), name), (nominal_type(&key), generics))]);
        let result = resolve_nominal(&scope, &ty, &scoped, &HashMap::new());

        assert_eq!(
            result,
            Type::nominal(
                NominalKind::Struct,
                name,
                vec![Type::Int],
                vec![],
                Some(std::rc::Rc::new(["tools".into()])),
            )
        );
    }

    #[test]
    fn nested_type_args() {
        let wrapper = ident("Wrapper");
        let inner = ident("Inner");
        let scope = ModuleScope::Root;
        let wrapper_key = NominalKey {
            module: scope.clone(),
            kind: NominalKind::Struct,
            name: wrapper,
        };
        let inner_key = NominalKey {
            module: scope.clone(),
            kind: NominalKind::Struct,
            name: inner,
        };
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: wrapper,
            generic_args: vec![GenericArg::Type(Type::UnresolvedName(inner))],
        };
        let wrapper_generics = GenericParams {
            type_params: vec![TypeParam {
                name: ident("T"),
                id: TypeVarId(0),
            }],
            const_params: vec![],
        };
        let scoped = HashMap::from([
            (
                (scope.clone(), wrapper),
                (nominal_type(&wrapper_key), wrapper_generics),
            ),
            (
                (scope.clone(), inner),
                (nominal_type(&inner_key), GenericParams::default()),
            ),
        ]);
        let result = resolve_nominal(&scope, &ty, &scoped, &HashMap::new());

        assert_eq!(
            result,
            Type::nominal(
                NominalKind::Struct,
                wrapper,
                vec![Type::nominal(
                    NominalKind::Struct,
                    inner,
                    vec![],
                    vec![],
                    None
                )],
                vec![],
                None,
            )
        );
    }

    #[test]
    fn unresolved_qualifier() {
        let ty = Type::UnresolvedNominal {
            qualifier: Some(ident("gamekit")),
            name: ident("Thing"),
            generic_args: vec![GenericArg::Type(Type::Int)],
        };
        let result = resolve_nominal(&ModuleScope::Root, &ty, &HashMap::new(), &HashMap::new());

        assert_eq!(result, ty);
    }
}
