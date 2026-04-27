use std::collections::{HashMap, HashSet};

use super::{
    ConstSubst, GenericArgs, GenericParams, Specificity, TypeSubst, compare_specificity,
    const_term::ConstTerm,
    generic_bind::bind_exact_generic_args_no_diag,
    infer::{GenericSolverSeeds, Solver},
    substitute,
    type_ops::TypeFolder,
};
use crate::{
    ast::{
        AggregateKind, ArrayLen, ConstArg, ConstParam, FuncParam, GenericArg, Ident,
        ImportItemKind, ImportKind, MethodReceiver, Mutability, NominalKind, Param, Program, Stmt,
        StmtNode, Type, TypeParam, VariantKind, Visibility,
    },
    resolve::{ModuleKey, ModulePath, ResolveResult},
    span::Span,
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

#[derive(Debug, Clone)]
pub(crate) struct ResolvedValue {
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
    pub(crate) decl: ValueDecl,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Namespace {
    pub(crate) values: HashMap<Ident, ResolvedValue>,
    pub(crate) types: HashMap<Ident, NominalKey>,
    pub(crate) modules: HashMap<Ident, ModuleScope>,
}

pub(crate) type ModuleExports = Namespace;

#[derive(Debug, Clone, Default)]
pub(crate) struct ImportScope {
    pub(crate) namespace: Namespace,
    pub(crate) active_modules: HashSet<ModuleScope>,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ModuleDecls {
    locals: Namespace,
    exports: ModuleExports,
    imports: ImportScope,
}

#[derive(Debug, Clone)]
pub(crate) enum ValueDecl {
    Func(FuncSig),
    Const(Type),
}

impl Namespace {
    fn value(&self, name: Ident) -> Option<&ResolvedValue> {
        self.values.get(&name)
    }

    fn ty(&self, name: Ident) -> Option<&NominalKey> {
        self.types.get(&name)
    }

    fn module(&self, name: Ident) -> Option<&ModuleScope> {
        self.modules.get(&name)
    }

    fn insert_value(&mut self, visible: Ident, value: ResolvedValue) {
        self.values.insert(visible, value);
    }

    fn insert_type(&mut self, visible: Ident, key: NominalKey) {
        self.types.insert(visible, key);
    }

    fn insert_module(&mut self, visible: Ident, module: ModuleScope) {
        self.modules.insert(visible, module);
    }
}

impl ImportScope {
    fn activate_imported_origins(&mut self) {
        self.active_modules.extend(
            self.namespace
                .values
                .values()
                .map(|value| value.module.clone()),
        );
        self.active_modules
            .extend(self.namespace.types.values().map(|key| key.module.clone()));
        self.active_modules
            .extend(self.namespace.modules.values().cloned());
    }
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
        owner_args: Result<GenericArgs, Vec<Ident>>,
    },
    Ambiguous,
}

impl DeclarationIndex {
    pub(crate) fn from_root(program: &Program) -> Self {
        let mut index = Self::new();
        let modules = [(ModuleScope::Root, program)];
        for (scope, program) in &modules {
            index.collect_module(program, scope.clone(), true);
        }
        index.apply_public_import_reexports(&modules);
        index.build_import_scopes(&modules);
        index.resolve_nominals();
        index
    }

    pub(crate) fn from_root_and_modules(root: &Program, resolved: &ResolveResult) -> Self {
        let mut index = Self::new();
        let modules = Self::module_programs(root, resolved);
        for (scope, program) in &modules {
            index.collect_module(program, scope.clone(), matches!(scope, ModuleScope::Root));
        }
        index.apply_public_import_reexports(&modules);
        index.build_import_scopes(&modules);
        index.resolve_nominals();
        index
    }

    fn module_programs<'a>(
        root: &'a Program,
        resolved: &'a ResolveResult,
    ) -> Vec<(ModuleScope, &'a Program)> {
        let mut modules = vec![(ModuleScope::Root, root)];
        for group in &resolved.module_groups {
            for module in group {
                let ModuleKey::Named(path) = &module.key else {
                    continue;
                };
                modules.push((ModuleScope::Named(path.clone()), &module.program));
            }
        }
        modules
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
        for decls in self.modules.values_mut() {
            Self::resolve_namespace_values(&mut decls.locals, &scoped, &fallback);
            Self::resolve_namespace_values(&mut decls.exports, &scoped, &fallback);
            Self::resolve_namespace_values(&mut decls.imports.namespace, &scoped, &fallback);
        }
    }

    fn resolve_namespace_values(
        namespace: &mut Namespace,
        scoped: &HashMap<(ModuleScope, Ident), (Type, GenericParams)>,
        fallback: &HashMap<Ident, (Type, GenericParams)>,
    ) {
        for value in namespace.values.values_mut() {
            Self::resolve_value_decl(&value.module, &mut value.decl, scoped, fallback);
        }
    }

    fn resolve_value_decl(
        scope: &ModuleScope,
        decl: &mut ValueDecl,
        scoped: &HashMap<(ModuleScope, Ident), (Type, GenericParams)>,
        fallback: &HashMap<Ident, (Type, GenericParams)>,
    ) {
        match decl {
            ValueDecl::Func(sig) => {
                sig.ty = resolve_nominal(scope, &sig.ty, scoped, fallback);
            }
            ValueDecl::Const(ty) => {
                *ty = resolve_nominal(scope, ty, scoped, fallback);
            }
        }
    }

    fn collect_module(&mut self, program: &Program, scope: ModuleScope, export_all: bool) {
        let mut decls = ModuleDecls::default();
        let mut extend_index = 0;

        for stmt in &program.stmts {
            let exported = export_all || matches!(stmt_visibility(stmt), Visibility::Public);
            match &stmt.node {
                Stmt::Func(func_node) => {
                    let func = &func_node.node;
                    let ty = func_type_from_params(&func.params, &func.ret);
                    let value = ResolvedValue {
                        module: scope.clone(),
                        name: func.name,
                        decl: ValueDecl::Func(FuncSig {
                            generics: generic_params(&func.type_params, &func.const_params),
                            ty,
                        }),
                    };
                    decls.locals.insert_value(func.name, value.clone());
                    if exported {
                        decls.exports.insert_value(func.name, value);
                    }
                }
                Stmt::Aggregate(agg_node) => {
                    let agg = &agg_node.node;
                    let key = NominalKey {
                        module: scope.clone(),
                        kind: agg.kind.into(),
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
                    decls.locals.insert_type(agg.name, key.clone());
                    if exported {
                        decls.exports.insert_type(agg.name, key.clone());
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
                    decls.locals.insert_type(enm.name, key.clone());
                    if exported {
                        decls.exports.insert_type(enm.name, key.clone());
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
                    let ty = func_type_from_params(&ext.params, &ext.ret);
                    let value = ResolvedValue {
                        module: scope.clone(),
                        name: ext.name,
                        decl: ValueDecl::Func(FuncSig {
                            generics: GenericParams::default(),
                            ty,
                        }),
                    };
                    decls.locals.insert_value(ext.name, value.clone());
                    if exported {
                        decls.exports.insert_value(ext.name, value);
                    }
                }
                Stmt::ExternType(ext_node) => {
                    let ext = &ext_node.node;
                    let key = NominalKey {
                        module: scope.clone(),
                        kind: NominalKind::Extern,
                        name: ext.name,
                    };
                    decls.locals.insert_type(ext.name, key.clone());
                    if exported {
                        decls.exports.insert_type(ext.name, key);
                    }
                }
                Stmt::Const(const_node) => {
                    let c = &const_node.node;
                    let value = ResolvedValue {
                        module: scope.clone(),
                        name: c.name,
                        decl: ValueDecl::Const(c.ty.clone().unwrap_or(Type::Infer)),
                    };
                    decls.locals.insert_value(c.name, value.clone());
                    if exported {
                        decls.exports.insert_value(c.name, value);
                    }
                }
                Stmt::Extend(extend_node) => {
                    let id = ExtendId {
                        module: scope.clone(),
                        index: extend_index,
                    };
                    extend_index += 1;
                    if !exported {
                        continue;
                    }
                    let ext = &extend_node.node;
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
                        target: ext.ty.clone(),
                        generics: generic_params(&ext.type_params, &ext.const_params),
                        methods,
                    });
                }
                _ => {}
            }
        }

        self.modules.insert(scope, decls);
    }

    fn apply_public_import_reexports(&mut self, modules: &[(ModuleScope, &Program)]) {
        for (scope, program) in modules {
            let Some(mut exports) = self.modules.get(scope).map(|decls| decls.exports.clone())
            else {
                continue;
            };

            for stmt in &program.stmts {
                let Stmt::Import(import) = &stmt.node else {
                    continue;
                };
                if !matches!(import.node.visibility, Visibility::Public) {
                    continue;
                }
                let path = ModulePath::from_idents(&import.node.path);
                let dep_scope = ModuleScope::Named(path.clone());
                let dep_exports = self.modules.get(&dep_scope).map(|decls| &decls.exports);
                Self::apply_import(&import.node.kind, &path, dep_exports, &mut exports);
            }

            if let Some(decls) = self.modules.get_mut(scope) {
                decls.exports = exports;
            }
        }
    }

    fn build_import_scopes(&mut self, modules: &[(ModuleScope, &Program)]) {
        for (scope, program) in modules {
            let mut imports = ImportScope::default();
            for stmt in &program.stmts {
                let Stmt::Import(import) = &stmt.node else {
                    continue;
                };
                let path = ModulePath::from_idents(&import.node.path);
                let dep_scope = ModuleScope::Named(path.clone());
                imports.active_modules.insert(dep_scope.clone());
                let dep_exports = self.modules.get(&dep_scope).map(|decls| &decls.exports);
                Self::apply_import(
                    &import.node.kind,
                    &path,
                    dep_exports,
                    &mut imports.namespace,
                );
            }
            imports.activate_imported_origins();
            if let Some(decls) = self.modules.get_mut(scope) {
                decls.imports = imports;
            }
        }
    }

    fn apply_import(
        kind: &ImportKind,
        path: &ModulePath,
        dep_exports: Option<&Namespace>,
        namespace: &mut Namespace,
    ) {
        let module = ModuleScope::Named(path.clone());
        match kind {
            ImportKind::Module => {
                if let Some(alias) = path.segments().last() {
                    namespace.insert_module(Ident::new(alias.as_str()), module);
                }
            }
            ImportKind::ModuleAs(alias) => {
                namespace.insert_module(*alias, module);
            }
            ImportKind::Selective(items) => {
                for item in items {
                    let target = item.alias.unwrap_or_else(|| match item.kind {
                        ImportItemKind::Name(name) => name,
                        ImportItemKind::SelfModule => path
                            .segments()
                            .last()
                            .map_or_else(|| Ident::new(""), |segment| Ident::new(segment.as_str())),
                    });
                    match item.kind {
                        ImportItemKind::SelfModule => {
                            namespace.insert_module(target, module.clone());
                        }
                        ImportItemKind::Name(name) => {
                            if let Some(dep_exports) = dep_exports {
                                Self::copy_named_members(dep_exports, name, target, namespace);
                            }
                        }
                    }
                }
            }
            ImportKind::Wildcard => {
                if let Some(dep_exports) = dep_exports {
                    Self::copy_wildcard_members(dep_exports, namespace);
                }
            }
        }
    }

    fn copy_named_members(
        source: &Namespace,
        source_name: Ident,
        target_name: Ident,
        dest: &mut Namespace,
    ) {
        if let Some(key) = source.ty(source_name).cloned() {
            dest.insert_type(target_name, key);
        }
        if let Some(value) = source.value(source_name).cloned() {
            dest.insert_value(target_name, value);
        }
        if let Some(module) = source.module(source_name).cloned() {
            dest.insert_module(target_name, module);
        }
    }

    fn copy_wildcard_members(source: &Namespace, dest: &mut Namespace) {
        for (name, key) in &source.types {
            dest.types.entry(*name).or_insert_with(|| key.clone());
        }
        for (name, value) in &source.values {
            dest.values.entry(*name).or_insert_with(|| value.clone());
        }
        for (name, module) in &source.modules {
            dest.modules.entry(*name).or_insert_with(|| module.clone());
        }
    }

    pub(crate) fn local_value(&self, module: &ModuleScope, name: Ident) -> Option<ResolvedValue> {
        self.modules.get(module)?.locals.value(name).cloned()
    }

    pub(crate) fn local_type(&self, module: &ModuleScope, name: Ident) -> Option<NominalKey> {
        self.modules.get(module)?.locals.ty(name).cloned()
    }

    pub(crate) fn exported_value(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<ResolvedValue> {
        self.modules.get(module)?.exports.value(name).cloned()
    }

    pub(crate) fn exported_type(&self, module: &ModuleScope, name: Ident) -> Option<NominalKey> {
        self.modules.get(module)?.exports.ty(name).cloned()
    }

    pub(crate) fn exported_module(&self, module: &ModuleScope, name: Ident) -> Option<ModuleScope> {
        self.modules.get(module)?.exports.module(name).cloned()
    }

    pub(crate) fn imported_value(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<ResolvedValue> {
        self.modules
            .get(module)?
            .imports
            .namespace
            .value(name)
            .cloned()
    }

    pub(crate) fn imported_type(&self, module: &ModuleScope, name: Ident) -> Option<NominalKey> {
        self.modules
            .get(module)?
            .imports
            .namespace
            .ty(name)
            .cloned()
    }

    pub(crate) fn imported_module(&self, module: &ModuleScope, name: Ident) -> Option<ModuleScope> {
        self.modules
            .get(module)?
            .imports
            .namespace
            .module(name)
            .cloned()
    }

    pub(crate) fn visible_type(&self, module: &ModuleScope, name: Ident) -> Option<NominalKey> {
        self.local_type(module, name)
            .or_else(|| self.imported_type(module, name))
    }

    pub(crate) fn imports_module(&self, module: &ModuleScope, imported: &ModuleScope) -> bool {
        self.modules
            .get(module)
            .is_some_and(|decls| decls.imports.active_modules.contains(imported))
    }

    pub(crate) fn set_const_type(&mut self, scope: &ModuleScope, name: Ident, ty: Type) {
        for decls in self.modules.values_mut() {
            Self::set_namespace_const_type(&mut decls.locals, scope, name, &ty);
            Self::set_namespace_const_type(&mut decls.exports, scope, name, &ty);
            Self::set_namespace_const_type(&mut decls.imports.namespace, scope, name, &ty);
        }
    }

    fn set_namespace_const_type(
        namespace: &mut Namespace,
        scope: &ModuleScope,
        name: Ident,
        ty: &Type,
    ) {
        for value in namespace.values.values_mut() {
            if value.module == *scope
                && value.name == name
                && let ValueDecl::Const(existing) = &mut value.decl
            {
                *existing = ty.clone();
            }
        }
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
            if let Some(key) = self.local_type(&scope, name) {
                return Some(key);
            }
        }

        if let Some(key) = self.local_type(&ModuleScope::Root, name) {
            return Some(key);
        }

        let mut found = None;
        for decls in self.modules.values() {
            let Some(key) = decls.exports.ty(name) else {
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
            let Some(owner_args) =
                match_generic_template_args(&ext.generics, &target, receiver, Span::new(0, 0))
            else {
                continue;
            };
            candidates.push(ExtendCandidate {
                extend: ext,
                method,
                target,
                owner_args,
            });
        }

        match candidates.len() {
            0 => None,
            1 => {
                let candidate = candidates.remove(0);
                Some(ExtendMethodMatch::Match {
                    extend: candidate.extend,
                    method: candidate.method,
                    owner_args: candidate.owner_args,
                })
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
        .map(|(param, arg)| (param.id, ConstTerm::from_arg(arg)))
        .collect();
    let has_substitutions = !type_subst.is_empty() || !const_subst.is_empty();
    if !has_substitutions {
        return ty.clone();
    }
    substitute(ty, &type_subst, &const_subst)
}

struct ExtendCandidate<'a> {
    extend: &'a ExtendSchema,
    method: &'a ExtendMethodSchema,
    target: Type,
    owner_args: Result<GenericArgs, Vec<Ident>>,
}

type GenericTemplateMatch = Result<GenericArgs, Vec<Ident>>;

fn match_generic_template_args(
    generics: &GenericParams,
    template: &Type,
    concrete: &Type,
    span: Span,
) -> Option<GenericTemplateMatch> {
    if generics.is_empty() {
        return (template == concrete).then(|| Ok(GenericArgs::empty()));
    }

    let mut solver = Solver::default();
    let seeds = GenericSolverSeeds::default();
    let vars = solver.generic_solver_vars(generics, &seeds, span);
    let template = solver.instantiate_generic_type(template, &vars);
    let concrete = solver.concrete_type(concrete);
    solver.add_handle_equal(span, template, concrete);
    if !solver.solve_pending().is_empty() {
        return None;
    }

    Some(solver.finalize_generic_args(generics, &vars))
}

fn most_specific_extend(mut candidates: Vec<ExtendCandidate<'_>>) -> ExtendMethodMatch<'_> {
    let winner = (1..candidates.len()).fold(0, |best, i| {
        if more_specific(&candidates[i].target, &candidates[best].target) {
            i
        } else {
            best
        }
    });

    let winner_target = &candidates[winner].target;
    let dominates_all = candidates
        .iter()
        .enumerate()
        .all(|(i, candidate)| i == winner || more_specific(winner_target, &candidate.target));
    if !dominates_all {
        return ExtendMethodMatch::Ambiguous;
    }

    let candidate = candidates.swap_remove(winner);
    ExtendMethodMatch::Match {
        extend: candidate.extend,
        method: candidate.method,
        owner_args: candidate.owner_args,
    }
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
    fn merge_generic_args(
        &mut self,
        base: Type,
        generics: &GenericParams,
        args: &[GenericArg],
    ) -> Option<Type> {
        let args = bind_exact_generic_args_no_diag(generics, args, |ty| self.fold_type(ty))?;
        let const_args = ConstTerm::to_args_no_infer(&args.const_args)?;
        Some(match base {
            Type::Nominal(nominal) if nominal.kind == NominalKind::Extern => {
                if !args.type_args.is_empty() || !const_args.is_empty() {
                    return None;
                }
                Type::nominal(nominal.kind, nominal.name, vec![], vec![], nominal.origin)
            }
            Type::Nominal(nominal) => Type::nominal(
                nominal.kind,
                nominal.name,
                args.type_args,
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
    use crate::{ast::TypeVarId, lexer::tokenize, parser, resolve::ResolvedModule};

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn parse(source: &str) -> Program {
        let tokens = tokenize(source).expect("lexer error");
        parser::parse_ast(&tokens).expect("parse error")
    }

    fn scope(name: &str) -> ModuleScope {
        ModuleScope::Named(ModulePath::new(vec![name.to_string()]))
    }

    fn index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse(root);
        let resolved = ResolveResult {
            module_groups: modules
                .iter()
                .map(|(name, source)| {
                    vec![ResolvedModule {
                        key: ModuleKey::Named(ModulePath::new(vec![(*name).to_string()])),
                        program: parse(source),
                    }]
                })
                .collect(),
        };
        DeclarationIndex::from_root_and_modules(&root, &resolved)
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

    #[test]
    fn module_locals_include_private_but_exports_do_not() {
        let index = index("", &[("tools", "fn hidden() {} pub fn shown() {}")]);
        let tools = scope("tools");

        assert!(index.local_value(&tools, ident("hidden")).is_some());
        assert!(index.exported_value(&tools, ident("hidden")).is_none());
        assert!(index.exported_value(&tools, ident("shown")).is_some());
    }

    #[test]
    fn value_reexport_preserves_origin() {
        let index = index(
            "",
            &[
                ("tools", "pub fn id<T>(x: T) -> T { x }"),
                ("facade", "pub import tools { id as dup };"),
            ],
        );
        let value = index
            .exported_value(&scope("facade"), ident("dup"))
            .expect("missing reexport");

        assert_eq!(value.module, scope("tools"));
        assert_eq!(value.name, ident("id"));
    }

    #[test]
    fn type_reexport_preserves_nominal_origin() {
        let index = index(
            "",
            &[
                ("tools", "pub struct Point { x: int }"),
                ("facade", "pub import tools { Point as P };"),
            ],
        );
        let key = index
            .exported_type(&scope("facade"), ident("P"))
            .expect("missing reexport");

        assert_eq!(key.module, scope("tools"));
        assert_eq!(key.name, ident("Point"));
        assert_eq!(key.kind, NominalKind::Struct);
    }

    #[test]
    fn import_alias_populates_module_namespace() {
        let index = index("import facade { self as f };", &[("facade", "")]);

        assert_eq!(
            index.imported_module(&ModuleScope::Root, ident("f")),
            Some(scope("facade")),
        );
    }

    #[test]
    fn imported_reexported_value_preserves_origin() {
        let index = index(
            "import facade { dup };",
            &[
                ("tools", "pub fn id<T>(x: T) -> T { x }"),
                ("facade", "pub import tools { id as dup };"),
            ],
        );
        let value = index
            .imported_value(&ModuleScope::Root, ident("dup"))
            .expect("missing import");

        assert_eq!(value.module, scope("tools"));
        assert_eq!(value.name, ident("id"));
    }

    #[test]
    fn imported_type_activates_direct_and_origin_modules() {
        let index = index(
            "import facade { P };",
            &[
                ("tools", "pub struct Point { x: int }"),
                ("facade", "pub import tools { Point as P };"),
            ],
        );

        assert!(index.imports_module(&ModuleScope::Root, &scope("facade")));
        assert!(index.imports_module(&ModuleScope::Root, &scope("tools")));
    }

    #[test]
    fn wildcard_import_keeps_first_value() {
        let index = index(
            "import a { * }; import b { * };",
            &[
                ("a", "pub fn dup() -> int { 1 }"),
                ("b", "pub fn dup() -> int { 2 }"),
            ],
        );
        let value = index
            .imported_value(&ModuleScope::Root, ident("dup"))
            .expect("missing import");

        assert_eq!(value.module, scope("a"));
    }

    #[test]
    fn set_const_type_updates_exported_and_imported_copies() {
        let mut index = index(
            "import tools { SIZE };",
            &[("tools", "pub const SIZE = 1;")],
        );
        index.set_const_type(&scope("tools"), ident("SIZE"), Type::Int);

        let exported = index
            .exported_value(&scope("tools"), ident("SIZE"))
            .expect("missing export");
        let imported = index
            .imported_value(&ModuleScope::Root, ident("SIZE"))
            .expect("missing import");

        assert!(matches!(exported.decl, ValueDecl::Const(Type::Int)));
        assert!(matches!(imported.decl, ValueDecl::Const(Type::Int)));
    }
}
