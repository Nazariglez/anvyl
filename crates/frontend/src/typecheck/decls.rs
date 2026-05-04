use std::collections::{HashMap, HashSet};

use super::{
    ConstSubst, GenericArgs, GenericParams, Specificity, TypeSubst, compare_specificity,
    const_term::ConstTerm,
    infer::{GenericSolverSeeds, Solver},
    substitute,
    type_ops::{TypeFolder, bare_type_name},
    type_refs::{GenericParamError, GenericTypeContext, TypeParamBinding},
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstParam, ConstParamId, FuncParam, GenericArg, Ident, ImportItemKind,
        ImportKind, MethodReceiver, ModuleOrigin, Mutability, NominalKind, Param, Program, Stmt,
        StmtNode, Type, TypeParam, VariantKind, Visibility,
    },
    externs::{RawExternModule, RawExterns, catalog::ExternCatalog, raw_module_scope},
    resolve::{ModuleId, ModulePath, PackageId, ResolveResult, SourceFileId},
    span::Span,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ModuleScope {
    Root,
    Named(ModulePath),
    Package(ModuleId),
}

impl ModuleScope {
    pub(crate) fn from_module_id(module: &ModuleId) -> Self {
        if module.source_file().is_some() {
            return Self::Package(module.clone());
        }
        if module.package_context() == Some(&PackageId::synthetic_root()) {
            match module.named_path() {
                Some(path) => Self::Named(path.clone()),
                None => Self::Root,
            }
        } else {
            Self::Package(module.clone())
        }
    }

    fn nominal_origin(&self) -> Option<ModuleOrigin> {
        match self {
            ModuleScope::Root => None,
            ModuleScope::Named(path) => Some(ModuleOrigin::Module(path.to_ast_path())),
            ModuleScope::Package(module) => match module.source_file() {
                Some(file) => Some(ModuleOrigin::SourceFile {
                    package: module
                        .package_context()
                        .map(|package| package.as_str().to_string()),
                    path: file.to_string(),
                }),
                None => Some(ModuleOrigin::Package {
                    package: module.package().as_str().to_string(),
                    path: module.named_path().map(ModulePath::to_ast_path),
                }),
            },
        }
    }

    fn from_nominal_origin(origin: &ModuleOrigin) -> Self {
        match origin {
            ModuleOrigin::Module(path) => Self::Named(
                ModulePath::new(path.iter().cloned().collect())
                    .expect("AST validates nominal origin module paths"),
            ),
            ModuleOrigin::SourceFile { package, path } => {
                Self::Package(ModuleId::source_with_context(
                    package.clone().map(PackageId::new),
                    SourceFileId::new(path).expect("source origin path is absolute"),
                ))
            }
            ModuleOrigin::Package { package, path } => {
                let package = PackageId::new(package.clone());
                match path {
                    Some(path) => Self::Package(ModuleId::named(
                        package,
                        ModulePath::new(path.iter().cloned().collect())
                            .expect("nominal origin module path is valid"),
                    )),
                    None => Self::Package(ModuleId::root(package)),
                }
            }
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

impl CallableId {
    pub(crate) fn function(module: ModuleScope, name: Ident) -> Self {
        Self {
            module,
            parent: None,
            kind: CallableKind::Function,
            name,
        }
    }

    pub(crate) fn extern_function(module: ModuleScope, name: Ident) -> Self {
        Self {
            module,
            parent: None,
            kind: CallableKind::ExternFunction,
            name,
        }
    }

    pub(crate) fn aggregate_method(owner: NominalKey, name: Ident, is_instance: bool) -> Self {
        Self {
            module: owner.module.clone(),
            parent: Some(CallableParent::Nominal(owner)),
            kind: if is_instance {
                CallableKind::InstanceMethod
            } else {
                CallableKind::StaticMethod
            },
            name,
        }
    }

    pub(crate) fn extend_method(extend: ExtendId, name: Ident) -> Self {
        Self {
            module: extend.module.clone(),
            parent: Some(CallableParent::Extend(extend)),
            kind: CallableKind::ExtendMethod,
            name,
        }
    }

    pub(crate) fn enum_variant(enum_key: NominalKey, variant: Ident) -> Self {
        Self {
            module: enum_key.module.clone(),
            parent: Some(CallableParent::Nominal(enum_key)),
            kind: CallableKind::EnumVariant,
            name: variant,
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum BindingNamespace {
    Value,
    Type,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BindingOrigin {
    Local,
    Import { source: ModuleScope },
    Reexport { source: ModuleScope },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum DeclError {
    DuplicateValue {
        module: ModuleScope,
        name: Ident,
        span: Span,
    },
    DuplicateType {
        module: ModuleScope,
        name: Ident,
        span: Span,
    },
    MissingImportMember {
        module: ModuleScope,
        imported: ModuleScope,
        name: Ident,
        span: Span,
    },
    PrivateImportMember {
        module: ModuleScope,
        imported: ModuleScope,
        name: Ident,
        span: Span,
    },
    ImportConflict {
        module: ModuleScope,
        name: Ident,
        namespace: BindingNamespace,
        first: BindingOrigin,
        second: BindingOrigin,
        span: Span,
    },
    DuplicateModuleBinding {
        module: ModuleScope,
        name: Ident,
        first: BindingOrigin,
        second: BindingOrigin,
        span: Span,
    },
    DuplicateGenericParam {
        module: ModuleScope,
        name: Ident,
        span: Span,
    },
    ReexportConflict {
        module: ModuleScope,
        name: Ident,
        namespace: BindingNamespace,
        first: BindingOrigin,
        second: BindingOrigin,
        span: Span,
    },
    UnknownType {
        module: ModuleScope,
        qualifier: Option<Ident>,
        name: Ident,
        span: Span,
    },
}

fn module_id_for_scope(scope: &ModuleScope) -> ModuleId {
    match scope {
        ModuleScope::Root => ModuleId::root(PackageId::synthetic_root()),
        ModuleScope::Named(path) => ModuleId::named(PackageId::synthetic_root(), path.clone()),
        ModuleScope::Package(module) => module.clone(),
    }
}

fn package_for_scope(scope: &ModuleScope) -> PackageId {
    match scope {
        ModuleScope::Package(module) => module
            .package_context()
            .cloned()
            .unwrap_or_else(PackageId::synthetic_root),
        ModuleScope::Root | ModuleScope::Named(_) => PackageId::synthetic_root(),
    }
}

#[derive(Clone, Default)]
pub(crate) struct DeclarationIndex {
    modules: HashMap<ModuleScope, ModuleDecls>,
    aggregates: HashMap<NominalKey, AggregateSchema>,
    enums: HashMap<NominalKey, EnumSchema>,
    extends: Vec<ExtendSchema>,
    value_spans: HashMap<(ModuleScope, Ident), Span>,
    type_spans: HashMap<NominalKey, Span>,
    always_active_modules: HashSet<ModuleScope>,
    errors: Vec<DeclError>,
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

type OriginKey = (BindingNamespace, Ident);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ImportMode {
    Import,
    Reexport,
}

struct ImportScopeBuilder {
    module: ModuleScope,
    namespace: Namespace,
    active_modules: HashSet<ModuleScope>,
    origins: HashMap<OriginKey, BindingOrigin>,
    errors: Vec<DeclError>,
    mode: ImportMode,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ImportScope {
    pub(crate) namespace: Namespace,
    pub(crate) active_modules: HashSet<ModuleScope>,
}

struct ImportTargetScope {
    module: ModuleScope,
    default_name: Ident,
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

    fn contains_member(&self, name: Ident) -> bool {
        self.values.contains_key(&name)
            || self.types.contains_key(&name)
            || self.modules.contains_key(&name)
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

impl ImportScopeBuilder {
    fn new(module: ModuleScope, mode: ImportMode) -> Self {
        Self {
            module,
            namespace: Namespace::default(),
            active_modules: HashSet::new(),
            origins: HashMap::new(),
            errors: vec![],
            mode,
        }
    }

    fn with_namespace(module: ModuleScope, namespace: Namespace, mode: ImportMode) -> Self {
        let mut builder = Self::new(module, mode);
        builder.namespace = namespace;
        builder.seed_existing_origins();
        builder
    }

    fn seed_existing_origins(&mut self) {
        let namespace = self.namespace.clone();
        self.seed_origins(&namespace, &BindingOrigin::Local);
    }

    fn seed_origins(&mut self, namespace: &Namespace, origin: &BindingOrigin) {
        self.origins.extend(
            namespace
                .values
                .keys()
                .map(|name| ((BindingNamespace::Value, *name), origin.clone())),
        );
        self.origins.extend(
            namespace
                .types
                .keys()
                .map(|name| ((BindingNamespace::Type, *name), origin.clone())),
        );
        self.origins.extend(
            namespace
                .modules
                .keys()
                .map(|name| ((BindingNamespace::Module, *name), origin.clone())),
        );
    }

    fn origin(&self, source: ModuleScope) -> BindingOrigin {
        match self.mode {
            ImportMode::Import => BindingOrigin::Import { source },
            ImportMode::Reexport => BindingOrigin::Reexport { source },
        }
    }

    fn apply_import(
        &mut self,
        kind: &ImportKind,
        source: ModuleScope,
        default_name: Ident,
        dep: Option<&ModuleDecls>,
        span: Span,
        validate_members: bool,
    ) {
        match kind {
            ImportKind::Module => {
                self.insert_module(default_name, source.clone(), self.origin(source), span);
            }
            ImportKind::ModuleAs(alias) => {
                self.insert_module(*alias, source.clone(), self.origin(source), span);
            }
            ImportKind::Selective(items) => {
                for item in items {
                    let target = item.alias.unwrap_or(match item.kind {
                        ImportItemKind::Name(name) => name,
                        ImportItemKind::SelfModule => default_name,
                    });
                    match item.kind {
                        ImportItemKind::SelfModule => {
                            self.insert_module(
                                target,
                                source.clone(),
                                self.origin(source.clone()),
                                span,
                            );
                        }
                        ImportItemKind::Name(name) => {
                            if let Some(dep) = dep {
                                self.copy_named_members(
                                    dep,
                                    &source,
                                    name,
                                    target,
                                    span,
                                    validate_members,
                                );
                            }
                        }
                    }
                }
            }
            ImportKind::Wildcard => {
                if let Some(dep) = dep {
                    self.copy_wildcard_members(&dep.exports, &source, span);
                }
            }
        }
    }

    fn copy_named_members(
        &mut self,
        dep: &ModuleDecls,
        origin_module: &ModuleScope,
        source_name: Ident,
        target_name: Ident,
        span: Span,
        validate_members: bool,
    ) {
        let mut found = false;
        if let Some(key) = dep.exports.ty(source_name).cloned() {
            found = true;
            self.insert_type(target_name, key, self.origin(origin_module.clone()), span);
        }
        if let Some(value) = dep.exports.value(source_name).cloned() {
            found = true;
            self.insert_value(target_name, value, self.origin(origin_module.clone()), span);
        }
        if let Some(module) = dep.exports.module(source_name).cloned() {
            found = true;
            self.insert_module(
                target_name,
                module,
                self.origin(origin_module.clone()),
                span,
            );
        }
        if found || !validate_members {
            return;
        }
        if dep.locals.contains_member(source_name) {
            self.push_private_member(origin_module.clone(), source_name, span);
        } else {
            self.push_missing_member(origin_module.clone(), source_name, span);
        }
    }

    fn push_private_member(&mut self, imported: ModuleScope, name: Ident, span: Span) {
        self.errors.push(DeclError::PrivateImportMember {
            module: self.module.clone(),
            imported,
            name,
            span,
        });
    }

    fn push_missing_member(&mut self, imported: ModuleScope, name: Ident, span: Span) {
        self.errors.push(DeclError::MissingImportMember {
            module: self.module.clone(),
            imported,
            name,
            span,
        });
    }

    fn copy_wildcard_members(
        &mut self,
        source: &Namespace,
        origin_module: &ModuleScope,
        span: Span,
    ) {
        for (name, key) in &source.types {
            self.insert_type(*name, key.clone(), self.origin(origin_module.clone()), span);
        }
        for (name, value) in &source.values {
            self.insert_value(
                *name,
                value.clone(),
                self.origin(origin_module.clone()),
                span,
            );
        }
        for (name, module) in &source.modules {
            self.insert_module(
                *name,
                module.clone(),
                self.origin(origin_module.clone()),
                span,
            );
        }
    }

    fn insert_value(
        &mut self,
        name: Ident,
        value: ResolvedValue,
        origin: BindingOrigin,
        span: Span,
    ) -> bool {
        if !self.claim_origin(BindingNamespace::Value, name, origin, span) {
            return false;
        }
        self.namespace.insert_value(name, value);
        true
    }

    fn insert_type(
        &mut self,
        name: Ident,
        key: NominalKey,
        origin: BindingOrigin,
        span: Span,
    ) -> bool {
        if !self.claim_origin(BindingNamespace::Type, name, origin, span) {
            return false;
        }
        self.namespace.insert_type(name, key);
        true
    }

    fn insert_module(
        &mut self,
        name: Ident,
        module: ModuleScope,
        origin: BindingOrigin,
        span: Span,
    ) -> bool {
        if !self.claim_origin(BindingNamespace::Module, name, origin, span) {
            return false;
        }
        self.namespace.insert_module(name, module);
        true
    }

    fn claim_origin(
        &mut self,
        namespace: BindingNamespace,
        name: Ident,
        second: BindingOrigin,
        span: Span,
    ) -> bool {
        let Some(first) = self.origins.get(&(namespace, name)).cloned() else {
            self.origins.insert((namespace, name), second);
            return true;
        };
        self.push_conflict(name, namespace, first, second, span);
        false
    }

    fn push_conflict(
        &mut self,
        name: Ident,
        namespace: BindingNamespace,
        first: BindingOrigin,
        second: BindingOrigin,
        span: Span,
    ) {
        let error = match namespace {
            BindingNamespace::Module => DeclError::DuplicateModuleBinding {
                module: self.module.clone(),
                name,
                first,
                second,
                span,
            },
            BindingNamespace::Value | BindingNamespace::Type => match self.mode {
                ImportMode::Import => DeclError::ImportConflict {
                    module: self.module.clone(),
                    name,
                    namespace,
                    first,
                    second,
                    span,
                },
                ImportMode::Reexport => DeclError::ReexportConflict {
                    module: self.module.clone(),
                    name,
                    namespace,
                    first,
                    second,
                    span,
                },
            },
        };
        self.errors.push(error);
    }

    fn finish_import_scope(self) -> (ImportScope, Vec<DeclError>) {
        (
            ImportScope {
                namespace: self.namespace,
                active_modules: self.active_modules,
            },
            self.errors,
        )
    }

    fn finish_namespace(self) -> (Namespace, Vec<DeclError>) {
        (self.namespace, self.errors)
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
    pub(crate) kind: CallableKind,
    pub(crate) generics: GenericParams,
    pub(crate) ty: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct CallableSig {
    pub(crate) owner_generics: GenericParams,
    pub(crate) generics: GenericParams,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) ret: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct CallableDef {
    pub(crate) id: CallableId,
    pub(crate) sig: CallableSig,
}

#[derive(Debug, Clone)]
pub(crate) struct CallableRef {
    pub(crate) def: CallableDef,
    pub(crate) receiver_ty: Option<Type>,
    pub(crate) owner_args: GenericArgs,
}

#[derive(Clone)]
pub(crate) struct AggregateSchema {
    pub(crate) key: NominalKey,
    pub(crate) generics: GenericParams,
    pub(crate) fields: HashMap<Ident, FieldSchema>,
    pub(crate) methods: HashMap<Ident, MethodSchema>,
}

#[derive(Clone)]
pub(crate) struct FieldSchema {
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

#[derive(Clone)]
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

#[derive(Clone)]
pub(crate) struct ExtendSchema {
    pub(crate) id: ExtendId,
    pub(crate) origin: ModuleScope,
    pub(crate) target: Type,
    pub(crate) generics: GenericParams,
    pub(crate) methods: HashMap<Ident, ExtendMethodSchema>,
    span: Span,
}

#[derive(Clone)]
pub(crate) struct ExtendMethodSchema {
    pub(crate) receiver: Option<MethodReceiver>,
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

#[derive(Clone)]
pub(crate) struct DeclTypeSite {
    pub(crate) module: ModuleScope,
    pub(crate) span: Span,
    pub(crate) generics: GenericTypeContext,
}

pub(crate) struct GenericContextError {
    pub(crate) module: ModuleScope,
    pub(crate) error: GenericParamError,
    pub(crate) span: Span,
}

impl DeclarationIndex {
    pub(crate) fn from_root_and_modules(
        root: &Program,
        resolved: &ResolveResult,
        always_active: HashSet<ModuleScope>,
        externs: &RawExterns,
    ) -> Self {
        let mut index = Self {
            always_active_modules: always_active,
            ..Self::default()
        };
        let modules = Self::module_programs(root, resolved);
        for (scope, program) in &modules {
            index.collect_module(
                program,
                scope.clone(),
                Self::is_export_all_scope(scope, resolved),
            );
        }
        index.collect_extern_headers(externs);
        index.apply_public_import_reexports(&modules, resolved);
        index.build_import_scopes(&modules, resolved);
        index
    }

    fn module_programs<'a>(
        root: &'a Program,
        resolved: &'a ResolveResult,
    ) -> Vec<(ModuleScope, &'a Program)> {
        let mut modules = vec![(ModuleScope::from_module_id(&resolved.root), root)];
        for group in &resolved.module_groups {
            for module in group {
                if module.key == resolved.root {
                    continue;
                }
                modules.push((ModuleScope::from_module_id(&module.key), &module.program));
            }
        }
        modules
    }

    fn is_export_all_scope(scope: &ModuleScope, resolved: &ResolveResult) -> bool {
        if scope == &ModuleScope::from_module_id(&resolved.root) {
            return true;
        }
        let Some(core_package) = &resolved.system.core else {
            return false;
        };
        matches!(scope, ModuleScope::Package(module) if module == &ModuleId::root(core_package.clone()))
    }

    pub(crate) fn errors(&self) -> &[DeclError] {
        &self.errors
    }

    pub(crate) fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub(crate) fn sync_extern_headers(&mut self, catalog: &ExternCatalog) {
        for function in catalog.functions() {
            let decls = self
                .modules
                .get_mut(&function.key.module)
                .expect("catalog extern function module exists in declarations");
            let value = decls
                .locals
                .values
                .get_mut(&function.key.name)
                .expect("catalog extern function exists in declarations");
            let ValueDecl::Func(sig) = &mut value.decl else {
                panic!("catalog extern function points to non-function declaration");
            };
            assert_eq!(sig.kind, CallableKind::ExternFunction);
            sig.ty = function.signature.to_func_type();
        }

        self.sync_value_projections();
    }

    pub(crate) fn map_canonical_type_uses<F>(&mut self, mut f: F) -> Vec<GenericContextError>
    where
        F: FnMut(DeclTypeSite, Type) -> Type,
    {
        let mut errors = vec![];

        let aggregate_keys = self.aggregates.keys().cloned().collect::<Vec<_>>();
        for key in aggregate_keys {
            let span = self.type_span_or_default(&key);
            let Some(schema) = self.aggregates.get_mut(&key) else {
                continue;
            };
            let owner_generics = generic_context(
                key.module.clone(),
                &schema.generics.type_params,
                &schema.generics.const_params,
                span,
                &mut errors,
            );
            for field in schema.fields.values_mut() {
                let site = DeclTypeSite {
                    module: key.module.clone(),
                    span,
                    generics: owner_generics.clone(),
                };
                field.ty = f(site, field.ty.clone());
            }
            for method in schema.methods.values_mut() {
                let generics = extend_generic_context(
                    key.module.clone(),
                    &owner_generics,
                    &method.generics.type_params,
                    &method.generics.const_params,
                    span,
                    &mut errors,
                );
                for param in &mut method.params {
                    let site = DeclTypeSite {
                        module: key.module.clone(),
                        span,
                        generics: generics.clone(),
                    };
                    param.ty = f(site, param.ty.clone());
                }
                let site = DeclTypeSite {
                    module: key.module.clone(),
                    span,
                    generics,
                };
                method.ret = f(site, method.ret.clone());
            }
        }

        let enum_keys = self.enums.keys().cloned().collect::<Vec<_>>();
        for key in enum_keys {
            let span = self.type_span_or_default(&key);
            let Some(schema) = self.enums.get_mut(&key) else {
                continue;
            };
            let generics = generic_context(
                key.module.clone(),
                &schema.generics.type_params,
                &schema.generics.const_params,
                span,
                &mut errors,
            );
            for variant in schema.variants.values_mut() {
                match variant {
                    VariantSchema::Unit => {}
                    VariantSchema::Tuple(types) => {
                        for ty in types {
                            let site = DeclTypeSite {
                                module: key.module.clone(),
                                span,
                                generics: generics.clone(),
                            };
                            *ty = f(site, ty.clone());
                        }
                    }
                    VariantSchema::Struct(fields) => {
                        for field in fields.values_mut() {
                            let site = DeclTypeSite {
                                module: key.module.clone(),
                                span,
                                generics: generics.clone(),
                            };
                            field.ty = f(site, field.ty.clone());
                        }
                    }
                }
            }
        }

        for index in 0..self.extends.len() {
            let origin = self.extends[index].origin.clone();
            let span = self.extends[index].span;
            let extend = &mut self.extends[index];
            let mut generics = generic_context(
                origin.clone(),
                &extend.generics.type_params,
                &extend.generics.const_params,
                span,
                &mut errors,
            );
            collect_implicit_extend_generics(&extend.target, &mut generics, true);
            let target_site = DeclTypeSite {
                module: origin.clone(),
                span,
                generics: generics.clone(),
            };
            extend.target = f(target_site, extend.target.clone());
            for method in extend.methods.values_mut() {
                for param in &mut method.params {
                    let site = DeclTypeSite {
                        module: origin.clone(),
                        span,
                        generics: generics.clone(),
                    };
                    param.ty = f(site, param.ty.clone());
                }
                let site = DeclTypeSite {
                    module: origin.clone(),
                    span,
                    generics: generics.clone(),
                };
                method.ret = f(site, method.ret.clone());
            }
        }

        let module_keys = self.modules.keys().cloned().collect::<Vec<_>>();
        for module in module_keys {
            let Some(decls) = self.modules.get_mut(&module) else {
                continue;
            };
            for value in decls.locals.values.values_mut() {
                let span = self
                    .value_spans
                    .get(&(value.module.clone(), value.name))
                    .copied()
                    .unwrap_or(Span::new(0, 0));
                let generics = match &value.decl {
                    ValueDecl::Func(sig) => generic_context(
                        value.module.clone(),
                        &sig.generics.type_params,
                        &sig.generics.const_params,
                        span,
                        &mut errors,
                    ),
                    ValueDecl::Const(_) => GenericTypeContext::default(),
                };
                let site = DeclTypeSite {
                    module: value.module.clone(),
                    span,
                    generics,
                };
                match &mut value.decl {
                    ValueDecl::Func(sig) if sig.kind == CallableKind::ExternFunction => {}
                    ValueDecl::Func(sig) => sig.ty = f(site, sig.ty.clone()),
                    ValueDecl::Const(ty) => *ty = f(site, ty.clone()),
                }
            }
        }

        self.sync_value_projections();
        errors
    }

    fn type_span_or_default(&self, key: &NominalKey) -> Span {
        self.type_spans.get(key).copied().unwrap_or(Span::new(0, 0))
    }

    fn sync_value_projections(&mut self) {
        let locals = self
            .modules
            .values()
            .flat_map(|decls| decls.locals.values.values())
            .map(|value| ((value.module.clone(), value.name), value.decl.clone()))
            .collect::<HashMap<_, _>>();

        for decls in self.modules.values_mut() {
            sync_namespace_values(&mut decls.exports, &locals);
            sync_namespace_values(&mut decls.imports.namespace, &locals);
        }
    }

    fn insert_local_value(
        &mut self,
        decls: &mut ModuleDecls,
        scope: &ModuleScope,
        name: Ident,
        value: ResolvedValue,
        exported: bool,
        span: Span,
    ) -> bool {
        if decls.locals.values.contains_key(&name) {
            self.errors.push(DeclError::DuplicateValue {
                module: scope.clone(),
                name,
                span,
            });
            return false;
        }
        decls.locals.insert_value(name, value.clone());
        if exported {
            decls.exports.insert_value(name, value);
        }
        self.value_spans.insert((scope.clone(), name), span);
        true
    }

    fn insert_local_type(
        &mut self,
        decls: &mut ModuleDecls,
        scope: &ModuleScope,
        name: Ident,
        key: NominalKey,
        exported: bool,
        span: Span,
    ) -> bool {
        if decls.locals.types.contains_key(&name) {
            self.errors.push(DeclError::DuplicateType {
                module: scope.clone(),
                name,
                span,
            });
            return false;
        }
        decls.locals.insert_type(name, key.clone());
        if exported {
            decls.exports.insert_type(name, key);
        }
        true
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
                            kind: CallableKind::Function,
                            generics: generic_params(&func.type_params, &func.const_params),
                            ty,
                        }),
                    };
                    self.insert_local_value(
                        &mut decls,
                        &scope,
                        func.name,
                        value,
                        exported,
                        func_node.span,
                    );
                }
                Stmt::Aggregate(agg_node) => {
                    let agg = &agg_node.node;
                    let key = NominalKey {
                        module: scope.clone(),
                        kind: agg.kind.into(),
                        name: agg.name,
                    };
                    let mut fields = HashMap::new();
                    for field in &agg.fields {
                        fields.insert(
                            field.name,
                            FieldSchema {
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
                    if self.insert_local_type(
                        &mut decls,
                        &scope,
                        agg.name,
                        key.clone(),
                        exported,
                        agg_node.span,
                    ) {
                        self.type_spans.insert(key.clone(), agg_node.span);
                        self.aggregates.insert(
                            key.clone(),
                            AggregateSchema {
                                key,
                                generics: generic_params(&agg.type_params, &agg.const_params),
                                fields,
                                methods,
                            },
                        );
                    }
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
                                for f in fields {
                                    field_map.insert(
                                        f.name,
                                        FieldSchema {
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
                    if self.insert_local_type(
                        &mut decls,
                        &scope,
                        enm.name,
                        key.clone(),
                        exported,
                        enum_node.span,
                    ) {
                        self.type_spans.insert(key.clone(), enum_node.span);
                        self.enums.insert(
                            key.clone(),
                            EnumSchema {
                                key,
                                generics: generic_params(&enm.type_params, &enm.const_params),
                                variants,
                            },
                        );
                    }
                }
                Stmt::Const(const_node) => {
                    let c = &const_node.node;
                    let value = ResolvedValue {
                        module: scope.clone(),
                        name: c.name,
                        decl: ValueDecl::Const(c.ty.clone().unwrap_or(Type::Infer)),
                    };
                    self.insert_local_value(
                        &mut decls,
                        &scope,
                        c.name,
                        value,
                        exported,
                        const_node.span,
                    );
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
                        let (receiver, params) = m.params.split_first().map_or(
                            (None, &[][..]),
                            |(self_param, params)| {
                                let receiver = match self_param.mutability {
                                    Mutability::Mutable => MethodReceiver::Var,
                                    Mutability::Immutable => MethodReceiver::Value,
                                };
                                (Some(receiver), params)
                            },
                        );
                        methods.insert(
                            m.name,
                            ExtendMethodSchema {
                                receiver,
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
                        span: extend_node.span,
                    });
                }
                _ => {}
            }
        }

        self.modules.insert(scope, decls);
    }

    fn collect_extern_headers(&mut self, externs: &RawExterns) {
        for group in &externs.groups {
            for module in &group.modules {
                self.collect_extern_module(module);
            }
        }
    }

    fn collect_extern_module(&mut self, module: &RawExternModule) {
        let scope = raw_module_scope(&module.scope);
        let mut decls = self.modules.remove(&scope).unwrap_or_default();

        for ty in &module.types {
            let name = Ident::new(&ty.name);
            let key = NominalKey {
                module: scope.clone(),
                kind: NominalKind::Extern,
                name,
            };
            let span = ty.site.span.unwrap_or(Span::new(0, 0));
            if self.insert_local_type(&mut decls, &scope, name, key.clone(), true, span) {
                self.type_spans.insert(key, span);
            }
        }

        for func in &module.functions {
            let name = Ident::new(&func.decl.name);
            let value = ResolvedValue {
                module: scope.clone(),
                name,
                decl: ValueDecl::Func(FuncSig {
                    kind: CallableKind::ExternFunction,
                    generics: GenericParams::default(),
                    ty: Type::Func {
                        params: vec![],
                        ret: Box::new(Type::Void),
                    },
                }),
            };
            self.insert_local_value(
                &mut decls,
                &scope,
                name,
                value,
                true,
                func.site.span.unwrap_or(Span::new(0, 0)),
            );
        }

        self.modules.insert(scope, decls);
    }

    fn resolve_import_target(
        &mut self,
        current: &ModuleScope,
        ordinal: usize,
        span: Span,
        resolved: &ResolveResult,
    ) -> Option<ImportTargetScope> {
        let current_module = module_id_for_scope(current);
        let Some(target) = resolved.import_target(&current_module, ordinal).cloned() else {
            debug_assert!(
                false,
                "missing resolved import edge for module {current_module:?} import {ordinal}"
            );
            return None;
        };
        let module = ModuleScope::from_module_id(resolved.canonical_module(&target.base));
        if target.exported_path.is_empty() {
            return Some(ImportTargetScope {
                module,
                default_name: target.default_name,
            });
        }
        self.resolve_exported_module_path(current, module, &target.exported_path, span)
    }

    fn resolve_exported_module_path(
        &mut self,
        current: &ModuleScope,
        mut module: ModuleScope,
        path: &[Ident],
        span: Span,
    ) -> Option<ImportTargetScope> {
        let mut default_name = None;
        for segment in path {
            default_name = Some(*segment);
            match self.exported_module(&module, *segment) {
                Some(next) => module = next,
                None => {
                    self.errors.push(DeclError::MissingImportMember {
                        module: current.clone(),
                        imported: module,
                        name: *segment,
                        span,
                    });
                    return None;
                }
            }
        }
        Some(ImportTargetScope {
            module,
            default_name: default_name.unwrap_or_else(|| Ident::new("")),
        })
    }

    fn apply_public_import_reexports(
        &mut self,
        modules: &[(ModuleScope, &Program)],
        resolved: &ResolveResult,
    ) {
        for (scope, program) in modules {
            let Some(exports) = self.modules.get(scope).map(|decls| decls.exports.clone()) else {
                continue;
            };

            let mut builder =
                ImportScopeBuilder::with_namespace(scope.clone(), exports, ImportMode::Reexport);
            self.apply_program_imports(scope, program, resolved, &mut builder);

            let (exports, errors) = builder.finish_namespace();
            self.errors.extend(errors);
            if let Some(decls) = self.modules.get_mut(scope) {
                decls.exports = exports;
            }
        }
    }

    fn build_import_scopes(
        &mut self,
        modules: &[(ModuleScope, &Program)],
        resolved: &ResolveResult,
    ) {
        for (scope, program) in modules {
            let mut builder = ImportScopeBuilder::new(scope.clone(), ImportMode::Import);
            if let Some(decls) = self.modules.get(scope) {
                builder.seed_origins(&decls.locals, &BindingOrigin::Local);
            }
            if let Some(core_package) = &resolved.system.core
                && package_for_scope(scope) != *core_package
            {
                let core = ModuleScope::from_module_id(&ModuleId::root(core_package.clone()));
                if let Some(decls) = self.modules.get(&core) {
                    builder.copy_wildcard_members(&decls.exports, &core, Span::new(0, 0));
                }
            }
            self.apply_program_imports(scope, program, resolved, &mut builder);
            let (mut imports, errors) = builder.finish_import_scope();
            self.errors.extend(errors);
            imports.activate_imported_origins();
            if let Some(decls) = self.modules.get_mut(scope) {
                decls.imports = imports;
            }
        }
    }

    fn apply_program_imports(
        &mut self,
        scope: &ModuleScope,
        program: &Program,
        resolved: &ResolveResult,
        builder: &mut ImportScopeBuilder,
    ) {
        let mut ordinal = 0;
        for stmt in &program.stmts {
            let Stmt::Import(import) = &stmt.node else {
                continue;
            };
            let import_ordinal = ordinal;
            ordinal += 1;
            let is_public = matches!(import.node.visibility, Visibility::Public);
            if builder.mode == ImportMode::Reexport && !is_public {
                continue;
            }
            let Some(target) =
                self.resolve_import_target(scope, import_ordinal, import.span, resolved)
            else {
                continue;
            };
            if builder.mode == ImportMode::Import {
                builder.active_modules.insert(target.module.clone());
            }
            let dep = self.modules.get(&target.module);
            let validate_members = builder.mode == ImportMode::Reexport || !is_public;
            builder.apply_import(
                &import.node.kind,
                target.module,
                target.default_name,
                dep,
                import.span,
                validate_members,
            );
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

    pub(crate) fn resolve_visible_type_key(
        &self,
        module: &ModuleScope,
        qualifier: Option<Ident>,
        name: Ident,
    ) -> Option<NominalKey> {
        match qualifier {
            Some(alias) => {
                let target = self.imported_module(module, alias)?;
                self.exported_type(&target, name)
            }
            None => self.visible_type(module, name),
        }
    }

    pub(crate) fn imports_module(&self, module: &ModuleScope, imported: &ModuleScope) -> bool {
        self.modules
            .get(module)
            .is_some_and(|decls| decls.imports.active_modules.contains(imported))
    }

    pub(crate) fn always_active_module(&self, module: &ModuleScope) -> bool {
        self.always_active_modules.contains(module)
    }

    pub(crate) fn set_const_type(&mut self, scope: &ModuleScope, name: Ident, ty: Type) {
        let ty = Box::new(ty);
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

    fn nominal_generics(&self, key: &NominalKey) -> Option<GenericParams> {
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                self.aggregate(key).map(|schema| schema.generics.clone())
            }
            NominalKind::Enum => self.enum_schema(key).map(|schema| schema.generics.clone()),
            NominalKind::Extern => self
                .modules
                .get(&key.module)?
                .locals
                .ty(key.name)
                .map(|_| GenericParams::default()),
        }
    }

    pub(crate) fn key_for_type(&self, ty: &Type) -> Option<NominalKey> {
        let Type::Nominal(nominal) = ty else {
            return None;
        };
        let scope = match &nominal.origin {
            Some(origin) => ModuleScope::from_nominal_origin(origin),
            None => ModuleScope::Root,
        };
        self.local_type(&scope, nominal.name)
            .filter(|key| key.kind == nominal.kind)
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

    pub(crate) fn callable_for_value(&self, value: &ResolvedValue) -> Option<CallableRef> {
        if !self.modules.contains_key(&value.module) {
            return None;
        }
        let ValueDecl::Func(sig) = &value.decl else {
            return None;
        };
        let Type::Func { params, ret } = &sig.ty else {
            return None;
        };
        let id = match sig.kind {
            CallableKind::Function => CallableId::function(value.module.clone(), value.name),
            CallableKind::ExternFunction => {
                CallableId::extern_function(value.module.clone(), value.name)
            }
            CallableKind::StaticMethod
            | CallableKind::InstanceMethod
            | CallableKind::ExtendMethod
            | CallableKind::EnumVariant => return None,
        };

        Some(CallableRef {
            def: CallableDef {
                id,
                sig: CallableSig {
                    owner_generics: GenericParams::default(),
                    generics: sig.generics.clone(),
                    params: params.clone(),
                    ret: (**ret).clone(),
                },
            },
            receiver_ty: None,
            owner_args: GenericArgs::default(),
        })
    }

    pub(crate) fn callable_for_aggregate_method(
        &self,
        aggregate: &AggregateSchema,
        name: Ident,
        method: &MethodSchema,
        receiver_ty: Option<Type>,
    ) -> CallableRef {
        debug_assert!(self.aggregates.contains_key(&aggregate.key));
        let params = match receiver_ty.as_ref() {
            Some(receiver) => method
                .params
                .iter()
                .map(|param| {
                    FuncParam::new(
                        substitute_aggregate_member(receiver, &aggregate.generics, &param.ty),
                        param.mutable,
                    )
                })
                .collect(),
            None => method.params.clone(),
        };
        let ret = receiver_ty.as_ref().map_or_else(
            || method.ret.clone(),
            |receiver| substitute_aggregate_member(receiver, &aggregate.generics, &method.ret),
        );

        CallableRef {
            def: CallableDef {
                id: CallableId::aggregate_method(
                    aggregate.key.clone(),
                    name,
                    method.receiver.is_some(),
                ),
                sig: CallableSig {
                    owner_generics: aggregate.generics.clone(),
                    generics: method.generics.clone(),
                    params,
                    ret,
                },
            },
            receiver_ty,
            owner_args: GenericArgs::default(),
        }
    }

    pub(crate) fn callable_for_extend_method(
        &self,
        receiver_ty: Type,
        extend: &ExtendSchema,
        name: Ident,
        method: &ExtendMethodSchema,
        owner_args: GenericArgs,
    ) -> Option<CallableRef> {
        debug_assert!(self.extends.iter().any(|schema| schema.id == extend.id));
        method.receiver?;
        let (type_subst, const_subst) = extend.generics.substitutions(&owner_args);
        let template_params = method
            .params
            .iter()
            .map(|param| {
                FuncParam::new(
                    generic_template_type(&param.ty, &extend.generics),
                    param.mutable,
                )
            })
            .collect::<Vec<_>>();
        let template_ret = generic_template_type(&method.ret, &extend.generics);

        Some(CallableRef {
            def: CallableDef {
                id: CallableId::extend_method(extend.id.clone(), name),
                sig: CallableSig {
                    owner_generics: extend.generics.clone(),
                    generics: method.generics.clone(),
                    params: substitute_func_params(&template_params, &type_subst, &const_subst),
                    ret: substitute(&template_ret, &type_subst, &const_subst),
                },
            },
            receiver_ty: Some(receiver_ty),
            owner_args,
        })
    }

    pub(crate) fn callable_for_variant(
        &self,
        enum_key: &NominalKey,
        variant: Ident,
        schema: &VariantSchema,
    ) -> Option<CallableRef> {
        let params = match schema {
            VariantSchema::Unit => vec![],
            VariantSchema::Tuple(types) => types.iter().cloned().map(FuncParam::immut).collect(),
            VariantSchema::Struct(_) => return None,
        };
        let enum_schema = self.enum_schema(enum_key)?;
        let owner_generics = enum_schema.generics.clone();
        let ret = owner_template(enum_key, &owner_generics);

        Some(CallableRef {
            def: CallableDef {
                id: CallableId::enum_variant(enum_key.clone(), variant),
                sig: CallableSig {
                    owner_generics,
                    generics: GenericParams::default(),
                    params,
                    ret,
                },
            },
            receiver_ty: None,
            owner_args: GenericArgs::default(),
        })
    }
}

pub(crate) fn substitute_aggregate_member(
    receiver: &Type,
    generics: &GenericParams,
    ty: &Type,
) -> Type {
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

fn substitute_func_params(
    params: &[FuncParam],
    type_subst: &TypeSubst,
    const_subst: &ConstSubst,
) -> Vec<FuncParam> {
    params
        .iter()
        .map(|param| {
            FuncParam::new(
                substitute(&param.ty, type_subst, const_subst),
                param.mutable,
            )
        })
        .collect()
}

pub(crate) fn owner_template(owner: &NominalKey, generics: &GenericParams) -> Type {
    let type_args = generics
        .type_params
        .iter()
        .map(|param| Type::Var(param.id))
        .collect::<Vec<_>>();
    let const_args = generics
        .const_params
        .iter()
        .map(|param| ConstArg::Param(param.id))
        .collect::<Vec<_>>();
    nominal_type_with_args(owner, &type_args, &const_args)
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
        return (template == concrete).then(|| Ok(GenericArgs::default()));
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
}

impl DeclarationIndex {
    pub(crate) fn finalize_type_ref(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        ty: &Type,
    ) -> Result<Type, TypeRefError> {
        match ty {
            Type::UnresolvedName(name) => {
                if let Some(TypeParamBinding::Explicit(id)) = generics.type_param(*name) {
                    return Ok(Type::Var(id));
                }
                if generics.has_const_param(*name) {
                    return Err(TypeRefError::Unknown {
                        qualifier: None,
                        name: *name,
                    });
                }
                self.resolve_visible_type_key(module, None, *name)
                    .map(|key| nominal_type(&key))
                    .ok_or(TypeRefError::Unknown {
                        qualifier: None,
                        name: *name,
                    })
            }
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => {
                if qualifier.is_none() && generic_args.is_empty() {
                    if let Some(TypeParamBinding::Explicit(id)) = generics.type_param(*name) {
                        return Ok(Type::Var(id));
                    }
                    if generics.has_const_param(*name) {
                        return Err(TypeRefError::Unknown {
                            qualifier: None,
                            name: *name,
                        });
                    }
                }
                let key = self
                    .resolve_visible_type_key(module, *qualifier, *name)
                    .ok_or(TypeRefError::Unknown {
                        qualifier: *qualifier,
                        name: *name,
                    })?;
                self.finalize_nominal_type_ref(module, generics, &key, generic_args)
            }
            Type::Func { params, ret } => Ok(Type::Func {
                params: params
                    .iter()
                    .map(|param| {
                        Ok(FuncParam::new(
                            self.finalize_type_ref(module, generics, &param.ty)?,
                            param.mutable,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
                ret: Box::new(self.finalize_type_ref(module, generics, ret)?),
            }),
            Type::Tuple(elems) => elems
                .iter()
                .map(|ty| self.finalize_type_ref(module, generics, ty))
                .collect::<Result<Vec<_>, _>>()
                .map(Type::Tuple),
            Type::NamedTuple(fields) => fields
                .iter()
                .map(|(name, ty)| Ok((*name, self.finalize_type_ref(module, generics, ty)?)))
                .collect::<Result<Vec<_>, _>>()
                .map(Type::NamedTuple),
            Type::Nominal(nominal) => Ok(Type::nominal_with_origin(
                nominal.kind,
                nominal.name,
                nominal
                    .type_args
                    .iter()
                    .map(|ty| self.finalize_type_ref(module, generics, ty))
                    .collect::<Result<_, _>>()?,
                nominal
                    .const_args
                    .iter()
                    .map(|arg| Self::finalize_const_arg(generics, arg))
                    .collect::<Result<_, _>>()?,
                nominal.origin.clone(),
            )),
            Type::List { elem } => Ok(Type::List {
                elem: Box::new(self.finalize_type_ref(module, generics, elem)?),
            }),
            Type::Slice { elem } => Ok(Type::Slice {
                elem: Box::new(self.finalize_type_ref(module, generics, elem)?),
            }),
            Type::Array { elem, len } => Ok(Type::Array {
                elem: Box::new(self.finalize_type_ref(module, generics, elem)?),
                len: Self::finalize_array_len(generics, *len)?,
            }),
            Type::Map { key, value } => Ok(Type::Map {
                key: Box::new(self.finalize_type_ref(module, generics, key)?),
                value: Box::new(self.finalize_type_ref(module, generics, value)?),
            }),
            Type::Infer
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_) => Ok(ty.clone()),
        }
    }

    pub(crate) fn finalize_nominal_type_args(
        &self,
        module: &ModuleScope,
        key: &NominalKey,
        args: Vec<Type>,
    ) -> Result<Type, TypeRefError> {
        let args = args.into_iter().map(GenericArg::Type).collect::<Vec<_>>();
        self.finalize_nominal_type_ref(module, &GenericTypeContext::default(), key, &args)
    }

    fn finalize_nominal_type_ref(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        key: &NominalKey,
        args: &[GenericArg],
    ) -> Result<Type, TypeRefError> {
        let params = self.nominal_generics(key).unwrap_or_default();
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
                type_args.push(self.finalize_type_ref(module, generics, ty)?);
            } else {
                const_args.push(self.finalize_generic_const_arg(module, generics, arg)?);
            }
        }
        Ok(nominal_type_with_args(key, &type_args, &const_args))
    }

    fn finalize_generic_const_arg(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        arg: &GenericArg,
    ) -> Result<ConstArg, TypeRefError> {
        match arg {
            GenericArg::Const(arg) => Self::finalize_const_arg(generics, arg),
            GenericArg::Type(ty) => match bare_type_name(ty) {
                Some(name) => Self::finalize_const_name_arg(generics, name),
                None => {
                    let ty = self.finalize_type_ref(module, generics, ty)?;
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

    fn finalize_const_arg(
        generics: &GenericTypeContext,
        arg: &ConstArg,
    ) -> Result<ConstArg, TypeRefError> {
        match arg {
            ConstArg::Name(name) => Self::finalize_const_name_arg(generics, *name),
            ConstArg::Value(_) | ConstArg::Param(_) => Ok(arg.clone()),
        }
    }

    fn finalize_const_name_arg(
        generics: &GenericTypeContext,
        name: Ident,
    ) -> Result<ConstArg, TypeRefError> {
        Ok(
            Self::finalize_const_name(generics, name)?
                .map_or(ConstArg::Name(name), ConstArg::Param),
        )
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
            ArrayLen::Named(name) => Ok(Self::finalize_const_name(generics, name)?
                .map_or(ArrayLen::Named(name), ArrayLen::Param)),
            ArrayLen::Fixed(_) | ArrayLen::Infer | ArrayLen::Param(_) => Ok(len),
        }
    }
}

fn generic_context(
    module: ModuleScope,
    type_params: &[TypeParam],
    const_params: &[ConstParam],
    span: Span,
    errors: &mut Vec<GenericContextError>,
) -> GenericTypeContext {
    match GenericTypeContext::try_from_params(type_params, const_params) {
        Ok(generics) => generics,
        Err(error) => {
            errors.push(GenericContextError {
                module,
                error,
                span,
            });
            GenericTypeContext::default()
        }
    }
}

fn extend_generic_context(
    module: ModuleScope,
    owner: &GenericTypeContext,
    type_params: &[TypeParam],
    const_params: &[ConstParam],
    span: Span,
    errors: &mut Vec<GenericContextError>,
) -> GenericTypeContext {
    match owner.try_with_shadowing_params(type_params, const_params) {
        Ok(generics) => generics,
        Err(error) => {
            errors.push(GenericContextError {
                module,
                error,
                span,
            });
            owner.clone()
        }
    }
}

fn sync_namespace_values(
    namespace: &mut Namespace,
    locals: &HashMap<(ModuleScope, Ident), ValueDecl>,
) {
    for value in namespace.values.values_mut() {
        if let Some(decl) = locals.get(&(value.module.clone(), value.name)) {
            value.decl = decl.clone();
        }
    }
}

struct GenericTemplate {
    generics: GenericTypeContext,
}

impl TypeFolder for GenericTemplate {
    fn fold_unresolved_name(&mut self, name: Ident) -> Type {
        match self.generics.type_param(name) {
            Some(TypeParamBinding::Explicit(id)) => Type::Var(id),
            Some(TypeParamBinding::ImplicitExtend) | None => Type::UnresolvedName(name),
        }
    }

    fn fold_unresolved_nominal(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        if qualifier.is_none()
            && generic_args.is_empty()
            && let Some(TypeParamBinding::Explicit(id)) = self.generics.type_param(name)
        {
            return Type::Var(id);
        }
        self.fold_unresolved_nominal_default(qualifier, name, generic_args)
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Name(name) => self
                .generics
                .const_param(*name)
                .map_or_else(|| arg.clone(), ConstArg::Param),
            ConstArg::Value(_) | ConstArg::Param(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Named(name) => self
                .generics
                .const_param(name)
                .map_or(ArrayLen::Named(name), ArrayLen::Param),
            other => other,
        }
    }
}

pub(crate) fn generic_template_type(ty: &Type, generics: &GenericParams) -> Type {
    GenericTemplate {
        generics: GenericTypeContext::try_from_params(
            &generics.type_params,
            &generics.const_params,
        )
        .expect("generic_template_type requires validated generic params"),
    }
    .fold_type(ty)
}

fn generic_params(type_params: &[TypeParam], const_params: &[ConstParam]) -> GenericParams {
    GenericParams {
        type_params: type_params.to_vec(),
        const_params: const_params.to_vec(),
    }
}

fn collect_implicit_extend_generics(
    ty: &Type,
    generics: &mut GenericTypeContext,
    target_root: bool,
) {
    match ty {
        Type::UnresolvedName(name) if !target_root => {
            generics.insert_implicit_extend_type(*name);
        }
        Type::UnresolvedNominal {
            name,
            generic_args,
            qualifier,
        } => {
            if qualifier.is_none() && !target_root {
                generics.insert_implicit_extend_type(*name);
            }
            for arg in generic_args {
                if let GenericArg::Type(ty) = arg {
                    collect_implicit_extend_generics(ty, generics, false);
                }
            }
        }
        Type::Func { params, ret } => {
            for param in params {
                collect_implicit_extend_generics(&param.ty, generics, false);
            }
            collect_implicit_extend_generics(ret, generics, false);
        }
        Type::Tuple(elems) => {
            for ty in elems {
                collect_implicit_extend_generics(ty, generics, false);
            }
        }
        Type::NamedTuple(fields) => {
            for (_, ty) in fields {
                collect_implicit_extend_generics(ty, generics, false);
            }
        }
        Type::Nominal(nominal) => {
            for ty in &nominal.type_args {
                collect_implicit_extend_generics(ty, generics, false);
            }
        }
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            collect_implicit_extend_generics(elem, generics, false);
        }
        Type::Map { key, value } => {
            collect_implicit_extend_generics(key, generics, false);
            collect_implicit_extend_generics(value, generics, false);
        }
        Type::UnresolvedName(_)
        | Type::Infer
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Var(_) => {}
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

pub(crate) fn nominal_type(key: &NominalKey) -> Type {
    nominal_type_with_args(key, &[], &[])
}

pub(crate) fn nominal_type_with_args(
    key: &NominalKey,
    type_args: &[Type],
    const_args: &[ConstArg],
) -> Type {
    Type::nominal_with_origin(
        key.kind,
        key.name,
        type_args.to_vec(),
        const_args.to_vec(),
        key.module.nominal_origin(),
    )
}

#[cfg(test)]
mod tests {
    use anvyx_externs::{
        ExternEffects, ExternFunctionDescriptor, ExternModuleDescriptor, ExternParam, ExternRep,
        ExternSignature, ExternTypeDescriptor, ExternTypeExpr, ModulePath as ExternModulePath,
        ParamFlow, ProviderDescriptor, ProviderId,
    };

    use super::*;
    use crate::{
        ast::TypeVarId,
        test_support::{parse_program, resolved_modules},
        typecheck::type_ops::type_closure_facts,
    };

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn parse(source: &str) -> Program {
        parse_program(source)
    }

    fn scope(name: &str) -> ModuleScope {
        ModuleScope::Named(ModulePath::new(vec![name.to_string()]).unwrap())
    }

    fn index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse(root);
        let resolved = resolved_modules(&root, modules);
        let externs = crate::externs::collect_source_externs(&root, &resolved).unwrap();
        DeclarationIndex::from_root_and_modules(&root, &resolved, HashSet::new(), &externs)
    }

    fn checked_index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse(root);
        let resolved = resolved_modules(&root, modules);
        let externs = crate::externs::collect_source_externs(&root, &resolved).unwrap();
        super::super::check_with_modules(&root, &resolved, HashSet::new(), externs)
            .expect("typecheck failed")
            .decls()
            .clone()
    }

    fn provider_index(root: &str, provider: ProviderDescriptor) -> DeclarationIndex {
        provider_index_with_modules(root, &[], provider)
    }

    fn provider_index_with_modules(
        root: &str,
        modules: &[(&str, &str)],
        provider: ProviderDescriptor,
    ) -> DeclarationIndex {
        let root = parse(root);
        let resolved = resolved_modules(&root, modules);
        let raw = crate::externs::ingest_providers(crate::externs::ExternInputs {
            providers: vec![provider],
        })
        .unwrap();
        DeclarationIndex::from_root_and_modules(&root, &resolved, HashSet::new(), &raw)
    }

    fn provider_with_module(module: ExternModuleDescriptor) -> ProviderDescriptor {
        ProviderDescriptor {
            provider: ProviderId {
                name: "host".to_string(),
            },
            modules: vec![module],
        }
    }

    fn extern_module(path: &[&str]) -> ExternModulePath {
        ExternModulePath {
            segments: path.iter().map(|segment| (*segment).to_string()).collect(),
        }
    }

    fn extern_fn(name: &str) -> ExternFunctionDescriptor {
        ExternFunctionDescriptor {
            name: name.to_string(),
            doc: None,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: Some("x".to_string()),
                    ty: ExternTypeExpr::Int,
                    flow: ParamFlow::Value,
                }],
                ret: ExternTypeExpr::Float,
            },
            effects: ExternEffects::default(),
        }
    }

    fn extern_type(name: &str) -> ExternTypeDescriptor {
        ExternTypeDescriptor {
            name: name.to_string(),
            doc: None,
            rep: ExternRep::Shared,
            fields: vec![],
            init: None,
            methods: vec![],
            statics: vec![],
            operators: vec![],
        }
    }

    fn func_ret(ty: &Type) -> &Type {
        let Type::Func { ret, .. } = ty else {
            panic!("expected function type: {ty:?}");
        };
        ret
    }

    fn assert_nominal(ty: &Type, kind: NominalKind, module: Option<&str>, name: &str) {
        let Type::Nominal(nominal) = ty else {
            panic!("expected nominal type: {ty:?}");
        };
        assert_eq!(nominal.kind, kind);
        assert_eq!(nominal.name, ident(name));
        assert_eq!(
            nominal.origin,
            module.map(|name| ModuleOrigin::Module(std::rc::Rc::from(vec![name.to_string()])))
        );
    }

    fn assert_no_unresolved_nominal(ty: &Type) {
        assert!(
            type_closure_facts(ty).first_unresolved.is_none(),
            "unresolved nominal survived: {ty:?}"
        );
    }

    #[test]
    fn generic_template_nominal_args() {
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
        let scope = ModuleScope::Named(ModulePath::new(vec!["tools".into()]).unwrap());
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name,
            generic_args: vec![GenericArg::Type(Type::Int)],
        };
        let index = index("", &[("tools", "pub struct Box<T> { value: T }")]);
        let result = index
            .finalize_type_ref(&scope, &GenericTypeContext::default(), &ty)
            .unwrap();

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
        let ty = Type::UnresolvedNominal {
            qualifier: None,
            name: wrapper,
            generic_args: vec![GenericArg::Type(Type::UnresolvedName(inner))],
        };
        let index = index(
            "struct Wrapper<T> { value: T } struct Inner { value: int }",
            &[],
        );
        let result = index
            .finalize_type_ref(&scope, &GenericTypeContext::default(), &ty)
            .unwrap();

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
        let index = index("", &[]);
        let error = index
            .finalize_type_ref(&ModuleScope::Root, &GenericTypeContext::default(), &ty)
            .expect_err("unknown qualified type should fail finalization");

        assert!(matches!(
            error,
            TypeRefError::Unknown {
                qualifier: Some(_),
                ..
            }
        ));
    }

    #[test]
    fn private_not_exported() {
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
    fn type_reexport_origin() {
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
    fn return_type_module_imports() {
        let index = checked_index(
            "import facade { make };",
            &[
                ("alpha", "pub struct Item { value: int }"),
                ("beta", "pub struct Item { label: string }"),
                (
                    "facade",
                    "import alpha { Item }; pub fn make() -> Item { Item { value: 1 } }",
                ),
            ],
        );
        let value = index
            .imported_value(&ModuleScope::Root, ident("make"))
            .expect("missing import");

        assert_no_unresolved_nominal(value.decl.ty());
        assert_nominal(
            func_ret(value.decl.ty()),
            NominalKind::Struct,
            Some("alpha"),
            "Item",
        );
    }

    #[test]
    fn reexported_return_origin() {
        let index = checked_index(
            "import facade { make };",
            &[
                (
                    "tools",
                    "pub struct Point { x: int } pub fn make() -> Point { Point { x: 1 } }",
                ),
                ("other", "pub struct Point { y: int }"),
                ("facade", "pub import tools { make };"),
            ],
        );
        let value = index
            .imported_value(&ModuleScope::Root, ident("make"))
            .expect("missing import");

        assert_no_unresolved_nominal(value.decl.ty());
        assert_eq!(value.module, scope("tools"));
        assert_eq!(value.name, ident("make"));
        assert_nominal(
            func_ret(value.decl.ty()),
            NominalKind::Struct,
            Some("tools"),
            "Point",
        );
    }

    #[test]
    fn module_import_not_bare_type() {
        let index = index(
            "import shapes;",
            &[("shapes", "pub struct Point { x: int }")],
        );

        assert_eq!(
            index.imported_module(&ModuleScope::Root, ident("shapes")),
            Some(scope("shapes"))
        );
        assert!(
            index
                .imported_type(&ModuleScope::Root, ident("Point"))
                .is_none()
        );
        assert!(
            index
                .visible_type(&ModuleScope::Root, ident("Point"))
                .is_none()
        );
    }

    #[test]
    fn qualified_module_export_key() {
        let index = index(
            "import shapes;",
            &[("shapes", "pub struct Point { x: int }")],
        );
        let key = index
            .exported_type(&scope("shapes"), ident("Point"))
            .expect("missing export");

        assert_eq!(key.module, scope("shapes"));
        assert_eq!(key.name, ident("Point"));
        assert_eq!(key.kind, NominalKind::Struct);
    }

    #[test]
    fn qualified_visible_module_binding() {
        let index = index(
            "import shapes;",
            &[("shapes", "pub struct Point { x: int }")],
        );

        let key = index
            .resolve_visible_type_key(&ModuleScope::Root, Some(ident("shapes")), ident("Point"))
            .expect("missing qualified type");

        assert_eq!(key.module, scope("shapes"));
        assert_eq!(key.name, ident("Point"));
        assert_eq!(key.kind, NominalKind::Struct);
    }

    #[test]
    fn qualified_visible_requires_binding() {
        let index = index("", &[("shapes", "pub struct Point { x: int }")]);

        assert!(
            index
                .resolve_visible_type_key(&ModuleScope::Root, Some(ident("shapes")), ident("Point"))
                .is_none()
        );
    }

    #[test]
    fn key_for_type_uses_nominal_origin() {
        let index = index(
            "",
            &[
                ("alpha", "pub struct Point { x: int }"),
                ("beta", "pub struct Point { y: int }"),
            ],
        );
        let ty = Type::nominal(
            NominalKind::Struct,
            ident("Point"),
            vec![],
            vec![],
            Some(std::rc::Rc::new(["alpha".into()])),
        );

        let key = index.key_for_type(&ty).expect("missing key");

        assert_eq!(key.module, scope("alpha"));
        assert_eq!(key.name, ident("Point"));
        assert_eq!(key.kind, NominalKind::Struct);
    }

    #[test]
    fn root_originless_nominal_key() {
        let index = index("struct Point { x: int }", &[]);
        let ty = Type::nominal(NominalKind::Struct, ident("Point"), vec![], vec![], None);

        let key = index.key_for_type(&ty).expect("missing key");

        assert_eq!(key.module, ModuleScope::Root);
        assert_eq!(key.name, ident("Point"));
        assert_eq!(key.kind, NominalKind::Struct);
    }

    #[test]
    fn imported_originless_no_guess() {
        let index = index("", &[("alpha", "pub struct Point { x: int }")]);
        let ty = Type::nominal(NominalKind::Struct, ident("Point"), vec![], vec![], None);

        assert!(index.key_for_type(&ty).is_none());
    }

    #[test]
    fn key_for_type_rejects_kind_mismatch() {
        let index = index("enum Point { A }", &[]);
        let ty = Type::nominal(NominalKind::Struct, ident("Point"), vec![], vec![], None);

        assert!(index.key_for_type(&ty).is_none());
    }

    #[test]
    fn enum_return_type_keeps_origin() {
        let index = checked_index(
            "import states { current };",
            &[(
                "states",
                "pub enum Status { Active } pub fn current() -> Status { Status.Active }",
            )],
        );
        let value = index
            .imported_value(&ModuleScope::Root, ident("current"))
            .expect("missing import");

        assert_no_unresolved_nominal(value.decl.ty());
        assert_nominal(
            func_ret(value.decl.ty()),
            NominalKind::Enum,
            Some("states"),
            "Status",
        );
    }

    #[test]
    fn alias_module_namespace() {
        let index = index("import facade { self as f };", &[("facade", "")]);

        assert_eq!(
            index.imported_module(&ModuleScope::Root, ident("f")),
            Some(scope("facade")),
        );
    }

    #[test]
    fn reexported_value_origin() {
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
    fn imported_type_activates_origins() {
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
    fn provider_extern_headers_are_visible_through_imports() {
        let index = provider_index(
            "import host { Handle, load }; import host { * };",
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![extern_fn("load")],
            }),
        );
        let host = scope("host");
        let ty = index
            .imported_type(&ModuleScope::Root, ident("Handle"))
            .expect("missing provider type import");
        let value = index
            .imported_value(&ModuleScope::Root, ident("load"))
            .expect("missing provider function import");

        assert_eq!(ty.module, host);
        assert_eq!(ty.kind, NominalKind::Extern);
        assert_eq!(value.module, scope("host"));
        assert!(matches!(
            value.decl,
            ValueDecl::Func(FuncSig {
                kind: CallableKind::ExternFunction,
                ..
            })
        ));
    }

    #[test]
    fn provider_module_import_creates_alias() {
        let index = provider_index(
            "import host;",
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![],
            }),
        );

        assert_eq!(
            index.imported_module(&ModuleScope::Root, ident("host")),
            Some(scope("host"))
        );
    }

    #[test]
    fn provider_members_are_not_module_values() {
        let index = provider_index(
            "import host { * };",
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![anvyx_externs::ExternFieldDescriptor {
                        name: "x".to_string(),
                        ty: ExternTypeExpr::Int,
                        access: anvyx_externs::FieldAccess::ReadOnly { computed: false },
                        doc: None,
                    }],
                    ..extern_type("Handle")
                }],
                functions: vec![],
            }),
        );

        assert!(
            index
                .imported_value(&ModuleScope::Root, ident("x"))
                .is_none()
        );
    }

    #[test]
    fn provider_extern_type_conflicts_with_source_type() {
        let index = provider_index_with_modules(
            "",
            &[("host", "pub struct Handle { id: int }")],
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![],
            }),
        );

        assert!(matches!(
            index.errors().first(),
            Some(DeclError::DuplicateType { name, .. }) if *name == ident("Handle")
        ));
    }

    #[test]
    fn provider_extern_function_conflicts_with_source_function() {
        let index = provider_index_with_modules(
            "",
            &[("host", "pub fn load() -> float { 0.0 }")],
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![extern_fn("load")],
            }),
        );

        assert!(matches!(
            index.errors().first(),
            Some(DeclError::DuplicateValue { name, .. }) if *name == ident("load")
        ));
    }

    #[test]
    fn callable_value_kinds() {
        let index = index(
            "fn f<T>(x: T) -> T { x } extern fn e(x: int) -> int; const C = 1;",
            &[],
        );
        let func = index.local_value(&ModuleScope::Root, ident("f")).unwrap();
        let ext = index.local_value(&ModuleScope::Root, ident("e")).unwrap();
        let konst = index.local_value(&ModuleScope::Root, ident("C")).unwrap();

        let func = index.callable_for_value(&func).unwrap();
        let ext = index.callable_for_value(&ext).unwrap();

        assert_eq!(func.def.id.kind, CallableKind::Function);
        assert_eq!(ext.def.id.kind, CallableKind::ExternFunction);
        assert!(index.callable_for_value(&konst).is_none());
    }

    #[test]
    fn aggregate_method_callable() {
        let index = index(
            "struct Box<T> { value: T, fn make(value: T) -> T { value } fn get(self, fallback: T) -> T { self.value } }",
            &[],
        );
        let key = index.local_type(&ModuleScope::Root, ident("Box")).unwrap();
        let aggregate = index.aggregate(&key).unwrap();
        let receiver = Type::nominal(
            NominalKind::Struct,
            ident("Box"),
            vec![Type::Int],
            vec![],
            None,
        );
        let static_method = aggregate.methods.get(&ident("make")).unwrap();
        let instance_method = aggregate.methods.get(&ident("get")).unwrap();

        let static_ref =
            index.callable_for_aggregate_method(aggregate, ident("make"), static_method, None);
        let instance_ref = index.callable_for_aggregate_method(
            aggregate,
            ident("get"),
            instance_method,
            Some(receiver.clone()),
        );

        assert_eq!(static_ref.def.id.kind, CallableKind::StaticMethod);
        assert_eq!(instance_ref.def.id.kind, CallableKind::InstanceMethod);
        assert_eq!(instance_ref.receiver_ty, Some(receiver));
        assert_eq!(
            instance_ref.def.sig.params,
            vec![FuncParam::immut(Type::Int)]
        );
        assert_eq!(instance_ref.def.sig.ret, Type::Int);
    }

    #[test]
    fn extend_method_owner_args() {
        let index = index("extend<T> T { fn id(self, x: T) -> T { x } }", &[]);
        let ExtendMethodMatch::Match {
            extend,
            method,
            owner_args: Ok(owner_args),
        } = index
            .find_extend_method(&Type::Int, ident("id"), |_| true)
            .unwrap()
        else {
            panic!("expected extend match");
        };
        let callable = index
            .callable_for_extend_method(Type::Int, extend, ident("id"), method, owner_args.clone())
            .unwrap();

        assert_eq!(callable.def.id.kind, CallableKind::ExtendMethod);
        assert_eq!(callable.owner_args, owner_args);
        assert_eq!(callable.receiver_ty, Some(Type::Int));
        assert_eq!(callable.def.sig.params, vec![FuncParam::immut(Type::Int)]);
        assert_eq!(callable.def.sig.ret, Type::Int);
    }

    #[test]
    fn enum_variant_callables() {
        let index = index("enum E<T> { A, B(T), C { x: T } }", &[]);
        let key = index.local_type(&ModuleScope::Root, ident("E")).unwrap();
        let enm = index.enum_schema(&key).unwrap();

        let unit = index
            .callable_for_variant(&key, ident("A"), enm.variants.get(&ident("A")).unwrap())
            .unwrap();
        let tuple = index
            .callable_for_variant(&key, ident("B"), enm.variants.get(&ident("B")).unwrap())
            .unwrap();

        assert_eq!(unit.def.id.kind, CallableKind::EnumVariant);
        assert!(unit.def.sig.params.is_empty());
        assert_eq!(
            tuple.def.sig.params,
            vec![FuncParam::immut(Type::Var(TypeVarId(0)))]
        );
        assert_eq!(
            tuple.def.sig.ret,
            Type::nominal(
                NominalKind::Enum,
                ident("E"),
                vec![Type::Var(TypeVarId(0))],
                vec![],
                None
            )
        );
        assert!(
            index
                .callable_for_variant(&key, ident("C"), enm.variants.get(&ident("C")).unwrap())
                .is_none()
        );
    }

    #[test]
    fn set_const_type_syncs_copies() {
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
