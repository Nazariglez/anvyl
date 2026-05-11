use std::collections::{HashMap, HashSet, hash_map::Entry};

use super::{
    ConstSubst, GenericArgs, GenericParams, Specificity, TypeSubst, annotation,
    compare_specificity,
    const_term::ConstTerm,
    infer::{GenericSolverSeeds, Solver},
    substitute,
    type_ops::TypeFolder,
    type_refs::{GenericParamError, GenericTypeContext, TypeRefError, TypeRefResolver},
};
use crate::{
    ast::{
        AggregateKind, ArrayLen, ConstArg, ConstParam, FuncParam, GenericArg, Ident,
        ImportItemKind, ImportKind, MethodReceiver, MethodSig, ModuleOrigin, Mutability,
        NominalKind, Param, Program, Stmt, StmtNode, Type, TypeParam, VariantKind, Visibility,
    },
    externs::{
        ExternProvenance, RawExternModule, RawExterns, catalog::ExternCatalog, raw_module_scope,
    },
    resolve::{ModuleId, ModulePath, PackageId, PackageModulePath, ResolveResult, SourceFileId},
    source::SourceId,
    span::{SourceSpan, Span},
    typecheck::annotation::AccessPolicy,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ModuleScope {
    Root,
    Named(ModulePath),
    Package(ModuleId),
}

impl ModuleScope {
    pub(crate) fn from_module_id(module: &ModuleId) -> Self {
        if module.package_context() == Some(&PackageId::synthetic_root()) {
            match module.path() {
                PackageModulePath::Root => return Self::Root,
                PackageModulePath::Named(path) => return Self::Named(path.clone()),
                PackageModulePath::Provider(_) | PackageModulePath::Source(_) => {}
            }
        }
        Self::Package(module.clone())
    }

    fn nominal_origin(&self) -> Option<ModuleOrigin> {
        match self {
            ModuleScope::Root => None,
            ModuleScope::Named(path) => Some(ModuleOrigin::Module(path.to_ast_path())),
            ModuleScope::Package(module) => match module.path() {
                PackageModulePath::Source(file) => Some(ModuleOrigin::SourceFile {
                    package: module
                        .package_context()
                        .map(|package| package.as_str().to_string()),
                    path: file.to_string(),
                }),
                PackageModulePath::Provider(path) => Some(ModuleOrigin::Provider {
                    package: module.package().as_str().to_string(),
                    path: path.to_ast_path(),
                }),
                PackageModulePath::Root => Some(ModuleOrigin::Package {
                    package: module.package().as_str().to_string(),
                    path: None,
                }),
                PackageModulePath::Named(path) => Some(ModuleOrigin::Package {
                    package: module.package().as_str().to_string(),
                    path: Some(path.to_ast_path()),
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
            ModuleOrigin::Provider { package, path } => Self::Package(ModuleId::provider(
                PackageId::new(package.clone()),
                ModulePath::new(path.iter().cloned().collect())
                    .expect("provider origin module path is valid"),
            )),
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
pub(crate) struct TypeAliasKey {
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TypeBinding {
    Nominal(NominalKey),
    Alias(TypeAliasKey),
}

impl TypeBinding {
    pub(crate) fn module(&self) -> &ModuleScope {
        match self {
            Self::Nominal(key) => &key.module,
            Self::Alias(key) => &key.module,
        }
    }

    pub(crate) fn as_nominal(&self) -> Option<&NominalKey> {
        match self {
            Self::Nominal(key) => Some(key),
            Self::Alias(_) => None,
        }
    }

    pub(crate) fn into_nominal(self) -> Option<NominalKey> {
        match self {
            Self::Nominal(key) => Some(key),
            Self::Alias(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LocalCallableId {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl LocalCallableId {
    pub(crate) fn new(span: Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
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

    pub(crate) fn local_function(module: ModuleScope, name: Ident, span: Span) -> Self {
        Self {
            module,
            parent: Some(CallableParent::Local(LocalCallableId::new(span))),
            kind: CallableKind::Function,
            name,
        }
    }

    pub(crate) fn aggregate_method(owner: NominalKey, name: Ident, surface: MethodSurface) -> Self {
        Self {
            module: owner.module.clone(),
            parent: Some(CallableParent::Nominal(owner)),
            kind: match surface {
                MethodSurface::Static => CallableKind::StaticMethod,
                MethodSurface::Instance => CallableKind::InstanceMethod,
            },
            name,
        }
    }

    pub(crate) fn extend_method(extend: ExtendId, name: Ident, surface: MethodSurface) -> Self {
        Self {
            module: extend.module.clone(),
            parent: Some(CallableParent::Extend(extend)),
            kind: CallableKind::ExtendMethod(surface),
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
    Local(LocalCallableId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CallableKind {
    Function,
    ExternFunction,
    StaticMethod,
    InstanceMethod,
    ExtendMethod(MethodSurface),
    EnumVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum MethodMode {
    Static,
    Instance { mutable: bool },
}

impl MethodMode {
    pub(crate) fn from_receiver(receiver: Option<MethodReceiver>) -> Self {
        match receiver {
            Some(MethodReceiver::Value) => Self::Instance { mutable: false },
            Some(MethodReceiver::Var) => Self::Instance { mutable: true },
            None => Self::Static,
        }
    }

    pub(crate) fn surface(self) -> MethodSurface {
        match self {
            Self::Static => MethodSurface::Static,
            Self::Instance { .. } => MethodSurface::Instance,
        }
    }

    pub(crate) fn receiver(self) -> Option<MethodReceiver> {
        match self {
            Self::Static => None,
            Self::Instance { mutable: false } => Some(MethodReceiver::Value),
            Self::Instance { mutable: true } => Some(MethodReceiver::Var),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum MethodSurface {
    Static,
    Instance,
}

impl MethodSurface {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Static => "static",
            Self::Instance => "instance",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct MethodKey {
    pub(crate) name: Ident,
    pub(crate) surface: MethodSurface,
}

impl MethodKey {
    pub(crate) fn new(name: Ident, surface: MethodSurface) -> Self {
        Self { name, surface }
    }

    pub(crate) fn instance(name: Ident) -> Self {
        Self::new(name, MethodSurface::Instance)
    }

    pub(crate) fn static_(name: Ident) -> Self {
        Self::new(name, MethodSurface::Static)
    }
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
        span: Option<SourceSpan>,
    },
    DuplicateType {
        module: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    },
    MissingImportMember {
        module: ModuleScope,
        imported: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    },
    PrivateImportMember {
        module: ModuleScope,
        imported: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    },
    ImportConflict {
        module: ModuleScope,
        name: Ident,
        namespace: BindingNamespace,
        first: BindingOrigin,
        second: BindingOrigin,
        span: Option<SourceSpan>,
    },
    DuplicateModuleBinding {
        module: ModuleScope,
        name: Ident,
        first: BindingOrigin,
        second: BindingOrigin,
        span: Option<SourceSpan>,
    },
    DuplicateGenericParam {
        module: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateAggregateMethod {
        owner: NominalKey,
        name: Ident,
        surface: MethodSurface,
        span: Option<SourceSpan>,
    },
    DuplicateExtendMethod {
        name: Ident,
        surface: MethodSurface,
        span: Option<SourceSpan>,
    },
    DuplicateCastFrom {
        target: Type,
        source: Type,
        span: Option<SourceSpan>,
    },
    PointlessCastFrom {
        ty: Type,
        span: Option<SourceSpan>,
    },
    CastFromReturnMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    UnsupportedExtendTarget {
        ty: Type,
        span: Option<SourceSpan>,
    },
    UnusedExtendTypeParam {
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnusedExtendConstParam {
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnusedAliasTypeParam {
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnusedAliasConstParam {
        name: Ident,
        span: Option<SourceSpan>,
    },
    PublicAliasPrivateType {
        name: Ident,
        ty: Type,
        span: Option<SourceSpan>,
    },
    ExtendMethodConflict {
        ty: Type,
        name: Ident,
        surface: MethodSurface,
        span: Option<SourceSpan>,
    },
    ReexportConflict {
        module: ModuleScope,
        name: Ident,
        namespace: BindingNamespace,
        first: BindingOrigin,
        second: BindingOrigin,
        span: Option<SourceSpan>,
    },
    UnknownType {
        module: ModuleScope,
        qualifier: Option<Ident>,
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnknownAnnotation {
        name: Ident,
        span: Option<SourceSpan>,
    },
    InvalidAnnotationTarget {
        name: Ident,
        target: String,
        valid_targets: String,
        span: Option<SourceSpan>,
    },
    DuplicateAnnotation {
        name: Ident,
        span: Option<SourceSpan>,
    },
    InvalidAnnotationArgs {
        name: Ident,
        message: String,
        span: Option<SourceSpan>,
    },
    InternalOnToString {
        span: Option<SourceSpan>,
    },
    InvalidToStringMethod {
        message: &'static str,
        span: Option<SourceSpan>,
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

fn core_module_scope(segment: &str) -> ModuleScope {
    let path = ModulePath::new(vec![segment.to_string()]).expect("core module segment is valid");
    ModuleScope::Package(ModuleId::named(PackageId::core(), path))
}

#[derive(Clone, Default)]
pub(crate) struct DeclarationIndex {
    modules: HashMap<ModuleScope, ModuleDecls>,
    aggregates: HashMap<NominalKey, AggregateSchema>,
    enums: HashMap<NominalKey, EnumSchema>,
    extends: Vec<ExtendSchema>,
    type_aliases: HashMap<TypeAliasKey, TypeAliasSchema>,
    extern_type_policies: HashMap<NominalKey, AccessPolicy>,
    value_spans: HashMap<(ModuleScope, Ident), Span>,
    type_spans: HashMap<NominalKey, Span>,
    errors: Vec<DeclError>,
}

#[derive(Default)]
struct SourceExternPolicies {
    funcs: HashMap<(ModuleScope, Ident), AccessPolicy>,
    types: HashMap<(ModuleScope, Ident), AccessPolicy>,
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
    pub(crate) types: HashMap<Ident, TypeBinding>,
    pub(crate) modules: HashMap<Ident, ModuleScope>,
}

pub(crate) type ModuleExports = Namespace;

pub(crate) enum ModuleMemberLookup<T> {
    Found(T),
    Private,
    Missing,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MapKeyError {
    pub(crate) ty: Type,
    pub(crate) field: Option<Ident>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CoreRangeKind {
    Exclusive,
    Inclusive,
    From,
    To,
    ToInclusive,
}

impl CoreRangeKind {
    const ALL: [Self; 5] = [
        Self::Exclusive,
        Self::Inclusive,
        Self::From,
        Self::To,
        Self::ToInclusive,
    ];

    fn name(self) -> &'static str {
        match self {
            Self::Exclusive => "Range",
            Self::Inclusive => "RangeInclusive",
            Self::From => "RangeFrom",
            Self::To => "RangeTo",
            Self::ToInclusive => "RangeToInclusive",
        }
    }
}

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
    exported_active_modules: HashSet<ModuleScope>,
    imports: ImportScope,
}

#[derive(Debug, Clone)]
pub(crate) enum ValueDecl {
    Func(FuncSig),
    Const(ConstSig),
}

impl Namespace {
    fn value(&self, name: Ident) -> Option<&ResolvedValue> {
        self.values.get(&name)
    }

    fn ty(&self, name: Ident) -> Option<&TypeBinding> {
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

    fn insert_type(&mut self, visible: Ident, binding: TypeBinding) {
        self.types.insert(visible, binding);
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
        span: Option<SourceSpan>,
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
        span: Option<SourceSpan>,
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
        if dep.locals.contains_member(source_name)
            || dep.imports.namespace.contains_member(source_name)
        {
            self.push_private_member(origin_module.clone(), source_name, span);
        } else {
            self.push_missing_member(origin_module.clone(), source_name, span);
        }
    }

    fn push_private_member(
        &mut self,
        imported: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    ) {
        self.errors.push(DeclError::PrivateImportMember {
            module: self.module.clone(),
            imported,
            name,
            span,
        });
    }

    fn push_missing_member(
        &mut self,
        imported: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    ) {
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
        span: Option<SourceSpan>,
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
        span: Option<SourceSpan>,
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
        binding: TypeBinding,
        origin: BindingOrigin,
        span: Option<SourceSpan>,
    ) -> bool {
        if !self.claim_origin(BindingNamespace::Type, name, origin, span) {
            return false;
        }
        self.namespace.insert_type(name, binding);
        true
    }

    fn insert_module(
        &mut self,
        name: Ident,
        module: ModuleScope,
        origin: BindingOrigin,
        span: Option<SourceSpan>,
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
        span: Option<SourceSpan>,
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
        span: Option<SourceSpan>,
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

    fn finish_reexport_scope(self) -> (Namespace, HashSet<ModuleScope>, Vec<DeclError>) {
        (self.namespace, self.active_modules, self.errors)
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
        self.active_modules.extend(
            self.namespace
                .types
                .values()
                .map(|binding| binding.module().clone()),
        );
        self.active_modules
            .extend(self.namespace.modules.values().cloned());
    }
}

impl ValueDecl {
    pub(crate) fn ty(&self) -> &Type {
        match self {
            ValueDecl::Func(sig) => &sig.ty,
            ValueDecl::Const(sig) => &sig.ty,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FuncSig {
    pub(crate) kind: CallableKind,
    pub(crate) generics: GenericParams,
    pub(crate) ty: Type,
    pub(crate) required_params: usize,
    pub(crate) policy: AccessPolicy,
}

#[derive(Debug, Clone)]
pub(crate) struct ConstSig {
    pub(crate) ty: Type,
    pub(crate) policy: AccessPolicy,
}

#[derive(Debug, Clone)]
pub(crate) struct CallableSig {
    pub(crate) owner_generics: GenericParams,
    pub(crate) generics: GenericParams,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) required_params: usize,
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
    pub(crate) methods: HashMap<MethodKey, MethodSchema>,
    pub(crate) policy: AccessPolicy,
}

#[derive(Clone)]
pub(crate) struct TypeAliasDef {
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
    pub(crate) generics: GenericParams,
    pub(crate) generic_context: GenericTypeContext,
    pub(crate) aliased: Type,
    pub(crate) policy: AccessPolicy,
    pub(crate) span: SourceSpan,
}

#[derive(Clone)]
pub(crate) struct TypeAliasSchema {
    pub(crate) def: TypeAliasDef,
    pub(crate) visibility: Visibility,
}

#[derive(Clone)]
pub(crate) struct FieldSchema {
    pub(crate) ty: Type,
    pub(crate) has_default: bool,
    pub(crate) policy: AccessPolicy,
}

#[derive(Clone)]
pub(crate) struct MethodSchema {
    pub(crate) generics: GenericParams,
    pub(crate) mode: MethodMode,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) required_params: usize,
    pub(crate) ret: Type,
    pub(crate) policy: AccessPolicy,
}

#[derive(Clone)]
pub(crate) struct EnumSchema {
    pub(crate) generics: GenericParams,
    pub(crate) variants: HashMap<Ident, VariantSchema>,
    pub(crate) policy: AccessPolicy,
}

#[derive(Clone)]
pub(crate) struct VariantSchema {
    pub(crate) policy: AccessPolicy,
    pub(crate) payload: VariantPayload,
}

#[derive(Clone)]
pub(crate) enum VariantPayload {
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
    pub(crate) methods: HashMap<MethodKey, ExtendMethodSchema>,
    pub(crate) cast_froms: Vec<CastConversionSchema>,
    pub(crate) span: SourceSpan,
}

#[derive(Clone)]
pub(crate) struct CastConversionSchema {
    pub(crate) source: Type,
    pub(crate) ret: Option<Type>,
    pub(crate) span: SourceSpan,
}

#[derive(Clone)]
pub(crate) struct ExtendMethodSchema {
    pub(crate) mode: MethodMode,
    pub(crate) generics: GenericParams,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) required_params: usize,
    pub(crate) ret: Type,
    pub(crate) policy: AccessPolicy,
}

pub(crate) enum ExtendMethodMatch<'a> {
    Match {
        extend: &'a ExtendSchema,
        method: &'a ExtendMethodSchema,
        receiver_ty: Type,
        owner_args: Result<GenericArgs, Vec<Ident>>,
    },
    Ambiguous,
}

pub(crate) enum CastConversionMatch {
    Match,
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

struct ModuleProgram<'a> {
    scope: ModuleScope,
    source: SourceId,
    program: &'a Program,
}

impl DeclarationIndex {
    pub(crate) fn from_root_and_modules(
        root: &Program,
        resolved: &ResolveResult,
        externs: &RawExterns,
    ) -> Self {
        let mut index = Self::default();
        let modules = Self::module_programs(root, resolved);
        let mut source_extern_policies = SourceExternPolicies::default();
        for module in &modules {
            index.collect_module(
                module.program,
                module.scope.clone(),
                module.source,
                Self::is_export_all_scope(&module.scope, resolved),
                &mut source_extern_policies,
            );
        }
        index.collect_extern_headers(externs, &source_extern_policies);
        index.apply_public_import_reexports(&modules, resolved);
        index.close_exported_active_modules();
        index.build_import_scopes(&modules, resolved);
        index
    }

    fn module_programs<'a>(
        root: &'a Program,
        resolved: &'a ResolveResult,
    ) -> Vec<ModuleProgram<'a>> {
        let mut modules = vec![ModuleProgram {
            scope: ModuleScope::from_module_id(&resolved.root),
            source: resolved.root_source,
            program: root,
        }];
        for group in &resolved.module_groups {
            for module in group {
                if module.key == resolved.root {
                    continue;
                }
                modules.push(ModuleProgram {
                    scope: ModuleScope::from_module_id(&module.key),
                    source: module.source,
                    program: &module.program,
                });
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
            sig.required_params = function.signature.params.len();
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
            let Some(span) = self.type_span(&key) else {
                continue;
            };
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
            let Some(span) = self.type_span(&key) else {
                continue;
            };
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
                match &mut variant.payload {
                    VariantPayload::Unit => {}
                    VariantPayload::Tuple(types) => {
                        for ty in types {
                            let site = DeclTypeSite {
                                module: key.module.clone(),
                                span,
                                generics: generics.clone(),
                            };
                            *ty = f(site, ty.clone());
                        }
                    }
                    VariantPayload::Struct(fields) => {
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

        let alias_keys = self.type_aliases.keys().cloned().collect::<Vec<_>>();
        for key in alias_keys {
            let Some(schema) = self.type_aliases.get_mut(&key) else {
                continue;
            };
            let generics = generic_context(
                schema.def.module.clone(),
                &schema.def.generics.type_params,
                &schema.def.generics.const_params,
                schema.def.span.byte(),
                &mut errors,
            );
            schema.def.generic_context = generics.clone();
            let site = DeclTypeSite {
                module: schema.def.module.clone(),
                span: schema.def.span.byte(),
                generics,
            };
            schema.def.aliased = f(site, schema.def.aliased.clone());
        }

        for index in 0..self.extends.len() {
            let origin = self.extends[index].origin.clone();
            let span = self.extends[index].span.byte();
            let extend = &mut self.extends[index];
            let generics = generic_context(
                origin.clone(),
                &extend.generics.type_params,
                &extend.generics.const_params,
                span,
                &mut errors,
            );
            let target_site = DeclTypeSite {
                module: origin.clone(),
                span,
                generics: generics.clone(),
            };
            extend.target = f(target_site, extend.target.clone());
            for method in extend.methods.values_mut() {
                let method_generics = extend_generic_context(
                    origin.clone(),
                    &generics,
                    &method.generics.type_params,
                    &method.generics.const_params,
                    span,
                    &mut errors,
                );
                for param in &mut method.params {
                    let site = DeclTypeSite {
                        module: origin.clone(),
                        span,
                        generics: method_generics.clone(),
                    };
                    param.ty = f(site, param.ty.clone());
                }
                let site = DeclTypeSite {
                    module: origin.clone(),
                    span,
                    generics: method_generics,
                };
                method.ret = f(site, method.ret.clone());
            }
            for cast in &mut extend.cast_froms {
                let site = DeclTypeSite {
                    module: origin.clone(),
                    span: cast.span.byte(),
                    generics: generics.clone(),
                };
                cast.source = f(site.clone(), cast.source.clone());
                if let Some(ret) = &mut cast.ret {
                    *ret = f(site, ret.clone());
                }
            }
        }

        let module_keys = self.modules.keys().cloned().collect::<Vec<_>>();
        for module in module_keys {
            let Some(decls) = self.modules.get_mut(&module) else {
                continue;
            };
            for value in decls.locals.values.values_mut() {
                let Some(span) = self
                    .value_spans
                    .get(&(value.module.clone(), value.name))
                    .copied()
                else {
                    continue;
                };
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
                    ValueDecl::Const(sig) => sig.ty = f(site, sig.ty.clone()),
                }
            }
        }

        self.sync_value_projections();
        errors
    }

    pub(crate) fn type_span(&self, key: &NominalKey) -> Option<Span> {
        self.type_spans.get(key).copied()
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
        span: Option<SourceSpan>,
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
        if let Some(span) = span {
            self.value_spans.insert((scope.clone(), name), span.byte());
        }
        true
    }

    fn insert_local_type(
        &mut self,
        decls: &mut ModuleDecls,
        scope: &ModuleScope,
        name: Ident,
        binding: TypeBinding,
        exported: bool,
        span: Option<SourceSpan>,
    ) -> bool {
        if decls.locals.types.contains_key(&name) {
            self.errors.push(DeclError::DuplicateType {
                module: scope.clone(),
                name,
                span,
            });
            return false;
        }
        decls.locals.insert_type(name, binding.clone());
        if exported {
            decls.exports.insert_type(name, binding);
        }
        true
    }

    fn validate_to_string_method(&mut self, sig: &MethodSig, span: SourceSpan) {
        let message = match sig.receiver {
            None => Some("to_string method must have a 'self' receiver"),
            Some(MethodReceiver::Var) => Some("to_string method must be 'self', not 'var self'"),
            Some(MethodReceiver::Value) if !sig.params.is_empty() => {
                Some("to_string method must take no parameters")
            }
            Some(MethodReceiver::Value) if sig.ret != Type::String => {
                Some("to_string method must return 'string'")
            }
            Some(MethodReceiver::Value) => None,
        };
        if let Some(message) = message {
            self.errors.push(DeclError::InvalidToStringMethod {
                message,
                span: Some(span),
            });
        }
    }

    fn collect_module(
        &mut self,
        program: &Program,
        scope: ModuleScope,
        source: SourceId,
        export_all: bool,
        source_extern_policies: &mut SourceExternPolicies,
    ) {
        let mut decls = ModuleDecls::default();
        let mut extend_index = 0;

        for stmt in &program.stmts {
            let exported = export_all || matches!(stmt_visibility(stmt), Visibility::Public);
            match &stmt.node {
                Stmt::Func(func_node) => {
                    let func = &func_node.node;
                    let policy = annotation::normalize_annotations(
                        source,
                        &func.annotations,
                        annotation::AnnotationTarget::Func,
                        &mut self.errors,
                    );
                    let ty = func_type_from_params(&func.params, &func.ret);
                    let value = ResolvedValue {
                        module: scope.clone(),
                        name: func.name,
                        decl: ValueDecl::Func(FuncSig {
                            kind: CallableKind::Function,
                            generics: generic_params(&func.type_params, &func.const_params),
                            ty,
                            required_params: required_param_count(&func.params),
                            policy,
                        }),
                    };
                    self.insert_local_value(
                        &mut decls,
                        &scope,
                        func.name,
                        value,
                        exported,
                        Some(SourceSpan::from_byte_span(source, func_node.span)),
                    );
                }
                Stmt::Aggregate(agg_node) => {
                    let agg = &agg_node.node;
                    let key = NominalKey {
                        module: scope.clone(),
                        kind: agg.kind.into(),
                        name: agg.name,
                    };
                    let target = match agg.kind {
                        AggregateKind::Struct => annotation::AnnotationTarget::Struct,
                        AggregateKind::DataRef => annotation::AnnotationTarget::DataRef,
                    };
                    let policy = annotation::normalize_annotations(
                        source,
                        &agg.annotations,
                        target,
                        &mut self.errors,
                    );
                    let mut fields = HashMap::new();
                    for field in &agg.fields {
                        let policy = annotation::normalize_annotations(
                            source,
                            &field.annotations,
                            annotation::AnnotationTarget::Field,
                            &mut self.errors,
                        );
                        fields.insert(
                            field.name,
                            FieldSchema {
                                ty: field.ty.clone(),
                                has_default: field.default.is_some(),
                                policy,
                            },
                        );
                    }
                    let mut methods = HashMap::new();
                    for method in &agg.methods {
                        let policy = annotation::normalize_annotations(
                            source,
                            &method.annotations,
                            annotation::AnnotationTarget::InlineMethod,
                            &mut self.errors,
                        );
                        if method.sig.name == Ident::new("to_string") {
                            if policy.has_internal() {
                                self.errors.push(DeclError::InternalOnToString {
                                    span: Some(SourceSpan::from_byte_span(source, agg_node.span)),
                                });
                            }
                            self.validate_to_string_method(
                                &method.sig,
                                SourceSpan::from_byte_span(source, agg_node.span),
                            );
                        }
                        let mode = MethodMode::from_receiver(method.sig.receiver);
                        let method_key = MethodKey::new(method.sig.name, mode.surface());
                        let schema = MethodSchema {
                            generics: generic_params(
                                &method.sig.type_params,
                                &method.sig.const_params,
                            ),
                            mode,
                            params: resolve_func_params(&method.sig.params),
                            required_params: required_param_count(&method.sig.params),
                            ret: method.sig.ret.clone(),
                            policy,
                        };
                        match methods.entry(method_key) {
                            Entry::Occupied(entry) => {
                                self.errors.push(DeclError::DuplicateAggregateMethod {
                                    owner: key.clone(),
                                    name: method.sig.name,
                                    surface: entry.key().surface,
                                    span: Some(SourceSpan::from_byte_span(source, agg_node.span)),
                                });
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(schema);
                            }
                        }
                    }
                    if self.insert_local_type(
                        &mut decls,
                        &scope,
                        agg.name,
                        TypeBinding::Nominal(key.clone()),
                        exported,
                        Some(SourceSpan::from_byte_span(source, agg_node.span)),
                    ) {
                        self.type_spans.insert(key.clone(), agg_node.span);
                        self.aggregates.insert(
                            key.clone(),
                            AggregateSchema {
                                key,
                                generics: generic_params(&agg.type_params, &agg.const_params),
                                fields,
                                methods,
                                policy,
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
                    let policy = annotation::normalize_annotations(
                        source,
                        &enm.annotations,
                        annotation::AnnotationTarget::Enum,
                        &mut self.errors,
                    );
                    let mut variants = HashMap::new();
                    for variant in &enm.variants {
                        let variant_policy = annotation::normalize_annotations(
                            source,
                            &variant.annotations,
                            annotation::AnnotationTarget::Variant,
                            &mut self.errors,
                        );
                        let payload = match &variant.kind {
                            VariantKind::Unit => VariantPayload::Unit,
                            VariantKind::Tuple(types) => VariantPayload::Tuple(types.clone()),
                            VariantKind::Struct(fields) => {
                                let mut field_map = HashMap::new();
                                for f in fields {
                                    let policy = annotation::normalize_annotations(
                                        source,
                                        &f.annotations,
                                        annotation::AnnotationTarget::Field,
                                        &mut self.errors,
                                    );
                                    field_map.insert(
                                        f.name,
                                        FieldSchema {
                                            ty: f.ty.clone(),
                                            has_default: f.default.is_some(),
                                            policy,
                                        },
                                    );
                                }
                                VariantPayload::Struct(field_map)
                            }
                        };
                        variants.insert(
                            variant.name,
                            VariantSchema {
                                policy: variant_policy,
                                payload,
                            },
                        );
                    }
                    if self.insert_local_type(
                        &mut decls,
                        &scope,
                        enm.name,
                        TypeBinding::Nominal(key.clone()),
                        exported,
                        Some(SourceSpan::from_byte_span(source, enum_node.span)),
                    ) {
                        self.type_spans.insert(key.clone(), enum_node.span);
                        self.enums.insert(
                            key.clone(),
                            EnumSchema {
                                generics: generic_params(&enm.type_params, &enm.const_params),
                                variants,
                                policy,
                            },
                        );
                    }
                }
                Stmt::Const(const_node) => {
                    let c = &const_node.node;
                    let policy = annotation::normalize_annotations(
                        source,
                        &c.annotations,
                        annotation::AnnotationTarget::Const,
                        &mut self.errors,
                    );
                    let value = ResolvedValue {
                        module: scope.clone(),
                        name: c.name,
                        decl: ValueDecl::Const(ConstSig {
                            ty: c.ty.clone().unwrap_or(Type::Infer),
                            policy,
                        }),
                    };
                    self.insert_local_value(
                        &mut decls,
                        &scope,
                        c.name,
                        value,
                        exported,
                        Some(SourceSpan::from_byte_span(source, const_node.span)),
                    );
                }
                Stmt::TypeAlias(alias_node) => {
                    let alias = &alias_node.node;
                    let policy = annotation::normalize_annotations(
                        source,
                        &alias.annotations,
                        annotation::AnnotationTarget::TypeAlias,
                        &mut self.errors,
                    );
                    let key = TypeAliasKey {
                        module: scope.clone(),
                        name: alias.name,
                    };
                    if self.insert_local_type(
                        &mut decls,
                        &scope,
                        alias.name,
                        TypeBinding::Alias(key.clone()),
                        exported,
                        Some(SourceSpan::from_byte_span(source, alias_node.span)),
                    ) {
                        let generics = generic_params(&alias.type_params, &alias.const_params);
                        let generic_context = GenericTypeContext::try_from_params(
                            &alias.type_params,
                            &alias.const_params,
                        )
                        .unwrap_or_default();
                        let span = SourceSpan::from_byte_span(source, alias_node.span);
                        self.type_aliases.insert(
                            key,
                            TypeAliasSchema {
                                def: TypeAliasDef {
                                    module: scope.clone(),
                                    name: alias.name,
                                    generics,
                                    generic_context,
                                    aliased: alias.aliased.clone(),
                                    policy,
                                    span,
                                },
                                visibility: alias.visibility,
                            },
                        );
                    }
                }
                Stmt::ExternFunc(func_node) => {
                    let policy = annotation::normalize_annotations(
                        source,
                        &func_node.node.annotations,
                        annotation::AnnotationTarget::ExternFunc,
                        &mut self.errors,
                    );
                    source_extern_policies
                        .funcs
                        .insert((scope.clone(), func_node.node.name), policy);
                }
                Stmt::ExternType(ty_node) => {
                    let policy = annotation::normalize_annotations(
                        source,
                        &ty_node.node.annotations,
                        annotation::AnnotationTarget::ExternType,
                        &mut self.errors,
                    );
                    source_extern_policies
                        .types
                        .insert((scope.clone(), ty_node.node.name), policy);
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
                    let generics = generic_params(&ext.type_params, &ext.const_params);
                    let mut methods = HashMap::new();
                    for method_node in &ext.methods {
                        let m = &method_node.node;
                        let policy = annotation::normalize_annotations(
                            source,
                            &m.annotations,
                            annotation::AnnotationTarget::ExtendMethod,
                            &mut self.errors,
                        );
                        let mode = MethodMode::from_receiver(m.sig.receiver);
                        let key = MethodKey::new(m.sig.name, mode.surface());
                        let schema = ExtendMethodSchema {
                            mode,
                            generics: generic_params(&m.sig.type_params, &m.sig.const_params),
                            params: resolve_func_params(&m.sig.params),
                            required_params: required_param_count(&m.sig.params),
                            ret: m.sig.ret.clone(),
                            policy,
                        };
                        if methods.contains_key(&key)
                            || self.extends.iter().any(|prior| {
                                prior.origin == scope
                                    && same_extend_target(
                                        &prior.target,
                                        &prior.generics,
                                        &ext.ty,
                                        &generics,
                                    )
                                    && prior.methods.contains_key(&key)
                            })
                        {
                            self.errors.push(DeclError::DuplicateExtendMethod {
                                name: m.sig.name,
                                surface: key.surface,
                                span: Some(SourceSpan::from_byte_span(source, method_node.span)),
                            });
                        } else {
                            methods.insert(key, schema);
                        }
                    }
                    let cast_froms = ext
                        .cast_froms
                        .iter()
                        .map(|cast| CastConversionSchema {
                            source: cast.node.param.ty.clone(),
                            ret: cast.node.ret.clone(),
                            span: SourceSpan::from_byte_span(source, cast.span),
                        })
                        .collect();
                    self.extends.push(ExtendSchema {
                        id,
                        origin: scope.clone(),
                        target: ext.ty.clone(),
                        generics,
                        methods,
                        cast_froms,
                        span: SourceSpan::from_byte_span(source, extend_node.span),
                    });
                }
                _ => {}
            }
        }

        self.modules.insert(scope, decls);
    }

    fn collect_extern_headers(
        &mut self,
        externs: &RawExterns,
        source_policies: &SourceExternPolicies,
    ) {
        for group in &externs.groups {
            let policies = match &group.provenance {
                ExternProvenance::Source { .. } => Some(source_policies),
                ExternProvenance::Provider { .. } => None,
            };
            for module in &group.modules {
                self.collect_extern_module(module, policies);
            }
        }
    }

    fn collect_extern_module(
        &mut self,
        module: &RawExternModule,
        source_policies: Option<&SourceExternPolicies>,
    ) {
        let scope = raw_module_scope(&module.scope);
        let mut decls = self.modules.remove(&scope).unwrap_or_default();

        for ty in &module.types {
            let name = Ident::new(&ty.name);
            let key = NominalKey {
                module: scope.clone(),
                kind: NominalKind::Extern,
                name,
            };
            let span = ty.site.span;
            if self.insert_local_type(
                &mut decls,
                &scope,
                name,
                TypeBinding::Nominal(key.clone()),
                ty.exported,
                span,
            ) {
                if let Some(span) = span {
                    self.type_spans.insert(key.clone(), span.byte());
                }
                let policy = source_policies
                    .and_then(|policies| policies.types.get(&(scope.clone(), name)))
                    .cloned()
                    .unwrap_or_default();
                self.extern_type_policies.insert(key, policy);
            }
        }

        for func in &module.functions {
            let name = Ident::new(&func.decl.name);
            let policy = source_policies
                .and_then(|policies| policies.funcs.get(&(scope.clone(), name)))
                .cloned()
                .unwrap_or_default();
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
                    required_params: 0,
                    policy,
                }),
            };
            self.insert_local_value(
                &mut decls,
                &scope,
                name,
                value,
                func.exported,
                func.site.span,
            );
        }

        self.modules.insert(scope, decls);
    }

    fn resolve_import_target(
        &mut self,
        current: &ModuleScope,
        ordinal: usize,
        span: SourceSpan,
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
        span: SourceSpan,
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
                        span: Some(span),
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
        modules: &[ModuleProgram<'_>],
        resolved: &ResolveResult,
    ) {
        for module in modules {
            let Some(exports) = self
                .modules
                .get(&module.scope)
                .map(|decls| decls.exports.clone())
            else {
                continue;
            };

            let mut builder = ImportScopeBuilder::with_namespace(
                module.scope.clone(),
                exports,
                ImportMode::Reexport,
            );
            self.apply_program_imports(
                &module.scope,
                module.source,
                module.program,
                resolved,
                &mut builder,
            );

            let (exports, active_modules, errors) = builder.finish_reexport_scope();
            self.errors.extend(errors);
            if let Some(decls) = self.modules.get_mut(&module.scope) {
                decls.exports = exports;
                decls.exported_active_modules = active_modules;
            }
        }
    }

    fn close_exported_active_modules(&mut self) {
        loop {
            let snapshot = self
                .modules
                .iter()
                .map(|(module, decls)| (module.clone(), decls.exported_active_modules.clone()))
                .collect::<HashMap<_, _>>();
            let mut changed = false;

            for (module, decls) in &mut self.modules {
                let owner = module.clone();
                let direct = decls
                    .exported_active_modules
                    .iter()
                    .cloned()
                    .collect::<Vec<_>>();
                for active in direct {
                    let Some(surface) = snapshot.get(&active) else {
                        continue;
                    };
                    for exported in surface {
                        if exported == &owner {
                            continue;
                        }
                        changed |= decls.exported_active_modules.insert(exported.clone());
                    }
                }
                decls.exported_active_modules.remove(&owner);
            }

            if !changed {
                break;
            }
        }
    }

    fn expand_active_modules(&self, active_modules: &mut HashSet<ModuleScope>) {
        let roots = active_modules.iter().cloned().collect::<Vec<_>>();
        for module in roots {
            let Some(decls) = self.modules.get(&module) else {
                continue;
            };
            active_modules.extend(decls.exported_active_modules.iter().cloned());
        }
    }

    fn build_import_scopes(&mut self, modules: &[ModuleProgram<'_>], resolved: &ResolveResult) {
        for module in modules.iter().skip(1).chain(modules.first()) {
            let mut builder = ImportScopeBuilder::new(module.scope.clone(), ImportMode::Import);
            if let Some(decls) = self.modules.get(&module.scope) {
                builder.seed_origins(&decls.locals, &BindingOrigin::Local);
            }
            self.apply_core_prelude_import(&module.scope, resolved, &mut builder);
            self.apply_program_imports(
                &module.scope,
                module.source,
                module.program,
                resolved,
                &mut builder,
            );
            let (mut imports, errors) = builder.finish_import_scope();
            self.errors.extend(errors);
            imports.activate_imported_origins();
            self.expand_active_modules(&mut imports.active_modules);
            if let Some(decls) = self.modules.get_mut(&module.scope) {
                decls.imports = imports;
            }
        }
    }

    fn apply_core_prelude_import(
        &self,
        scope: &ModuleScope,
        resolved: &ResolveResult,
        builder: &mut ImportScopeBuilder,
    ) {
        let Some(core_package) = &resolved.system.core else {
            return;
        };
        if package_for_scope(scope) == *core_package {
            return;
        }
        let core = ModuleScope::from_module_id(&ModuleId::root(core_package.clone()));
        let Some(decls) = self.modules.get(&core) else {
            return;
        };
        builder.copy_wildcard_members(&decls.exports, &core, None);
        builder.active_modules.insert(core);
    }

    fn apply_program_imports(
        &mut self,
        scope: &ModuleScope,
        source: SourceId,
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
            let import_span = SourceSpan::from_byte_span(source, import.span);
            let Some(target) =
                self.resolve_import_target(scope, import_ordinal, import_span, resolved)
            else {
                continue;
            };
            builder.active_modules.insert(target.module.clone());
            let dep = self.modules.get(&target.module);
            let validate_members = builder.mode == ImportMode::Reexport || !is_public;
            builder.apply_import(
                &import.node.kind,
                target.module,
                target.default_name,
                dep,
                Some(import_span),
                validate_members,
            );
        }
    }

    pub(crate) fn local_value(&self, module: &ModuleScope, name: Ident) -> Option<ResolvedValue> {
        self.modules.get(module)?.locals.value(name).cloned()
    }

    pub(crate) fn local_type_binding(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<TypeBinding> {
        self.modules.get(module)?.locals.ty(name).cloned()
    }

    pub(crate) fn local_nominal_type(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<NominalKey> {
        self.local_type_binding(module, name)?.into_nominal()
    }

    pub(crate) fn exported_value(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<ResolvedValue> {
        self.modules.get(module)?.exports.value(name).cloned()
    }

    pub(crate) fn exported_type_binding(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<TypeBinding> {
        self.modules.get(module)?.exports.ty(name).cloned()
    }

    pub(crate) fn exported_nominal_type(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<NominalKey> {
        self.exported_type_binding(module, name)?.into_nominal()
    }

    pub(crate) fn exported_module(&self, module: &ModuleScope, name: Ident) -> Option<ModuleScope> {
        self.modules.get(module)?.exports.module(name).cloned()
    }

    pub(crate) fn module_value(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> ModuleMemberLookup<ResolvedValue> {
        let Some(decls) = self.modules.get(module) else {
            return ModuleMemberLookup::Missing;
        };
        if let Some(value) = decls.exports.value(name) {
            return ModuleMemberLookup::Found(value.clone());
        }
        if decls.locals.values.contains_key(&name)
            || decls.imports.namespace.values.contains_key(&name)
        {
            return ModuleMemberLookup::Private;
        }
        ModuleMemberLookup::Missing
    }

    pub(crate) fn module_type(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> ModuleMemberLookup<TypeBinding> {
        let Some(decls) = self.modules.get(module) else {
            return ModuleMemberLookup::Missing;
        };
        if let Some(key) = decls.exports.ty(name) {
            return ModuleMemberLookup::Found(key.clone());
        }
        if decls.locals.types.contains_key(&name)
            || decls.imports.namespace.types.contains_key(&name)
        {
            return ModuleMemberLookup::Private;
        }
        ModuleMemberLookup::Missing
    }

    pub(crate) fn module_module(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> ModuleMemberLookup<ModuleScope> {
        let Some(decls) = self.modules.get(module) else {
            return ModuleMemberLookup::Missing;
        };
        if let Some(module) = decls.exports.module(name) {
            return ModuleMemberLookup::Found(module.clone());
        }
        if decls.locals.modules.contains_key(&name)
            || decls.imports.namespace.modules.contains_key(&name)
        {
            return ModuleMemberLookup::Private;
        }
        ModuleMemberLookup::Missing
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

    pub(crate) fn imported_type_binding(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<TypeBinding> {
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

    pub(crate) fn visible_type_binding(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<TypeBinding> {
        self.local_type_binding(module, name)
            .or_else(|| self.imported_type_binding(module, name))
    }

    pub(crate) fn resolve_visible_type_binding(
        &self,
        module: &ModuleScope,
        qualifier: Option<Ident>,
        name: Ident,
    ) -> Option<TypeBinding> {
        match qualifier {
            Some(alias) => {
                let target = self.imported_module(module, alias)?;
                self.exported_type_binding(&target, name)
            }
            None => self.visible_type_binding(module, name),
        }
    }

    pub(crate) fn resolve_visible_nominal_key(
        &self,
        module: &ModuleScope,
        qualifier: Option<Ident>,
        name: Ident,
    ) -> Option<NominalKey> {
        self.resolve_visible_type_binding(module, qualifier, name)?
            .into_nominal()
    }

    pub(crate) fn imports_module(&self, module: &ModuleScope, imported: &ModuleScope) -> bool {
        self.modules
            .get(module)
            .is_some_and(|decls| decls.imports.active_modules.contains(imported))
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
                && let ValueDecl::Const(sig) = &mut value.decl
            {
                sig.ty = ty.clone();
            }
        }
    }

    pub(crate) fn aggregate(&self, key: &NominalKey) -> Option<&AggregateSchema> {
        self.aggregates.get(key)
    }

    pub(crate) fn aggregates(&self) -> impl Iterator<Item = (&NominalKey, &AggregateSchema)> {
        self.aggregates.iter()
    }

    pub(crate) fn enum_schema(&self, key: &NominalKey) -> Option<&EnumSchema> {
        self.enums.get(key)
    }

    pub(crate) fn enums(&self) -> impl Iterator<Item = (&NominalKey, &EnumSchema)> {
        self.enums.iter()
    }

    pub(crate) fn extern_type_policy(&self, key: &NominalKey) -> Option<&AccessPolicy> {
        self.extern_type_policies.get(key)
    }

    pub(crate) fn type_alias(&self, key: &TypeAliasKey) -> Option<&TypeAliasSchema> {
        self.type_aliases.get(key)
    }

    pub(crate) fn type_aliases(&self) -> impl Iterator<Item = &TypeAliasSchema> {
        self.type_aliases.values()
    }

    pub(crate) fn nominal_generics(&self, key: &NominalKey) -> Option<GenericParams> {
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                self.aggregate(key).map(|schema| schema.generics.clone())
            }
            NominalKind::Enum => self.enum_schema(key).map(|schema| schema.generics.clone()),
            NominalKind::Extern => self
                .modules
                .get(&key.module)?
                .locals
                .ty(key.name)?
                .as_nominal()
                .filter(|found| *found == key)
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
        self.local_nominal_type(&scope, nominal.name)
            .filter(|key| key.kind == nominal.kind)
    }

    pub(crate) fn core_result_key(&self) -> Option<NominalKey> {
        self.core_enum_key("result", "Result")
    }

    pub(crate) fn core_option_of(&self, inner: Type) -> Option<Type> {
        let key = self.core_option_key()?;
        Some(nominal_type_with_args(&key, &[inner], &[]))
    }

    pub(crate) fn core_option_inner<'a>(&self, ty: &'a Type) -> Option<&'a Type> {
        if self.key_for_type(ty)? != self.core_option_key()? {
            return None;
        }
        let Type::Nominal(nominal) = ty else {
            return None;
        };
        let [inner] = nominal.type_args.as_slice() else {
            return None;
        };
        Some(inner)
    }

    pub(crate) fn core_range_of(&self, kind: CoreRangeKind, inner: Type) -> Option<Type> {
        let key = self.core_range_key(kind)?;
        Some(nominal_type_with_args(&key, &[inner], &[]))
    }

    pub(crate) fn core_range_kind(&self, ty: &Type) -> Option<CoreRangeKind> {
        let key = self.key_for_type(ty)?;
        CoreRangeKind::ALL
            .iter()
            .copied()
            .find(|kind| self.core_range_key(*kind).as_ref() == Some(&key))
    }

    pub(crate) fn core_range_inner<'a>(&self, ty: &'a Type) -> Option<&'a Type> {
        self.core_range_kind(ty)?;
        let Type::Nominal(nominal) = ty else {
            return None;
        };
        let [inner] = nominal.type_args.as_slice() else {
            return None;
        };
        Some(inner)
    }

    pub(crate) fn map_key_error(&self, ty: &Type) -> Option<MapKeyError> {
        self.map_key_error_inner(ty, &mut HashSet::new())
    }

    fn map_key_error_inner(
        &self,
        ty: &Type,
        seen: &mut HashSet<NominalKey>,
    ) -> Option<MapKeyError> {
        if self.core_option_inner(ty).is_some() {
            return Some(MapKeyError {
                ty: ty.clone(),
                field: None,
            });
        }

        match ty {
            Type::Int
            | Type::Bool
            | Type::String
            | Type::Infer
            | Type::InferReturn
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => None,
            Type::Tuple(elems) => elems
                .iter()
                .find_map(|elem| self.map_key_error_inner(elem, seen)),
            Type::Nominal(_) => self.nominal_map_key_error(ty, seen),
            Type::Any
            | Type::Float
            | Type::Void
            | Type::Func { .. }
            | Type::List { .. }
            | Type::Array { .. }
            | Type::Map { .. }
            | Type::Slice { .. } => Some(MapKeyError {
                ty: ty.clone(),
                field: None,
            }),
        }
    }

    fn nominal_map_key_error(
        &self,
        ty: &Type,
        seen: &mut HashSet<NominalKey>,
    ) -> Option<MapKeyError> {
        let key = self.key_for_type(ty)?;
        match key.kind {
            NominalKind::Enum => None,
            NominalKind::Extern => Some(MapKeyError {
                ty: ty.clone(),
                field: None,
            }),
            NominalKind::Struct | NominalKind::DataRef => {
                if !seen.insert(key.clone()) {
                    return None;
                }
                let agg = self.aggregate(&key)?;
                let mut fields = agg.fields.iter().collect::<Vec<_>>();
                fields.sort_by(|(left, _), (right, _)| left.as_str().cmp(right.as_str()));
                for (name, field) in fields {
                    let field_ty = substitute_aggregate_member(ty, &agg.generics, &field.ty);
                    if let Some(err) = self.map_key_error_inner(&field_ty, seen) {
                        seen.remove(&key);
                        return Some(MapKeyError {
                            ty: err.ty,
                            field: Some(*name),
                        });
                    }
                }
                seen.remove(&key);
                None
            }
        }
    }

    fn core_option_key(&self) -> Option<NominalKey> {
        self.core_enum_key("option", Type::OPTION_ENUM_NAME)
    }

    fn core_range_key(&self, kind: CoreRangeKind) -> Option<NominalKey> {
        let name = Ident::new(kind.name());
        self.local_nominal_type(&core_module_scope("range"), name)
            .filter(|key| key.kind == NominalKind::Struct && key.name == name)
    }

    fn core_enum_key(&self, module: &str, name: &str) -> Option<NominalKey> {
        let name = Ident::new(name);
        self.local_nominal_type(&core_module_scope(module), name)
            .filter(|key| key.kind == NominalKind::Enum && key.name == name)
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

    pub(crate) fn aggregate_field_policy(
        &self,
        receiver: &Type,
        name: Ident,
    ) -> Option<(ModuleScope, AccessPolicy)> {
        let key = self.key_for_type(receiver)?;
        let policy = self.aggregate(&key)?.fields.get(&name)?.policy.clone();
        Some((key.module, policy))
    }

    pub(crate) fn extends(&self) -> impl Iterator<Item = &ExtendSchema> {
        self.extends.iter()
    }

    pub(crate) fn find_cast_conversion(
        &self,
        source: &Type,
        target: &Type,
        visible: impl Fn(&ExtendSchema) -> bool,
    ) -> Option<CastConversionMatch> {
        let mut selected = None;
        for extend in self.extends.iter().filter(|extend| visible(extend)) {
            let target_template = generic_template_type(&extend.target, &extend.generics);
            for cast in &extend.cast_froms {
                let source_template = generic_template_type(&cast.source, &extend.generics);
                if match_cast_conversion(
                    &extend.generics,
                    &source_template,
                    source,
                    &target_template,
                    target,
                )
                .is_none()
                {
                    continue;
                }
                if selected.is_some() {
                    return Some(CastConversionMatch::Ambiguous);
                }
                selected = Some(());
            }
        }
        selected.map(|()| CastConversionMatch::Match)
    }

    fn module_surface_contains(&self, module: &ModuleScope, origin: &ModuleScope) -> bool {
        module == origin
            || self
                .modules
                .get(module)
                .is_some_and(|decls| decls.exported_active_modules.contains(origin))
    }

    pub(crate) fn module_surface_has_extend_method(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> bool {
        self.extends().any(|ext| {
            ext.methods.contains_key(&MethodKey::instance(name))
                && self.module_surface_contains(module, &ext.origin)
        })
    }

    pub(crate) fn find_extend_method_in_module_surface(
        &self,
        module: &ModuleScope,
        receiver: &Type,
        name: Ident,
    ) -> Option<ExtendMethodMatch<'_>> {
        self.find_instance_extend_method(receiver, name, |ext| {
            self.module_surface_contains(module, &ext.origin)
        })
    }

    pub(crate) fn find_instance_extend_method<F>(
        &self,
        receiver: &Type,
        name: Ident,
        visible: F,
    ) -> Option<ExtendMethodMatch<'_>>
    where
        F: FnMut(&ExtendSchema) -> bool,
    {
        self.find_extend_method(MethodSurface::Instance, receiver, name, visible)
    }

    pub(crate) fn find_static_extend_method<F>(
        &self,
        target: &Type,
        name: Ident,
        visible: F,
    ) -> Option<ExtendMethodMatch<'_>>
    where
        F: FnMut(&ExtendSchema) -> bool,
    {
        self.find_extend_method(MethodSurface::Static, target, name, visible)
    }

    fn find_extend_method<F>(
        &self,
        surface: MethodSurface,
        subject: &Type,
        name: Ident,
        mut visible: F,
    ) -> Option<ExtendMethodMatch<'_>>
    where
        F: FnMut(&ExtendSchema) -> bool,
    {
        let method_key = MethodKey::new(name, surface);
        let candidates = self
            .extends()
            .filter(|ext| visible(ext))
            .filter_map(|ext| extend_candidate(ext, method_key, subject))
            .collect::<Vec<_>>();

        select_extend_candidate(candidates)
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
            | CallableKind::ExtendMethod(_)
            | CallableKind::EnumVariant => return None,
        };

        Some(CallableRef {
            def: CallableDef {
                id,
                sig: CallableSig {
                    owner_generics: GenericParams::default(),
                    generics: sig.generics.clone(),
                    params: params.clone(),
                    required_params: sig.required_params,
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
        receiver_ty: Type,
    ) -> CallableRef {
        debug_assert_eq!(method.mode.surface(), MethodSurface::Instance);
        let owner_ty = receiver_ty.clone();
        self.callable_for_aggregate_member(
            aggregate,
            name,
            method,
            Some(&owner_ty),
            Some(receiver_ty),
        )
    }

    pub(crate) fn callable_for_aggregate_static_method(
        &self,
        aggregate: &AggregateSchema,
        name: Ident,
        method: &MethodSchema,
        owner_ty: Option<&Type>,
    ) -> CallableRef {
        debug_assert_eq!(method.mode.surface(), MethodSurface::Static);
        self.callable_for_aggregate_member(aggregate, name, method, owner_ty, None)
    }

    fn callable_for_aggregate_member(
        &self,
        aggregate: &AggregateSchema,
        name: Ident,
        method: &MethodSchema,
        owner_ty: Option<&Type>,
        receiver_ty: Option<Type>,
    ) -> CallableRef {
        debug_assert!(self.aggregates.contains_key(&aggregate.key));
        let subst = owner_ty.and_then(|ty| aggregate_substitutions(ty, &aggregate.generics));
        let params = subst.as_ref().map_or_else(
            || method.params.clone(),
            |(type_subst, const_subst)| {
                substitute_func_params(&method.params, type_subst, const_subst)
            },
        );
        let ret = subst.as_ref().map_or_else(
            || method.ret.clone(),
            |(type_subst, const_subst)| substitute(&method.ret, type_subst, const_subst),
        );

        CallableRef {
            def: CallableDef {
                id: CallableId::aggregate_method(
                    aggregate.key.clone(),
                    name,
                    method.mode.surface(),
                ),
                sig: CallableSig {
                    owner_generics: aggregate.generics.clone(),
                    generics: method.generics.clone(),
                    params,
                    required_params: method.required_params,
                    ret,
                },
            },
            receiver_ty,
            owner_args: owner_ty.and_then(nominal_generic_args).unwrap_or_default(),
        }
    }

    pub(crate) fn callable_for_extend_method(
        &self,
        receiver_ty: Type,
        extend: &ExtendSchema,
        name: Ident,
        method: &ExtendMethodSchema,
        owner_args: GenericArgs,
    ) -> CallableRef {
        debug_assert_eq!(method.mode.surface(), MethodSurface::Instance);
        self.callable_for_extension(Some(receiver_ty), extend, name, method, owner_args)
    }

    pub(crate) fn callable_for_static_extend_method(
        &self,
        extend: &ExtendSchema,
        name: Ident,
        method: &ExtendMethodSchema,
        owner_args: GenericArgs,
    ) -> CallableRef {
        debug_assert_eq!(method.mode.surface(), MethodSurface::Static);
        self.callable_for_extension(None, extend, name, method, owner_args)
    }

    fn callable_for_extension(
        &self,
        receiver_ty: Option<Type>,
        extend: &ExtendSchema,
        name: Ident,
        method: &ExtendMethodSchema,
        owner_args: GenericArgs,
    ) -> CallableRef {
        debug_assert!(self.extends.iter().any(|schema| schema.id == extend.id));
        let (type_subst, const_subst) = extend.generics.substitutions(&owner_args);
        let template_params = method
            .params
            .iter()
            .map(|param| {
                FuncParam::new(
                    generic_template_type(&param.ty, &extend.generics),
                    param.mutable,
                    param.cast_accept,
                )
            })
            .collect::<Vec<_>>();
        let template_ret = generic_template_type(&method.ret, &extend.generics);

        CallableRef {
            def: CallableDef {
                id: CallableId::extend_method(extend.id.clone(), name, method.mode.surface()),
                sig: CallableSig {
                    owner_generics: extend.generics.clone(),
                    generics: method.generics.clone(),
                    params: substitute_func_params(&template_params, &type_subst, &const_subst),
                    required_params: method.required_params,
                    ret: substitute(&template_ret, &type_subst, &const_subst),
                },
            },
            receiver_ty,
            owner_args,
        }
    }

    pub(crate) fn callable_for_variant(
        &self,
        enum_key: &NominalKey,
        variant: Ident,
        schema: &VariantSchema,
    ) -> Option<CallableRef> {
        let params = match &schema.payload {
            VariantPayload::Unit => vec![],
            VariantPayload::Tuple(types) => types.iter().cloned().map(FuncParam::immut).collect(),
            VariantPayload::Struct(_) => return None,
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
                    required_params: params.len(),
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
    let Some((type_subst, const_subst)) = aggregate_substitutions(receiver, generics) else {
        return ty.clone();
    };
    substitute(ty, &type_subst, &const_subst)
}

fn aggregate_substitutions(
    owner_ty: &Type,
    generics: &GenericParams,
) -> Option<(TypeSubst, ConstSubst)> {
    let owner = owner_ty.as_aggregate()?;
    let type_subst: TypeSubst = generics
        .type_params
        .iter()
        .zip(owner.type_args)
        .map(|(param, arg)| (param.id, arg.clone()))
        .collect();
    let const_subst: ConstSubst = generics
        .const_params
        .iter()
        .zip(owner.const_args)
        .map(|(param, arg)| (param.id, ConstTerm::from_arg(arg)))
        .collect();
    (!type_subst.is_empty() || !const_subst.is_empty()).then_some((type_subst, const_subst))
}

pub(crate) fn nominal_generic_args(ty: &Type) -> Option<GenericArgs> {
    let Type::Nominal(nominal) = ty else {
        return None;
    };
    Some(GenericArgs {
        type_args: nominal.type_args.clone(),
        const_args: nominal.const_args.iter().map(ConstTerm::from_arg).collect(),
    })
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
                param.cast_accept,
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum ExtendReceiverMatch {
    Exact,
    SliceView,
}

struct ExtendCandidate<'a> {
    extend: &'a ExtendSchema,
    method: &'a ExtendMethodSchema,
    target: Type,
    receiver_ty: Type,
    owner_args: Result<GenericArgs, Vec<Ident>>,
    receiver_match: ExtendReceiverMatch,
}

type GenericTemplateMatch = Result<GenericArgs, Vec<Ident>>;

fn extend_candidate<'a>(
    ext: &'a ExtendSchema,
    method_key: MethodKey,
    subject: &Type,
) -> Option<ExtendCandidate<'a>> {
    let method = ext.methods.get(&method_key)?;
    let target = generic_template_type(&ext.target, &ext.generics);
    let (receiver_ty, owner_args, receiver_match) =
        extend_receiver_match(&ext.generics, &target, subject, method_key.surface)?;
    Some(ExtendCandidate {
        extend: ext,
        method,
        target,
        receiver_ty,
        owner_args,
        receiver_match,
    })
}

fn extend_receiver_match(
    generics: &GenericParams,
    target: &Type,
    subject: &Type,
    surface: MethodSurface,
) -> Option<(Type, Result<GenericArgs, Vec<Ident>>, ExtendReceiverMatch)> {
    if surface == MethodSurface::Static && static_nominal_family_match(target, subject) {
        return Some((
            subject.clone(),
            Ok(GenericArgs::default()),
            ExtendReceiverMatch::Exact,
        ));
    }

    match match_generic_template_args(generics, target, subject) {
        Some(owner_args) => return Some((subject.clone(), owner_args, ExtendReceiverMatch::Exact)),
        None if surface != MethodSurface::Instance => return None,
        None => {}
    }
    let Type::Slice { elem: target_elem } = target else {
        return None;
    };
    let (Type::List { elem: subject_elem }
    | Type::Array {
        elem: subject_elem, ..
    }) = subject
    else {
        return None;
    };
    let receiver_ty = Type::Slice {
        elem: subject_elem.clone(),
    };
    let owner_args = match_generic_template_args(generics, target_elem, subject_elem)?;
    Some((receiver_ty, owner_args, ExtendReceiverMatch::SliceView))
}

fn static_nominal_family_match(target: &Type, subject: &Type) -> bool {
    let (Type::Nominal(target), Type::Nominal(subject)) = (target, subject) else {
        return false;
    };
    target.kind == subject.kind
        && target.name == subject.name
        && target.origin == subject.origin
        && subject.type_args.is_empty()
        && subject.const_args.is_empty()
}

fn select_extend_candidate(
    mut candidates: Vec<ExtendCandidate<'_>>,
) -> Option<ExtendMethodMatch<'_>> {
    if candidates
        .iter()
        .any(|candidate| candidate.receiver_match == ExtendReceiverMatch::Exact)
    {
        candidates.retain(|candidate| candidate.receiver_match == ExtendReceiverMatch::Exact);
    }

    match candidates.len() {
        0 => None,
        1 => {
            let candidate = candidates.pop().expect("one extend candidate");
            Some(ExtendMethodMatch::Match {
                extend: candidate.extend,
                method: candidate.method,
                receiver_ty: candidate.receiver_ty,
                owner_args: candidate.owner_args,
            })
        }
        _ => Some(most_specific_extend(candidates)),
    }
}

fn match_generic_template_args(
    generics: &GenericParams,
    template: &Type,
    concrete: &Type,
) -> Option<GenericTemplateMatch> {
    match_cast_conversion(generics, template, concrete, template, concrete)
}

fn match_cast_conversion(
    generics: &GenericParams,
    source_template: &Type,
    source: &Type,
    target_template: &Type,
    target: &Type,
) -> Option<GenericTemplateMatch> {
    let span = None;
    if generics.is_empty() {
        return (source_template == source && target_template == target)
            .then(|| Ok(GenericArgs::default()));
    }

    let mut solver = Solver::default();
    let seeds = GenericSolverSeeds::default();
    let vars = solver.generic_solver_vars(generics, &seeds, span);
    let source_template = solver.instantiate_generic_type(source_template, &vars);
    let target_template = solver.instantiate_generic_type(target_template, &vars);
    let source = solver.concrete_type(source);
    let target = solver.concrete_type(target);
    solver.add_handle_equal(span, source_template, source);
    solver.add_handle_equal(span, target_template, target);
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
        receiver_ty: candidate.receiver_ty,
        owner_args: candidate.owner_args,
    }
}

fn more_specific(a: &Type, b: &Type) -> bool {
    compare_specificity(a, b) == Specificity::MoreSpecific
}

impl DeclarationIndex {
    pub(crate) fn finalize_type_ref(
        &self,
        module: &ModuleScope,
        generics: &GenericTypeContext,
        ty: &Type,
    ) -> Result<Type, TypeRefError> {
        TypeRefResolver::module_only(self).finalize(module, generics, ty)
    }

    pub(crate) fn finalize_nominal_type_args(
        &self,
        module: &ModuleScope,
        key: &NominalKey,
        args: Vec<Type>,
    ) -> Result<Type, TypeRefError> {
        let args = args.into_iter().map(GenericArg::Type).collect::<Vec<_>>();
        TypeRefResolver::module_only(self).finalize_nominal_args(module, key, &args)
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
        self.generics
            .type_param(name)
            .map_or(Type::UnresolvedName(name), Type::Var)
    }

    fn fold_unresolved_nominal(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        if qualifier.is_none()
            && generic_args.is_empty()
            && let Some(id) = self.generics.type_param(name)
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

pub(crate) fn same_extend_target(
    a: &Type,
    a_generics: &GenericParams,
    b: &Type,
    b_generics: &GenericParams,
) -> bool {
    let Some(a_template) = try_generic_template_type(a, a_generics) else {
        return false;
    };
    let Some(b_template) = try_generic_template_type(b, b_generics) else {
        return false;
    };
    match_generic_template_args(a_generics, &a_template, &b_template).is_some()
        && match_generic_template_args(b_generics, &b_template, &a_template).is_some()
}

fn try_generic_template_type(ty: &Type, generics: &GenericParams) -> Option<Type> {
    let generics =
        GenericTypeContext::try_from_params(&generics.type_params, &generics.const_params).ok()?;
    Some(GenericTemplate { generics }.fold_type(ty))
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

pub(crate) fn generic_params(
    type_params: &[TypeParam],
    const_params: &[ConstParam],
) -> GenericParams {
    GenericParams {
        type_params: type_params.to_vec(),
        const_params: const_params.to_vec(),
    }
}

pub(crate) fn func_type_from_params(params: &[Param], ret: &Type) -> Type {
    let resolved_params = params
        .iter()
        .map(|p| {
            FuncParam::new(
                p.ty.clone(),
                matches!(p.mutability, Mutability::Mutable),
                p.cast_accept,
            )
        })
        .collect();
    Type::Func {
        params: resolved_params,
        ret: Box::new(ret.clone()),
    }
}

pub(crate) fn resolve_func_params(params: &[Param]) -> Vec<FuncParam> {
    params
        .iter()
        .map(|p| {
            FuncParam::new(
                p.ty.clone(),
                matches!(p.mutability, Mutability::Mutable),
                p.cast_accept,
            )
        })
        .collect()
}

pub(crate) fn required_param_count(params: &[Param]) -> usize {
    params
        .iter()
        .position(|param| param.default.is_some())
        .unwrap_or(params.len())
}

fn stmt_visibility(stmt: &StmtNode) -> Visibility {
    match &stmt.node {
        Stmt::ExternFunc(n) => n.node.visibility,
        Stmt::ExternType(n) => n.node.visibility,
        Stmt::Extend(n) => n.node.visibility,
        Stmt::Func(n) => n.node.visibility,
        Stmt::Aggregate(n) => n.node.visibility,
        Stmt::Enum(n) => n.node.visibility,
        Stmt::Const(n) => n.node.visibility,
        Stmt::TypeAlias(n) => n.node.visibility,
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
        test_support::{parse_program, resolved_modules, resolved_modules_with_external},
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

    fn provider_scope(name: &str) -> ModuleScope {
        ModuleScope::from_module_id(&ModuleId::provider(
            PackageId::synthetic_root(),
            ModulePath::new(vec![name.to_string()]).unwrap(),
        ))
    }

    fn index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse(root);
        let resolved = resolved_modules(&root, modules);
        let externs = crate::externs::collect_source_externs(&root, &resolved).unwrap();
        DeclarationIndex::from_root_and_modules(&root, &resolved, &externs)
    }

    fn checked_index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse(root);
        let resolved = resolved_modules(&root, modules);
        let externs = crate::externs::collect_source_externs(&root, &resolved).unwrap();
        let mut tc = super::super::typechecker_for_modules(
            &root,
            &resolved,
            externs,
            super::super::TypecheckConfig::default(),
        )
        .expect("typecheck failed");
        tc.finish().expect("typecheck failed");
        tc.decls.clone()
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
        let raw = crate::externs::ingest_providers(crate::externs::ExternInputs {
            packages: vec![crate::externs::PackageExternInputs {
                package: PackageId::synthetic_root(),
                providers: vec![provider],
            }],
        })
        .unwrap();
        let external_modules = crate::externs::raw_extern_module_ids(&raw);
        let resolved = resolved_modules_with_external(&root, modules, &external_modules);
        DeclarationIndex::from_root_and_modules(&root, &resolved, &raw)
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

    fn assert_deprecated_reason(policy: &AccessPolicy, reason: &str) {
        assert_eq!(policy.deprecated_reason(), Some(reason));
    }

    #[test]
    fn function_policy() {
        let index = index("@deprecated(\"use newer\") fn old() {}", &[]);
        let value = index
            .local_value(&ModuleScope::Root, ident("old"))
            .expect("missing function");
        let ValueDecl::Func(sig) = &value.decl else {
            panic!("expected function");
        };

        assert_deprecated_reason(&sig.policy, "use newer");
    }

    #[test]
    fn imported_function_policy() {
        let index = index(
            "import reexport { old };",
            &[
                ("lib", "@deprecated(\"use fresh\") pub fn old() {}"),
                ("reexport", "pub import lib { old };"),
            ],
        );
        let value = index
            .imported_value(&ModuleScope::Root, ident("old"))
            .expect("missing imported function");
        let ValueDecl::Func(sig) = &value.decl else {
            panic!("expected function");
        };

        assert_deprecated_reason(&sig.policy, "use fresh");
    }

    #[test]
    fn source_extern_function_policy() {
        let index = index(
            "@deprecated(\"use host_new\") extern fn host_old() -> int;",
            &[],
        );
        let value = index
            .local_value(&ModuleScope::Root, ident("host_old"))
            .expect("missing extern function");
        let ValueDecl::Func(sig) = &value.decl else {
            panic!("expected function");
        };

        assert_eq!(sig.kind, CallableKind::ExternFunction);
        assert_deprecated_reason(&sig.policy, "use host_new");
    }

    #[test]
    fn aggregate_policy() {
        let index = index(
            "@deprecated(\"use NewPoint\") struct OldPoint { x: int }",
            &[],
        );
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident("OldPoint"))
            .expect("missing aggregate");
        let agg = index.aggregate(&key).expect("missing aggregate schema");

        assert_deprecated_reason(&agg.policy, "use NewPoint");
    }

    #[test]
    fn enum_and_variant_policy() {
        let index = index(
            "@deprecated(\"use NewStatus\") enum Status { Active, @deprecated(\"use Active\") Disabled }",
            &[],
        );
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident("Status"))
            .expect("missing enum");
        let enm = index.enum_schema(&key).expect("missing enum schema");
        let variant = enm
            .variants
            .get(&ident("Disabled"))
            .expect("missing variant");

        assert_deprecated_reason(&enm.policy, "use NewStatus");
        assert_deprecated_reason(&variant.policy, "use Active");
    }

    #[test]
    fn enum_variant_field_policy() {
        let index = index(
            "enum Event { Move { @deprecated(\"use x\") old_x: int, x: int } }",
            &[],
        );
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident("Event"))
            .expect("missing enum");
        let enm = index.enum_schema(&key).expect("missing enum schema");
        let variant = enm.variants.get(&ident("Move")).expect("missing variant");
        let VariantPayload::Struct(fields) = &variant.payload else {
            panic!("expected struct variant");
        };
        let field = fields.get(&ident("old_x")).expect("missing field");

        assert_deprecated_reason(&field.policy, "use x");
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
            .exported_nominal_type(&scope("facade"), ident("P"))
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
                .imported_type_binding(&ModuleScope::Root, ident("Point"))
                .is_none()
        );
        assert!(
            index
                .visible_type_binding(&ModuleScope::Root, ident("Point"))
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
            .exported_nominal_type(&scope("shapes"), ident("Point"))
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
            .resolve_visible_nominal_key(&ModuleScope::Root, Some(ident("shapes")), ident("Point"))
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
                .resolve_visible_nominal_key(
                    &ModuleScope::Root,
                    Some(ident("shapes")),
                    ident("Point")
                )
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
            "import ext:host { Handle, load }; import ext:host { * };",
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![extern_fn("load")],
            }),
        );
        let host = provider_scope("host");
        let ty = index
            .imported_type_binding(&ModuleScope::Root, ident("Handle"))
            .and_then(TypeBinding::into_nominal)
            .expect("missing provider type import");
        let value = index
            .imported_value(&ModuleScope::Root, ident("load"))
            .expect("missing provider function import");

        assert_eq!(ty.module, host);
        assert_eq!(ty.kind, NominalKind::Extern);
        assert_eq!(value.module, provider_scope("host"));
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
            "import ext:host;",
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![],
            }),
        );

        assert_eq!(
            index.imported_module(&ModuleScope::Root, ident("host")),
            Some(provider_scope("host"))
        );
    }

    #[test]
    fn provider_members_are_not_module_values() {
        let index = provider_index(
            "import ext:host { * };",
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![anvyx_externs::ExternFieldDescriptor {
                        name: "x".to_string(),
                        ty: ExternTypeExpr::Int,
                        computed: false,
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
    fn provider_externs_do_not_conflict_with_same_path_source_decls() {
        let index = provider_index_with_modules(
            "",
            &[(
                "host",
                "pub struct Handle { id: int } pub fn load() -> float { 0.0 }",
            )],
            provider_with_module(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![extern_fn("load")],
            }),
        );

        assert!(index.errors().is_empty());
        assert!(
            index
                .local_nominal_type(&scope("host"), ident("Handle"))
                .is_some()
        );
        assert!(
            index
                .local_nominal_type(&provider_scope("host"), ident("Handle"))
                .is_some()
        );
        assert!(index.local_value(&scope("host"), ident("load")).is_some());
        assert!(
            index
                .local_value(&provider_scope("host"), ident("load"))
                .is_some()
        );
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
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident("Box"))
            .unwrap();
        let aggregate = index.aggregate(&key).unwrap();
        let receiver = Type::nominal(
            NominalKind::Struct,
            ident("Box"),
            vec![Type::Int],
            vec![],
            None,
        );
        let static_method = aggregate
            .methods
            .get(&MethodKey::static_(ident("make")))
            .unwrap();
        let instance_method = aggregate
            .methods
            .get(&MethodKey::instance(ident("get")))
            .unwrap();

        let static_ref = index.callable_for_aggregate_static_method(
            aggregate,
            ident("make"),
            static_method,
            None,
        );
        let instance_ref = index.callable_for_aggregate_method(
            aggregate,
            ident("get"),
            instance_method,
            receiver.clone(),
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
            receiver_ty: _,
            owner_args: Ok(owner_args),
        } = index
            .find_instance_extend_method(&Type::Int, ident("id"), |_| true)
            .unwrap()
        else {
            panic!("expected extend match");
        };
        let callable = index.callable_for_extend_method(
            Type::Int,
            extend,
            ident("id"),
            method,
            owner_args.clone(),
        );

        assert_eq!(
            callable.def.id.kind,
            CallableKind::ExtendMethod(MethodSurface::Instance)
        );
        assert_eq!(callable.owner_args, owner_args);
        assert_eq!(callable.receiver_ty, Some(Type::Int));
        assert_eq!(callable.def.sig.params, vec![FuncParam::immut(Type::Int)]);
        assert_eq!(callable.def.sig.ret, Type::Int);
    }

    #[test]
    fn enum_variant_callables() {
        let index = index("enum E<T> { A, B(T), C { x: T } }", &[]);
        let key = index
            .local_nominal_type(&ModuleScope::Root, ident("E"))
            .unwrap();
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

        assert!(matches!(
            exported.decl,
            ValueDecl::Const(ConstSig { ty: Type::Int, .. })
        ));
        assert!(matches!(
            imported.decl,
            ValueDecl::Const(ConstSig { ty: Type::Int, .. })
        ));
    }
}
