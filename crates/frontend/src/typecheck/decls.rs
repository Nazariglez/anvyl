use std::{
    collections::{HashMap, HashSet, hash_map::Entry},
    fmt,
};

use super::{
    CastFromInstanceKey, ConstSubst, DeprecatedUseKind, GenericArgs, GenericParams, TypeSubst,
    annotation,
    const_term::ConstTerm,
    extend_target::{
        ExtendTargetPattern, MatchedTargetPattern, match_exact_target, most_specific_target_match,
    },
    generic_template_type, match_cast_conversion, substitute,
    surface::{dependent_embed_template_valid, projection_path_valid},
    type_ops::type_depends_on_generics,
    type_refs::{
        GenericParamError, GenericTypeContext, TypeRefError, TypeRefResolver,
        extend_generic_context_with_params, generic_context_from_params,
    },
};
use crate::{
    ast::{
        self, AggregateKind, ConstArg, ConstParam, ContractRef, EscapeMode, ExtendTargetConstraint,
        FuncParam, GenericArg, Ident, ImportItemKind, ImportKind, MethodReceiver, MethodSig,
        ModuleOrigin, Mutability, NominalKind, Param, Program, ReturnSpec, Stmt, StmtNode, Type,
        TypeParam, VariantKind, Visibility,
    },
    externs::{
        ExternProvenance, RawExternModule, RawExterns, catalog::ExternCatalog, raw_module_scope,
    },
    resolve::{ModuleId, ModulePath, PackageId, PackageModulePath, ResolveResult, SourceFileId},
    source::SourceId,
    span::{SourceSpan, Span, Spanned},
    typecheck::{CanonicalTypeKey, Exposure, PromotedSurface, annotation::AccessPolicy},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ModuleScope {
    Root,
    Named(ModulePath),
    Package(ModuleId),
}

impl fmt::Display for ModuleScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Root => f.write_str("<root>"),
            Self::Named(path) => write!(f, "{path}"),
            Self::Package(module) => write!(f, "{module}"),
        }
    }
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

    pub(crate) fn nominal_origin(&self) -> Option<ModuleOrigin> {
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

    pub(crate) fn is_core_module(&self, segment: &str) -> bool {
        matches!(self, Self::Package(module)
            if module.package_context() == Some(&PackageId::core())
                && matches!(module.path(), PackageModulePath::Named(path)
                    if path.segments().len() == 1 && path.segments()[0] == segment))
    }

    pub(crate) fn from_nominal_origin(origin: &ModuleOrigin) -> Self {
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
pub(crate) struct ContractKey {
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct GlobalKey {
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ContractSetKey {
    pub(crate) requirements: Vec<ContractRequirementKey>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ContractRequirementKey {
    pub(crate) name: Ident,
    pub(crate) receiver: Option<MethodReceiver>,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) required_params: usize,
    pub(crate) ret: ReturnSpec,
}

impl ContractRequirementKey {
    pub(crate) fn from_schema(req: &ContractRequirementSchema) -> Self {
        Self {
            name: req.name,
            receiver: req.receiver,
            params: req.params.clone(),
            required_params: req.required_params,
            ret: req.ret.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TypeBinding {
    Nominal(NominalKey),
    Alias(TypeAliasKey),
    Contract(ContractKey),
}

impl TypeBinding {
    pub(crate) fn module(&self) -> &ModuleScope {
        match self {
            Self::Nominal(key) => &key.module,
            Self::Alias(key) => &key.module,
            Self::Contract(key) => &key.module,
        }
    }

    pub(crate) fn as_nominal(&self) -> Option<&NominalKey> {
        match self {
            Self::Nominal(key) => Some(key),
            Self::Alias(_) | Self::Contract(_) => None,
        }
    }

    pub(crate) fn into_nominal(self) -> Option<NominalKey> {
        match self {
            Self::Nominal(key) => Some(key),
            Self::Alias(_) | Self::Contract(_) => None,
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

impl CallableKind {
    pub(crate) fn has_receiver_param(self) -> bool {
        matches!(
            self,
            Self::InstanceMethod | Self::ExtendMethod(MethodSurface::Instance)
        )
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ImportId {
    pub(crate) module: ModuleScope,
    pub(crate) ordinal: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ImportRecord {
    pub(crate) id: ImportId,
    pub(crate) visibility: Visibility,
    pub(crate) span: SourceSpan,
    pub(crate) target_module: ModuleScope,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BindingOrigin {
    Local,
    Import {
        source: ModuleScope,
        import: ImportId,
    },
    Reexport {
        source: ModuleScope,
        import: ImportId,
    },
    ImplicitImport {
        source: ModuleScope,
    },
}

impl BindingOrigin {
    fn import_id(&self) -> Option<&ImportId> {
        match self {
            Self::Import { import, .. } | Self::Reexport { import, .. } => Some(import),
            Self::Local | Self::ImplicitImport { .. } => None,
        }
    }
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
    DuplicateAggregateField {
        owner: NominalKey,
        name: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateEnumVariant {
        owner: NominalKey,
        name: Ident,
        span: Option<SourceSpan>,
    },
    RawEnumInvalidBacking {
        owner: NominalKey,
        span: Option<SourceSpan>,
    },
    RawEnumGenericParams {
        owner: NominalKey,
        span: Option<SourceSpan>,
    },
    RawEnumValueWithoutBacking {
        owner: NominalKey,
        variant: Ident,
        span: Option<SourceSpan>,
    },
    RawEnumPayloadVariant {
        owner: NominalKey,
        variant: Ident,
        span: Option<SourceSpan>,
    },
    RawEnumMissingStringValue {
        owner: NominalKey,
        variant: Ident,
        span: Option<SourceSpan>,
    },
    RawEnumDuplicateValue {
        owner: NominalKey,
        variant: Ident,
        value: RawEnumValue,
        span: Option<SourceSpan>,
    },
    RawEnumIntOverflow {
        owner: NominalKey,
        variant: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateVariantField {
        owner: NominalKey,
        variant: Ident,
        name: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateContractRequirement {
        contract: ContractKey,
        name: Ident,
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
    AsProjectionWithoutEmbed {
        span: Option<SourceSpan>,
    },
    AsProjectionWithArgs {
        span: Option<SourceSpan>,
    },
    InternalOnToString {
        span: Option<SourceSpan>,
    },
    InvalidToStringMethod {
        message: &'static str,
        span: Option<SourceSpan>,
    },
    EmptyEmbedSelector {
        span: Option<SourceSpan>,
    },
    DuplicateEmbedSelector {
        name: Ident,
        kind: ast::EmbedSelectorKind,
        span: Option<SourceSpan>,
    },
    EmbedSurfaceCycle {
        owner: NominalKey,
        target: NominalKey,
        span: Option<SourceSpan>,
    },
    UnknownEmbedFieldSelector {
        name: Ident,
        span: Option<SourceSpan>,
    },
    EmbedFieldSelectorNamesMethod {
        name: Ident,
        span: Option<SourceSpan>,
    },
    AmbiguousEmbedFieldSelector {
        name: Ident,
        span: Option<SourceSpan>,
    },
    EmbedFieldConflictsWithDirect {
        owner: NominalKey,
        name: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateExplicitEmbedField {
        owner: NominalKey,
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnknownEmbedMethodSelector {
        name: Ident,
        span: Option<SourceSpan>,
    },
    EmbedMethodSelectorNamesField {
        name: Ident,
        span: Option<SourceSpan>,
    },
    EmbedMethodSelectorNamesStatic {
        name: Ident,
        span: Option<SourceSpan>,
    },
    EmbedMethodSelectorNamesToString {
        span: Option<SourceSpan>,
    },
    AmbiguousEmbedMethodSelector {
        name: Ident,
        span: Option<SourceSpan>,
    },
    EmbedMethodConflictsWithDirect {
        owner: NominalKey,
        name: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateExplicitEmbedMethod {
        owner: NominalKey,
        name: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateProjectionTarget {
        owner: NominalKey,
        target: Type,
        span: Option<SourceSpan>,
    },
}
impl DeclError {
    pub(crate) fn span(&self) -> Option<SourceSpan> {
        match self {
            DeclError::DuplicateValue { span, .. }
            | DeclError::DuplicateType { span, .. }
            | DeclError::MissingImportMember { span, .. }
            | DeclError::PrivateImportMember { span, .. }
            | DeclError::ImportConflict { span, .. }
            | DeclError::DuplicateModuleBinding { span, .. }
            | DeclError::DuplicateGenericParam { span, .. }
            | DeclError::DuplicateAggregateMethod { span, .. }
            | DeclError::DuplicateAggregateField { span, .. }
            | DeclError::DuplicateEnumVariant { span, .. }
            | DeclError::RawEnumInvalidBacking { span, .. }
            | DeclError::RawEnumGenericParams { span, .. }
            | DeclError::RawEnumValueWithoutBacking { span, .. }
            | DeclError::RawEnumPayloadVariant { span, .. }
            | DeclError::RawEnumMissingStringValue { span, .. }
            | DeclError::RawEnumDuplicateValue { span, .. }
            | DeclError::RawEnumIntOverflow { span, .. }
            | DeclError::DuplicateVariantField { span, .. }
            | DeclError::DuplicateContractRequirement { span, .. }
            | DeclError::DuplicateExtendMethod { span, .. }
            | DeclError::DuplicateCastFrom { span, .. }
            | DeclError::PointlessCastFrom { span, .. }
            | DeclError::CastFromReturnMismatch { span, .. }
            | DeclError::UnsupportedExtendTarget { span, .. }
            | DeclError::UnusedExtendTypeParam { span, .. }
            | DeclError::UnusedExtendConstParam { span, .. }
            | DeclError::UnusedAliasTypeParam { span, .. }
            | DeclError::UnusedAliasConstParam { span, .. }
            | DeclError::ExtendMethodConflict { span, .. }
            | DeclError::ReexportConflict { span, .. }
            | DeclError::UnknownType { span, .. }
            | DeclError::UnknownAnnotation { span, .. }
            | DeclError::InvalidAnnotationTarget { span, .. }
            | DeclError::DuplicateAnnotation { span, .. }
            | DeclError::InvalidAnnotationArgs { span, .. }
            | DeclError::AsProjectionWithoutEmbed { span }
            | DeclError::AsProjectionWithArgs { span }
            | DeclError::InternalOnToString { span }
            | DeclError::InvalidToStringMethod { span, .. }
            | DeclError::EmptyEmbedSelector { span }
            | DeclError::DuplicateEmbedSelector { span, .. }
            | DeclError::EmbedSurfaceCycle { span, .. }
            | DeclError::UnknownEmbedFieldSelector { span, .. }
            | DeclError::EmbedFieldSelectorNamesMethod { span, .. }
            | DeclError::AmbiguousEmbedFieldSelector { span, .. }
            | DeclError::EmbedFieldConflictsWithDirect { span, .. }
            | DeclError::DuplicateExplicitEmbedField { span, .. }
            | DeclError::UnknownEmbedMethodSelector { span, .. }
            | DeclError::EmbedMethodSelectorNamesField { span, .. }
            | DeclError::EmbedMethodSelectorNamesStatic { span, .. }
            | DeclError::EmbedMethodSelectorNamesToString { span }
            | DeclError::AmbiguousEmbedMethodSelector { span, .. }
            | DeclError::EmbedMethodConflictsWithDirect { span, .. }
            | DeclError::DuplicateExplicitEmbedMethod { span, .. }
            | DeclError::DuplicateProjectionTarget { span, .. } => *span,
        }
    }
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
    pending_raw_enums: HashMap<NominalKey, PendingRawEnum>,
    extends: Vec<ExtendSchema>,
    type_aliases: HashMap<TypeAliasKey, TypeAliasSchema>,
    contracts: HashMap<ContractKey, ContractSchema>,
    extern_type_policies: HashMap<NominalKey, AccessPolicy>,
    value_spans: HashMap<(ModuleScope, Ident), Span>,
    type_spans: HashMap<NominalKey, Span>,
    errors: Vec<DeclError>,
    import_records: Vec<ImportRecord>,
    used_imports: HashSet<ImportId>,
}

#[derive(Clone)]
pub(super) struct PendingRawEnum {
    pub(super) source: SourceId,
    pub(super) backing: Option<Spanned<Type>>,
    pub(super) variants: Vec<PendingRawVariant>,
}

#[derive(Clone)]
pub(super) struct PendingRawVariant {
    pub(super) name: Ident,
    pub(super) span: Span,
    pub(super) value: Option<ast::ExprNode>,
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

#[derive(Clone, Copy)]
enum NamespaceMember<'a> {
    Value(&'a ResolvedValue),
    Type(&'a TypeBinding),
    Module(&'a ModuleScope),
}

pub(crate) type ModuleExports = Namespace;

pub(crate) enum ModuleMemberLookup<T> {
    Found(T),
    Private,
    Missing,
}

pub(crate) struct TypeMemberLookup {
    pub(crate) result: ModuleMemberLookup<TypeBinding>,
    pub(crate) import: Option<ImportId>,
    pub(crate) target: Option<ModuleScope>,
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
    active_module_imports: HashMap<ModuleScope, Vec<ImportId>>,
    origins: HashMap<OriginKey, BindingOrigin>,
    binding_origins: HashMap<OriginKey, ImportId>,
    private_reexports: HashSet<Ident>,
    errors: Vec<DeclError>,
    mode: ImportMode,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct ImportScope {
    pub(crate) namespace: Namespace,
    pub(crate) active_modules: HashSet<ModuleScope>,
    pub(crate) active_module_imports: HashMap<ModuleScope, Vec<ImportId>>,
    pub(crate) binding_origins: HashMap<OriginKey, ImportId>,
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
    private_reexports: HashSet<Ident>,
    imports: ImportScope,
}

#[derive(Debug, Clone)]
pub(crate) enum ValueDecl {
    Func(FuncSig),
    Const(ConstSig),
    Global(GlobalSig),
}

impl NamespaceMember<'_> {
    fn namespace(&self) -> BindingNamespace {
        match self {
            Self::Value(_) => BindingNamespace::Value,
            Self::Type(_) => BindingNamespace::Type,
            Self::Module(_) => BindingNamespace::Module,
        }
    }

    fn module(&self) -> ModuleScope {
        match self {
            Self::Value(value) => value.module.clone(),
            Self::Type(binding) => binding.module().clone(),
            Self::Module(module) => (*module).clone(),
        }
    }
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

    fn for_each_member(&self, mut f: impl FnMut(Ident, NamespaceMember<'_>)) {
        for (name, value) in &self.values {
            f(*name, NamespaceMember::Value(value));
        }
        for (name, binding) in &self.types {
            f(*name, NamespaceMember::Type(binding));
        }
        for (name, module) in &self.modules {
            f(*name, NamespaceMember::Module(module));
        }
    }

    fn named_members(&self, name: Ident) -> Vec<NamespaceMember<'_>> {
        let mut members = vec![];
        if let Some(value) = self.values.get(&name) {
            members.push(NamespaceMember::Value(value));
        }
        if let Some(binding) = self.types.get(&name) {
            members.push(NamespaceMember::Type(binding));
        }
        if let Some(module) = self.modules.get(&name) {
            members.push(NamespaceMember::Module(module));
        }
        members
    }

    fn insert_value(&mut self, visible: Ident, value: ResolvedValue) {
        self.values.insert(visible, value);
    }

    fn insert_type(&mut self, visible: Ident, binding: TypeBinding) {
        self.types.insert(visible, binding);
    }

    fn insert_member(&mut self, visible: Ident, member: NamespaceMember<'_>) {
        match member {
            NamespaceMember::Value(value) => {
                self.values.insert(visible, value.clone());
            }
            NamespaceMember::Type(binding) => {
                self.types.insert(visible, binding.clone());
            }
            NamespaceMember::Module(module) => {
                self.modules.insert(visible, module.clone());
            }
        }
    }
}

impl ImportScopeBuilder {
    fn new(module: ModuleScope, mode: ImportMode) -> Self {
        Self {
            module,
            namespace: Namespace::default(),
            active_modules: HashSet::new(),
            active_module_imports: HashMap::new(),
            origins: HashMap::new(),
            binding_origins: HashMap::new(),
            private_reexports: HashSet::new(),
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
        namespace.for_each_member(|name, member| {
            self.origins
                .insert((member.namespace(), name), origin.clone());
        });
    }

    fn origin(&self, source: ModuleScope, import: ImportId) -> BindingOrigin {
        match self.mode {
            ImportMode::Import => BindingOrigin::Import { source, import },
            ImportMode::Reexport => BindingOrigin::Reexport { source, import },
        }
    }

    fn implicit_origin(source: ModuleScope) -> BindingOrigin {
        BindingOrigin::ImplicitImport { source }
    }

    fn apply_import(
        &mut self,
        kind: &ImportKind,
        source: &ModuleScope,
        default_name: Ident,
        dep: Option<&ModuleDecls>,
        span: Option<SourceSpan>,
        validate_members: bool,
        import_id: ImportId,
    ) {
        match kind {
            ImportKind::Module => {
                self.insert_member(
                    default_name,
                    NamespaceMember::Module(source),
                    self.origin(source.clone(), import_id),
                    span,
                );
            }
            ImportKind::ModuleAs(alias) => {
                self.insert_member(
                    *alias,
                    NamespaceMember::Module(source),
                    self.origin(source.clone(), import_id),
                    span,
                );
            }
            ImportKind::Selective(items) => {
                for item in items {
                    let target = item.alias.unwrap_or(match item.kind {
                        ImportItemKind::Name(name) => name,
                        ImportItemKind::SelfModule => default_name,
                    });
                    match item.kind {
                        ImportItemKind::SelfModule => {
                            self.insert_member(
                                target,
                                NamespaceMember::Module(source),
                                self.origin(source.clone(), import_id.clone()),
                                span,
                            );
                        }
                        ImportItemKind::Name(name) => {
                            if let Some(dep) = dep {
                                self.copy_named_members(
                                    dep,
                                    source,
                                    name,
                                    target,
                                    span,
                                    validate_members,
                                    import_id.clone(),
                                );
                            }
                        }
                    }
                }
            }
            ImportKind::Wildcard => {
                if let Some(dep) = dep {
                    self.copy_wildcard_members(dep, span, &self.origin(source.clone(), import_id));
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
        import_id: ImportId,
    ) {
        let origin = self.origin(origin_module.clone(), import_id);
        let members = dep.exports.named_members(source_name);
        let found = !members.is_empty();
        for member in members {
            self.insert_member(target_name, member, origin.clone(), span);
        }
        if found || !validate_members {
            return;
        }
        if dep.private_reexports.contains(&source_name) {
            if self.mode == ImportMode::Reexport {
                self.private_reexports.insert(target_name);
            }
            return;
        }
        if dep.locals.contains_member(source_name)
            || dep.imports.namespace.contains_member(source_name)
        {
            if self.mode == ImportMode::Reexport {
                self.private_reexports.insert(target_name);
            }
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
        dep: &ModuleDecls,
        span: Option<SourceSpan>,
        origin: &BindingOrigin,
    ) {
        dep.exports.for_each_member(|name, member| {
            self.insert_member(name, member, origin.clone(), span);
        });
        if self.mode == ImportMode::Reexport {
            self.private_reexports
                .extend(dep.private_reexports.iter().copied());
        }
    }

    fn insert_member(
        &mut self,
        name: Ident,
        member: NamespaceMember<'_>,
        origin: BindingOrigin,
        span: Option<SourceSpan>,
    ) -> bool {
        if !self.claim_origin(member.namespace(), name, origin, span) {
            return false;
        }
        self.namespace.insert_member(name, member);
        true
    }

    fn record_activation(&mut self, module: ModuleScope, import: ImportId) {
        self.active_modules.insert(module.clone());
        let imports = self.active_module_imports.entry(module).or_default();
        if !imports.contains(&import) {
            imports.push(import);
        }
    }

    fn claim_origin(
        &mut self,
        namespace: BindingNamespace,
        name: Ident,
        second: BindingOrigin,
        span: Option<SourceSpan>,
    ) -> bool {
        let Some(first) = self.origins.get(&(namespace, name)).cloned() else {
            if let Some(import) = second.import_id() {
                self.binding_origins
                    .insert((namespace, name), import.clone());
            }
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
                active_module_imports: self.active_module_imports,
                binding_origins: self.binding_origins,
            },
            self.errors,
        )
    }

    fn finish_reexport_scope(
        self,
    ) -> (
        Namespace,
        HashSet<ModuleScope>,
        HashSet<Ident>,
        Vec<DeclError>,
    ) {
        (
            self.namespace,
            self.active_modules,
            self.private_reexports,
            self.errors,
        )
    }
}

impl ImportScope {
    fn binding_origin(&self, namespace: BindingNamespace, name: Ident) -> Option<ImportId> {
        self.binding_origins.get(&(namespace, name)).cloned()
    }

    fn activate_imported_origins(&mut self) {
        let mut origins = vec![];
        self.namespace.for_each_member(|name, member| {
            origins.push((member.module(), (member.namespace(), name)));
        });
        for (module, key) in origins {
            self.activate_imported_origin(module, &key);
        }
    }

    fn activate_imported_origin(&mut self, module: ModuleScope, key: &OriginKey) {
        self.active_modules.insert(module.clone());
        let Some(import) = self.binding_origins.get(key).cloned() else {
            return;
        };
        let imports = self.active_module_imports.entry(module).or_default();
        if !imports.contains(&import) {
            imports.push(import);
        }
    }
}

impl ValueDecl {
    pub(crate) fn ty(&self) -> &Type {
        match self {
            Self::Func(sig) => &sig.ty,
            Self::Const(sig) => &sig.ty,
            Self::Global(sig) => &sig.ty,
        }
    }

    pub(crate) fn policy(&self) -> &AccessPolicy {
        match self {
            Self::Func(sig) => &sig.policy,
            Self::Const(sig) => &sig.policy,
            Self::Global(sig) => &sig.policy,
        }
    }

    pub(crate) fn deprecated_kind(&self) -> Option<DeprecatedUseKind> {
        match self {
            Self::Func(sig) => match sig.kind {
                CallableKind::Function => Some(DeprecatedUseKind::Function),
                CallableKind::ExternFunction => Some(DeprecatedUseKind::ExternFunction),
                CallableKind::StaticMethod
                | CallableKind::InstanceMethod
                | CallableKind::ExtendMethod(_)
                | CallableKind::EnumVariant => None,
            },
            Self::Const(_) => Some(DeprecatedUseKind::Const),
            Self::Global(_) => Some(DeprecatedUseKind::Global),
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ParamTypeSpans(Vec<Span>);

impl ParamTypeSpans {
    fn from_params(params: &[Param]) -> Self {
        Self(params.iter().map(|param| param.ty_span).collect())
    }

    pub(crate) fn span_for(&self, index: usize, fallback: Span) -> Span {
        self.0.get(index).copied().unwrap_or(fallback)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FuncSig {
    pub(crate) kind: CallableKind,
    pub(crate) generics: GenericParams,
    pub(crate) ty: Type,
    pub(crate) param_spans: ParamTypeSpans,
    pub(crate) default_sites: Vec<Option<ParamDefaultSite>>,
    pub(crate) required_params: usize,
    pub(crate) policy: AccessPolicy,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParamDefaultSite {
    pub(crate) expr_id: ast::ExprId,
    pub(crate) source: SourceId,
}

impl ParamDefaultSite {
    pub(crate) fn from_params(source: SourceId, params: &[Param]) -> Vec<Option<Self>> {
        params
            .iter()
            .map(|param| {
                param.default.as_ref().map(|default| Self {
                    expr_id: default.node.id,
                    source,
                })
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ConstSig {
    pub(crate) ty: Type,
    pub(crate) policy: AccessPolicy,
}

#[derive(Debug, Clone)]
pub(crate) struct GlobalSig {
    pub(crate) key: GlobalKey,
    pub(crate) ty: Type,
    pub(crate) mutability: Mutability,
    pub(crate) policy: AccessPolicy,
    pub(crate) span: SourceSpan,
    pub(crate) initializer_span: SourceSpan,
}

#[derive(Debug, Clone)]
pub(crate) struct CallableSig {
    pub(crate) owner_generics: GenericParams,
    pub(crate) generics: GenericParams,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) default_sites: Vec<Option<ParamDefaultSite>>,
    pub(crate) required_params: usize,
    pub(crate) ret: ReturnSpec,
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
    pub(crate) is_stringify_override: bool,
}

#[derive(Clone)]
pub(crate) struct NamedSchemas<T> {
    entries: Vec<NamedSchema<T>>,
    by_name: HashMap<Ident, usize>,
}

#[derive(Clone)]
pub(crate) struct NamedSchema<T> {
    name: Ident,
    schema: T,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct DuplicateName;

impl<T> Default for NamedSchemas<T> {
    fn default() -> Self {
        Self {
            entries: vec![],
            by_name: HashMap::new(),
        }
    }
}

impl<T> NamedSchemas<T> {
    pub(crate) fn insert(&mut self, name: Ident, schema: T) -> Result<(), DuplicateName> {
        match self.by_name.entry(name) {
            Entry::Occupied(_) => Err(DuplicateName),
            Entry::Vacant(entry) => {
                let slot = self.entries.len();
                self.entries.push(NamedSchema { name, schema });
                entry.insert(slot);
                Ok(())
            }
        }
    }

    pub(crate) fn get(&self, name: Ident) -> Option<&T> {
        self.by_name
            .get(&name)
            .map(|slot| &self.entries[*slot].schema)
    }

    fn get_mut(&mut self, name: Ident) -> Option<&mut T> {
        let slot = *self.by_name.get(&name)?;
        Some(&mut self.entries[slot].schema)
    }

    pub(crate) fn contains_key(&self, name: Ident) -> bool {
        self.by_name.contains_key(&name)
    }

    pub(crate) fn slot(&self, name: Ident) -> Option<usize> {
        self.by_name.get(&name).copied()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (Ident, &T)> {
        self.entries.iter().map(|entry| (entry.name, &entry.schema))
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &T> {
        self.entries.iter().map(|entry| &entry.schema)
    }

    pub(crate) fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.entries.iter_mut().map(|entry| &mut entry.schema)
    }

    pub(crate) fn names(&self) -> impl Iterator<Item = Ident> + '_ {
        self.entries.iter().map(|entry| entry.name)
    }

    fn invariants_hold(&self) -> bool {
        self.entries.len() == self.by_name.len()
            && self
                .entries
                .iter()
                .enumerate()
                .all(|(slot, entry)| self.by_name.get(&entry.name) == Some(&slot))
    }
}

#[derive(Clone)]
pub(crate) struct AggregateSchema {
    pub(crate) key: NominalKey,
    pub(crate) generics: GenericParams,
    pub(crate) fields: NamedSchemas<FieldSchema>,
    pub(crate) methods: HashMap<MethodKey, MethodSchema>,
    pub(crate) promoted: PromotedSurface,
    pub(crate) dependent_embeds: Vec<DependentEmbedTemplate>,
    pub(crate) projection_edges: Vec<ProjectionPath>,
    pub(crate) policy: AccessPolicy,
}

#[derive(Clone)]
pub(crate) struct EmbeddedFieldRef<'a> {
    pub(crate) owner: &'a NominalKey,
    pub(crate) name: Ident,
    pub(crate) field: &'a FieldSchema,
    pub(crate) embed: &'a EmbedFieldSchema,
}

#[derive(Clone)]
pub(crate) struct DependentEmbedTemplate {
    pub(crate) field_path: Vec<Ident>,
    pub(crate) target_ty: Type,
    pub(crate) selector: Option<ast::EmbedSelector>,
    pub(crate) exposure: Exposure,
    pub(crate) span: SourceSpan,
}

#[derive(Clone)]
pub(crate) struct ProjectionPath {
    pub(crate) target_ty: Type,
    pub(crate) field_path: Vec<Ident>,
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
}

#[derive(Clone)]
pub(crate) struct ContractSchema {
    pub(crate) key: ContractKey,
    pub(crate) visibility: Visibility,
    pub(crate) includes: Vec<(ContractRef, SourceSpan)>,
    pub(crate) direct_requirements: Vec<ContractRequirementSchema>,
    pub(crate) requirements: Vec<ContractRequirementSchema>,
    pub(crate) contract_set: Option<ContractSetKey>,
    pub(crate) policy: AccessPolicy,
    pub(crate) span: SourceSpan,
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct ContractRequirementSchema {
    pub(crate) name: Ident,
    pub(crate) receiver: Option<MethodReceiver>,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) param_spans: ParamTypeSpans,
    pub(crate) required_params: usize,
    pub(crate) ret: ReturnSpec,
    pub(crate) generics_empty: bool,
    pub(crate) span: Option<SourceSpan>,
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct FieldDefault {
    pub(crate) expr_id: ast::ExprId,
    pub(crate) span: SourceSpan,
}

#[derive(Clone)]
pub(crate) struct FieldSchema {
    pub(crate) ty: Type,
    pub(crate) default: Option<FieldDefault>,
    pub(crate) policy: AccessPolicy,
    pub(crate) span: Option<SourceSpan>,
    pub(crate) embed: Option<EmbedFieldSchema>,
}

impl FieldSchema {
    pub(crate) fn has_default(&self) -> bool {
        self.default.is_some()
    }
}

#[derive(Clone)]
pub(crate) struct EmbedFieldSchema {
    pub(crate) selector: Option<ast::EmbedSelector>,
    pub(crate) exposure: Exposure,
    pub(crate) as_projection: bool,
    pub(crate) span: SourceSpan,
}

#[derive(Clone)]
pub(crate) struct MethodSchema {
    pub(crate) mode: MethodMode,
    pub(crate) generics: GenericParams,
    pub(crate) params: Vec<FuncParam>,
    pub(crate) param_spans: ParamTypeSpans,
    pub(crate) default_sites: Vec<Option<ParamDefaultSite>>,
    pub(crate) required_params: usize,
    pub(crate) ret: ReturnSpec,
    pub(crate) policy: AccessPolicy,
}

impl AggregateSchema {
    pub(crate) fn stringify_override(&self) -> Option<&MethodSchema> {
        self.methods
            .get(&MethodKey::instance(Ident::new("to_string")))
            .filter(|method| method.is_stringify_override())
    }
}

impl MethodSchema {
    pub(crate) fn is_stringify_override(&self) -> bool {
        self.mode == (MethodMode::Instance { mutable: false })
            && self.params.is_empty()
            && self.generics.is_empty()
            && self.ret.ty == Type::String
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum EnumRepr {
    Adt,
    RawInt,
    RawString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum RawEnumValue {
    Int(i64),
    String(String),
}

pub(super) struct VerifiedRawEnumMetadata {
    pub(super) repr: EnumRepr,
    pub(super) values: Vec<(Ident, RawEnumValue)>,
}

#[derive(Clone)]
pub(crate) struct EnumSchema {
    pub(crate) generics: GenericParams,
    pub(crate) repr: EnumRepr,
    pub(crate) variants: NamedSchemas<VariantSchema>,
    pub(crate) policy: AccessPolicy,
}

#[derive(Clone)]
pub(crate) struct VariantSchema {
    pub(crate) policy: AccessPolicy,
    pub(crate) payload: VariantPayload,
    pub(crate) raw_value: Option<RawEnumValue>,
}

#[derive(Clone)]
pub(crate) enum VariantPayload {
    Unit,
    Tuple(Vec<Type>),
    Struct(NamedSchemas<FieldSchema>),
}

impl VariantPayload {
    pub(crate) fn for_each_type(&self, mut f: impl FnMut(&Type)) {
        match self {
            Self::Unit => {}
            Self::Tuple(items) => {
                for item in items {
                    f(item);
                }
            }
            Self::Struct(fields) => {
                for field in fields.values() {
                    f(&field.ty);
                }
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct ExtendSchema {
    pub(crate) id: ExtendId,
    pub(crate) origin: ModuleScope,
    pub(crate) exported: bool,
    pub(crate) target: Type,
    pub(crate) target_constraint: Option<ExtendTargetConstraint>,
    pub(crate) generics: GenericParams,
    pub(crate) methods: HashMap<MethodKey, ExtendMethodSchema>,
    pub(crate) cast_froms: Vec<CastConversionSchema>,
    pub(crate) span: SourceSpan,
}

#[derive(Clone)]
pub(crate) struct CastConversionSchema {
    pub(crate) param: FuncParam,
    pub(crate) param_span: Span,
    pub(crate) ret: Option<ReturnSpec>,
    pub(crate) span: SourceSpan,
}

pub(crate) type ExtendMethodSchema = MethodSchema;

pub(crate) enum ExtendMethodMatch<'a> {
    Match {
        extend: &'a ExtendSchema,
        method: &'a ExtendMethodSchema,
        receiver_ty: Type,
        owner_args: Result<GenericArgs, Vec<Ident>>,
    },
    Ambiguous,
}

#[derive(Clone)]
pub(crate) struct CastFromConversion {
    pub(crate) escape: EscapeMode,
    pub(crate) origin: ModuleScope,
    pub(crate) instance: CastFromInstanceKey,
}

pub(crate) enum CastConversionMatch {
    Match(CastFromConversion),
    Ambiguous,
}

#[derive(Clone)]
pub(crate) struct DeclTypeSite {
    pub(crate) module: ModuleScope,
    pub(crate) span: Span,
    pub(crate) generics: GenericTypeContext,
    pub(crate) type_params: Vec<TypeParam>,
    pub(crate) kind: DeclTypeUseKind,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeclTypeUseKind {
    GenericBound,
    Field,
    MethodParam,
    MethodReturn,
    EnumVariant,
    ContractParam,
    ContractReturn,
    AliasTarget,
    ExtendTarget,
    CastParam,
    CastReturn,
    FunctionParam,
    FunctionReturn,
    ExternFunctionParam,
    ExternFunctionReturn,
    Const,
    Global,
}

impl DeclTypeUseKind {
    pub(crate) fn rejects_raw_dyn_hole(self) -> bool {
        matches!(
            self,
            Self::Field
                | Self::EnumVariant
                | Self::AliasTarget
                | Self::Const
                | Self::Global
                | Self::ExternFunctionParam
                | Self::ExternFunctionReturn
        )
    }
}

fn combined_type_params(owner: &[TypeParam], local: &[TypeParam]) -> Vec<TypeParam> {
    owner.iter().chain(local).cloned().collect()
}

fn type_site(
    module: &ModuleScope,
    span: Span,
    generics: &GenericTypeContext,
    type_params: &[TypeParam],
    kind: DeclTypeUseKind,
) -> DeclTypeSite {
    DeclTypeSite {
        module: module.clone(),
        span,
        generics: generics.clone(),
        type_params: type_params.to_vec(),
        kind,
    }
}

fn bare_type_site(module: &ModuleScope, span: Span, kind: DeclTypeUseKind) -> DeclTypeSite {
    type_site(module, span, &GenericTypeContext::default(), &[], kind)
}

pub(crate) fn map_generic_param_bounds<F>(params: &mut GenericParams, mut f: F)
where
    F: FnMut(Type) -> Type,
{
    for param in &mut params.type_params {
        param.bounds = std::mem::take(&mut param.bounds)
            .into_iter()
            .filter_map(|bound| match f(Type::Dyn(bound)) {
                Type::Dyn(bound) if !matches!(bound, ContractRef::Infer | ContractRef::Hole(_)) => {
                    Some(bound)
                }
                _ => None,
            })
            .collect();
    }
}

fn map_generic_bounds<F>(
    module: &ModuleScope,
    span: Span,
    generics: &GenericTypeContext,
    params: &mut GenericParams,
    f: &mut F,
) where
    F: FnMut(DeclTypeSite, Type) -> Type,
{
    let type_params = params.type_params.clone();
    map_generic_param_bounds(params, |bound| {
        let site = type_site(
            module,
            span,
            generics,
            &type_params,
            DeclTypeUseKind::GenericBound,
        );
        f(site, bound)
    });
}

pub(crate) struct GenericContextError {
    pub(crate) module: ModuleScope,
    pub(crate) error: GenericParamError,
    pub(crate) span: Span,
}

pub(crate) struct SourceModule<'a> {
    pub(crate) scope: ModuleScope,
    pub(crate) source: SourceId,
    pub(crate) program: &'a Program,
}

impl DeclarationIndex {
    pub(crate) fn from_root_and_modules(
        root: &Program,
        resolved: &ResolveResult,
        externs: &RawExterns,
    ) -> Self {
        let mut index = Self::default();
        let modules = Self::source_modules(root, resolved);
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

    pub(crate) fn import_records(&self) -> &[ImportRecord] {
        &self.import_records
    }

    pub(crate) fn used_imports(&self) -> &HashSet<ImportId> {
        &self.used_imports
    }

    pub(crate) fn mark_import_used(&mut self, import: Option<ImportId>) {
        if let Some(import) = import {
            self.used_imports.insert(import);
        }
    }

    pub(crate) fn source_modules<'a>(
        root: &'a Program,
        resolved: &'a ResolveResult,
    ) -> Vec<SourceModule<'a>> {
        let mut modules = vec![SourceModule {
            scope: ModuleScope::from_module_id(&resolved.root),
            source: resolved.root_source,
            program: root,
        }];
        for group in &resolved.module_groups {
            for module in group {
                if module.key == resolved.root {
                    continue;
                }
                modules.push(SourceModule {
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

    pub(crate) fn fold_canonical_type_uses<F>(&mut self, mut f: F) -> Vec<GenericContextError>
    where
        F: FnMut(DeclTypeSite, Type) -> Type,
    {
        let mut errors = vec![];

        for (key, schema) in &mut self.aggregates {
            let Some(span) = self.type_spans.get(key).copied() else {
                continue;
            };
            let owner_generics = generic_context(
                &key.module,
                &schema.generics.type_params,
                &schema.generics.const_params,
                span,
                &mut errors,
            );
            map_generic_bounds(
                &key.module,
                span,
                &owner_generics,
                &mut schema.generics,
                &mut f,
            );
            let owner_type_params = schema.generics.type_params.clone();
            for field in schema.fields.values_mut() {
                let site = type_site(
                    &key.module,
                    span,
                    &owner_generics,
                    &owner_type_params,
                    DeclTypeUseKind::Field,
                );
                field.ty = f(site, field.ty.clone());
            }
            for method in schema.methods.values_mut() {
                let generics = extend_generic_context(
                    &key.module,
                    &owner_generics,
                    &method.generics.type_params,
                    &method.generics.const_params,
                    span,
                    &mut errors,
                );
                map_generic_bounds(&key.module, span, &generics, &mut method.generics, &mut f);
                let type_params =
                    combined_type_params(&owner_type_params, &method.generics.type_params);
                for (index, param) in method.params.iter_mut().enumerate() {
                    let site = type_site(
                        &key.module,
                        method.param_spans.span_for(index, span),
                        &generics,
                        &type_params,
                        DeclTypeUseKind::MethodParam,
                    );
                    param.ty = f(site, param.ty.clone());
                }
                let site = type_site(
                    &key.module,
                    span,
                    &generics,
                    &type_params,
                    DeclTypeUseKind::MethodReturn,
                );
                method.ret.ty = f(site, method.ret.ty.clone());
            }
        }

        for (key, schema) in &mut self.enums {
            let Some(span) = self.type_spans.get(key).copied() else {
                continue;
            };
            let generics = generic_context(
                &key.module,
                &schema.generics.type_params,
                &schema.generics.const_params,
                span,
                &mut errors,
            );
            map_generic_bounds(&key.module, span, &generics, &mut schema.generics, &mut f);
            let type_params = schema.generics.type_params.clone();
            for variant in schema.variants.values_mut() {
                match &mut variant.payload {
                    VariantPayload::Unit => {}
                    VariantPayload::Tuple(types) => {
                        for ty in types {
                            let site = type_site(
                                &key.module,
                                span,
                                &generics,
                                &type_params,
                                DeclTypeUseKind::EnumVariant,
                            );
                            *ty = f(site, ty.clone());
                        }
                    }
                    VariantPayload::Struct(fields) => {
                        for field in fields.values_mut() {
                            let site = type_site(
                                &key.module,
                                span,
                                &generics,
                                &type_params,
                                DeclTypeUseKind::EnumVariant,
                            );
                            field.ty = f(site, field.ty.clone());
                        }
                    }
                }
            }
        }

        for (key, schema) in &mut self.contracts {
            for req in &mut schema.direct_requirements {
                let Some(span) = req.span else {
                    continue;
                };
                for (index, param) in req.params.iter_mut().enumerate() {
                    let site = bare_type_site(
                        &key.module,
                        req.param_spans.span_for(index, span.byte()),
                        DeclTypeUseKind::ContractParam,
                    );
                    param.ty = f(site, param.ty.clone());
                }
                let site =
                    bare_type_site(&key.module, span.byte(), DeclTypeUseKind::ContractReturn);
                req.ret.ty = f(site, req.ret.ty.clone());
            }
        }

        for schema in self.type_aliases.values_mut() {
            let span = schema.def.span.byte();
            let generics = generic_context(
                &schema.def.module,
                &schema.def.generics.type_params,
                &schema.def.generics.const_params,
                span,
                &mut errors,
            );
            schema.def.generic_context = generics.clone();
            map_generic_bounds(
                &schema.def.module,
                span,
                &generics,
                &mut schema.def.generics,
                &mut f,
            );
            let site = type_site(
                &schema.def.module,
                span,
                &generics,
                &schema.def.generics.type_params,
                DeclTypeUseKind::AliasTarget,
            );
            schema.def.aliased = f(site, schema.def.aliased.clone());
        }

        for extend in &mut self.extends {
            let origin = extend.origin.clone();
            let span = extend.span.byte();
            let generics = generic_context(
                &origin,
                &extend.generics.type_params,
                &extend.generics.const_params,
                span,
                &mut errors,
            );
            map_generic_bounds(&origin, span, &generics, &mut extend.generics, &mut f);
            let extend_type_params = extend.generics.type_params.clone();
            let target_site = type_site(
                &origin,
                span,
                &generics,
                &extend_type_params,
                DeclTypeUseKind::ExtendTarget,
            );
            extend.target = f(target_site, extend.target.clone());
            for method in extend.methods.values_mut() {
                let method_generics = extend_generic_context(
                    &origin,
                    &generics,
                    &method.generics.type_params,
                    &method.generics.const_params,
                    span,
                    &mut errors,
                );
                map_generic_bounds(
                    &origin,
                    span,
                    &method_generics,
                    &mut method.generics,
                    &mut f,
                );
                let type_params =
                    combined_type_params(&extend_type_params, &method.generics.type_params);
                for (index, param) in method.params.iter_mut().enumerate() {
                    let site = type_site(
                        &origin,
                        method.param_spans.span_for(index, span),
                        &method_generics,
                        &type_params,
                        DeclTypeUseKind::MethodParam,
                    );
                    param.ty = f(site, param.ty.clone());
                }
                let site = type_site(
                    &origin,
                    span,
                    &method_generics,
                    &type_params,
                    DeclTypeUseKind::MethodReturn,
                );
                method.ret.ty = f(site, method.ret.ty.clone());
            }
            for cast in &mut extend.cast_froms {
                let site = type_site(
                    &origin,
                    cast.span.byte(),
                    &generics,
                    &extend_type_params,
                    DeclTypeUseKind::CastReturn,
                );
                cast.param.ty = f(
                    DeclTypeSite {
                        span: cast.param_span,
                        kind: DeclTypeUseKind::CastParam,
                        ..site.clone()
                    },
                    cast.param.ty.clone(),
                );
                if let Some(ret) = &mut cast.ret {
                    ret.ty = f(site, ret.ty.clone());
                }
            }
        }

        for decls in self.modules.values_mut() {
            for value in decls.locals.values.values_mut() {
                let Some(span) = self
                    .value_spans
                    .get(&(value.module.clone(), value.name))
                    .copied()
                else {
                    continue;
                };
                match &mut value.decl {
                    ValueDecl::Func(sig) => {
                        let generics = generic_context(
                            &value.module,
                            &sig.generics.type_params,
                            &sig.generics.const_params,
                            span,
                            &mut errors,
                        );
                        map_generic_bounds(
                            &value.module,
                            span,
                            &generics,
                            &mut sig.generics,
                            &mut f,
                        );
                        let kind = if sig.kind == CallableKind::ExternFunction {
                            DeclTypeUseKind::ExternFunctionReturn
                        } else {
                            DeclTypeUseKind::FunctionReturn
                        };
                        let site = type_site(
                            &value.module,
                            span,
                            &generics,
                            &sig.generics.type_params,
                            kind,
                        );
                        sig.ty =
                            fold_func_type_uses(site, sig.ty.clone(), &sig.param_spans, &mut f);
                    }
                    ValueDecl::Const(sig) => {
                        let site = bare_type_site(&value.module, span, DeclTypeUseKind::Const);
                        sig.ty = f(site, sig.ty.clone());
                    }
                    ValueDecl::Global(sig) => {
                        let site = bare_type_site(&value.module, span, DeclTypeUseKind::Global);
                        sig.ty = f(site, sig.ty.clone());
                    }
                }
            }
        }

        self.sync_value_projections();
        errors
    }

    pub(crate) fn embedded_fields(&self) -> impl Iterator<Item = EmbeddedFieldRef<'_>> {
        self.aggregates.iter().flat_map(|(owner, schema)| {
            schema.fields.iter().filter_map(move |(name, field)| {
                let embed = field.embed.as_ref()?;
                Some(EmbeddedFieldRef {
                    owner,
                    name,
                    field,
                    embed,
                })
            })
        })
    }

    pub(crate) fn build_projection_edges(&mut self) -> Vec<DeclError> {
        let mut errors = vec![];
        let mut keys = self.aggregates.keys().cloned().collect::<Vec<_>>();
        keys.sort_by_key(nominal_key_sort_key);
        for key in keys {
            let mut projections = vec![];
            let mut seen = HashSet::new();
            for field in self.embedded_fields().filter(|field| field.owner == &key) {
                if !field.embed.as_projection {
                    continue;
                }
                let target_ty = field.field.ty.clone();
                let target = CanonicalTypeKey(target_ty.clone());
                if !type_depends_on_generics(&target_ty) && !seen.insert(target) {
                    errors.push(DeclError::DuplicateProjectionTarget {
                        owner: key.clone(),
                        target: target_ty,
                        span: field.field.span.or(Some(field.embed.span)),
                    });
                    continue;
                }
                projections.push(ProjectionPath {
                    target_ty,
                    field_path: vec![field.name],
                });
            }
            if let Some(schema) = self.aggregates.get_mut(&key) {
                schema.projection_edges = projections;
            }
        }
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
            Some(MethodReceiver::Value)
                if !sig.type_params.is_empty() || !sig.const_params.is_empty() =>
            {
                Some("to_string method cannot be generic")
            }
            Some(MethodReceiver::Value) if !sig.params.is_empty() => {
                Some("to_string method must take no parameters")
            }
            Some(MethodReceiver::Value) if sig.ret.ty != Type::String => {
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

    fn collect_field_schemas(
        &mut self,
        source: SourceId,
        fields: &[ast::StructField],
        defaults: bool,
        embeds: bool,
        mut duplicate: impl FnMut(Ident, SourceSpan) -> DeclError,
    ) -> NamedSchemas<FieldSchema> {
        let mut schemas = NamedSchemas::default();
        for field in fields {
            let annotations = annotation::normalize_field_annotations(
                source,
                &field.annotations,
                embeds && field.embed.is_some(),
                &mut self.errors,
            );
            let span = SourceSpan::from_byte_span(source, field.span);
            let default = if defaults {
                field.default.as_ref().map(|default| FieldDefault {
                    expr_id: default.node.id,
                    span: SourceSpan::from_byte_span(source, default.span),
                })
            } else {
                None
            };
            let schema = FieldSchema {
                ty: field.ty.clone(),
                default,
                policy: annotations.policy,
                span: Some(span),
                embed: embeds
                    .then(|| embed_field_schema(field, annotations.as_projection, source))
                    .flatten(),
            };
            if schemas.insert(field.name, schema).is_err() {
                self.errors.push(duplicate(field.name, span));
            }
        }
        schemas
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
                            param_spans: ParamTypeSpans::from_params(&func.params),
                            default_sites: ParamDefaultSite::from_params(source, &func.params),
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
                    let fields = self.collect_field_schemas(
                        source,
                        &agg.fields,
                        true,
                        true,
                        |name, span| DeclError::DuplicateAggregateField {
                            owner: key.clone(),
                            name,
                            span: Some(span),
                        },
                    );
                    validate_embed_field_schemas(&fields, &mut self.errors);
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
                        let schema = method_schema(source, &method.sig, mode, policy);
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
                                promoted: PromotedSurface::default(),
                                dependent_embeds: vec![],
                                projection_edges: vec![],
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
                    let mut variants = NamedSchemas::default();
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
                                VariantPayload::Struct(self.collect_field_schemas(
                                    source,
                                    fields,
                                    false,
                                    false,
                                    |name, span| DeclError::DuplicateVariantField {
                                        owner: key.clone(),
                                        variant: variant.name,
                                        name,
                                        span: Some(span),
                                    },
                                ))
                            }
                        };
                        let schema = VariantSchema {
                            policy: variant_policy,
                            payload,
                            raw_value: None,
                        };
                        if variants.insert(variant.name, schema).is_err() {
                            self.errors.push(DeclError::DuplicateEnumVariant {
                                owner: key.clone(),
                                name: variant.name,
                                span: Some(SourceSpan::from_byte_span(source, variant.span)),
                            });
                        }
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
                        if enm.raw_backing.is_some()
                            || enm
                                .variants
                                .iter()
                                .any(|variant| variant.raw_value.is_some())
                        {
                            self.pending_raw_enums.insert(
                                key.clone(),
                                PendingRawEnum {
                                    source,
                                    backing: enm.raw_backing.clone(),
                                    variants: enm
                                        .variants
                                        .iter()
                                        .map(|variant| PendingRawVariant {
                                            name: variant.name,
                                            span: variant.span,
                                            value: variant.raw_value.clone(),
                                        })
                                        .collect(),
                                },
                            );
                        }
                        self.enums.insert(
                            key.clone(),
                            EnumSchema {
                                generics: generic_params(&enm.type_params, &enm.const_params),
                                repr: EnumRepr::Adt,
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
                Stmt::Global(global_node) => {
                    let global = &global_node.node;
                    let policy = annotation::normalize_annotations(
                        source,
                        &global.annotations,
                        annotation::AnnotationTarget::Global,
                        &mut self.errors,
                    );
                    let key = GlobalKey {
                        module: scope.clone(),
                        name: global.name,
                    };
                    let span = SourceSpan::from_byte_span(source, global_node.span);
                    let initializer_span = SourceSpan::from_byte_span(source, global.value.span);
                    let value = ResolvedValue {
                        module: scope.clone(),
                        name: global.name,
                        decl: ValueDecl::Global(GlobalSig {
                            key,
                            ty: global.ty.clone().unwrap_or(Type::Infer),
                            mutability: global.mutability,
                            policy,
                            span,
                            initializer_span,
                        }),
                    };
                    self.insert_local_value(
                        &mut decls,
                        &scope,
                        global.name,
                        value,
                        exported,
                        Some(span),
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
                            },
                        );
                    }
                }
                Stmt::Contract(contract_node) => {
                    let contract = &contract_node.node;
                    let policy = annotation::normalize_annotations(
                        source,
                        &contract.annotations,
                        annotation::AnnotationTarget::Contract,
                        &mut self.errors,
                    );
                    let key = ContractKey {
                        module: scope.clone(),
                        name: contract.name,
                    };
                    if self.insert_local_type(
                        &mut decls,
                        &scope,
                        contract.name,
                        TypeBinding::Contract(key.clone()),
                        exported,
                        Some(SourceSpan::from_byte_span(source, contract_node.span)),
                    ) {
                        let span = SourceSpan::from_byte_span(source, contract_node.span);
                        let requirements = contract
                            .requirements
                            .iter()
                            .map(|req| ContractRequirementSchema {
                                name: req.node.sig.name,
                                receiver: req.node.sig.receiver,
                                params: resolve_func_params(&req.node.sig.params),
                                param_spans: ParamTypeSpans::from_params(&req.node.sig.params),
                                required_params: required_param_count(&req.node.sig.params),
                                ret: req.node.sig.ret.clone(),
                                generics_empty: req.node.sig.type_params.is_empty()
                                    && req.node.sig.const_params.is_empty(),
                                span: Some(SourceSpan::from_byte_span(source, req.span)),
                            })
                            .collect();
                        self.contracts.insert(
                            key.clone(),
                            ContractSchema {
                                key,
                                visibility: contract.visibility,
                                includes: contract
                                    .includes
                                    .iter()
                                    .map(|include| {
                                        (
                                            include.node.clone(),
                                            SourceSpan::from_byte_span(source, include.span),
                                        )
                                    })
                                    .collect(),
                                direct_requirements: requirements,
                                requirements: vec![],
                                contract_set: None,
                                policy,
                                span,
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
                        let schema = method_schema(source, &m.sig, mode, policy);
                        let surface = key.surface;
                        match methods.entry(key) {
                            Entry::Occupied(_) => {
                                self.errors.push(DeclError::DuplicateExtendMethod {
                                    name: m.sig.name,
                                    surface,
                                    span: Some(SourceSpan::from_byte_span(
                                        source,
                                        method_node.span,
                                    )),
                                });
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(schema);
                            }
                        }
                    }
                    let cast_froms = ext
                        .cast_froms
                        .iter()
                        .map(|cast| CastConversionSchema {
                            param: cast.node.param.func_param(),
                            param_span: cast.node.param.ty_span,
                            ret: cast.node.ret.clone(),
                            span: SourceSpan::from_byte_span(source, cast.span),
                        })
                        .collect();
                    self.extends.push(ExtendSchema {
                        id,
                        origin: scope.clone(),
                        exported,
                        target: ext.ty.clone(),
                        target_constraint: ext.target_constraint,
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
                        ret: Box::new(ReturnSpec::void()),
                    },
                    param_spans: ParamTypeSpans::default(),
                    default_sites: vec![],
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
        modules: &[SourceModule<'_>],
        resolved: &ResolveResult,
    ) {
        for module in modules.iter().skip(1).chain(modules.first()) {
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

            let (exports, active_modules, private_reexports, errors) =
                builder.finish_reexport_scope();
            self.errors.extend(errors);
            if let Some(decls) = self.modules.get_mut(&module.scope) {
                decls.exports = exports;
                decls.exported_active_modules = active_modules;
                decls.private_reexports = private_reexports;
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

    fn expand_active_modules(&self, imports: &mut ImportScope) {
        let roots = imports.active_modules.iter().cloned().collect::<Vec<_>>();
        for module in roots {
            let Some(decls) = self.modules.get(&module) else {
                continue;
            };
            let activating_imports = imports
                .active_module_imports
                .get(&module)
                .cloned()
                .unwrap_or_default();
            for exported in &decls.exported_active_modules {
                imports.active_modules.insert(exported.clone());
                let target = imports
                    .active_module_imports
                    .entry(exported.clone())
                    .or_default();
                for import in &activating_imports {
                    if !target.contains(import) {
                        target.push(import.clone());
                    }
                }
            }
        }
    }

    fn build_import_scopes(&mut self, modules: &[SourceModule<'_>], resolved: &ResolveResult) {
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
            self.expand_active_modules(&mut imports);
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
        builder.copy_wildcard_members(
            decls,
            None,
            &ImportScopeBuilder::implicit_origin(core.clone()),
        );
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
            let import_id = ImportId {
                module: scope.clone(),
                ordinal: u32::try_from(import_ordinal).expect("import ordinal fits in u32"),
            };
            if builder.mode == ImportMode::Import {
                self.import_records.push(ImportRecord {
                    id: import_id.clone(),
                    visibility: import.node.visibility,
                    span: import_span,
                    target_module: target.module.clone(),
                });
            }
            if builder.mode == ImportMode::Import {
                builder.record_activation(target.module.clone(), import_id.clone());
            } else {
                builder.active_modules.insert(target.module.clone());
            }
            let dep = self.modules.get(&target.module);
            let validate_members = builder.mode == ImportMode::Reexport || !is_public;
            builder.apply_import(
                &import.node.kind,
                &target.module,
                target.default_name,
                dep,
                Some(import_span),
                validate_members,
                import_id,
            );
        }
    }

    pub(crate) fn local_value(&self, module: &ModuleScope, name: Ident) -> Option<ResolvedValue> {
        self.modules.get(module)?.locals.value(name).cloned()
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &ResolvedValue> {
        self.modules
            .values()
            .flat_map(|module| module.locals.values.values())
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
        if let Some(binding) = decls.exports.ty(name) {
            return ModuleMemberLookup::Found(binding.clone());
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

    #[cfg(test)]
    pub(crate) fn imported_value(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<ResolvedValue> {
        self.imported_value_with_import(module, name)
            .map(|(value, _)| value)
    }

    pub(crate) fn imported_value_with_import(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<(ResolvedValue, Option<ImportId>)> {
        let imports = &self.modules.get(module)?.imports;
        Some((
            imports.namespace.value(name)?.clone(),
            imports.binding_origin(BindingNamespace::Value, name),
        ))
    }

    #[cfg(test)]
    pub(crate) fn imported_type_binding(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<TypeBinding> {
        self.imported_type_binding_with_import(module, name)
            .map(|(binding, _)| binding)
    }

    pub(crate) fn imported_type_binding_with_import(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<(TypeBinding, Option<ImportId>)> {
        let imports = &self.modules.get(module)?.imports;
        Some((
            imports.namespace.ty(name)?.clone(),
            imports.binding_origin(BindingNamespace::Type, name),
        ))
    }

    #[cfg(test)]
    pub(crate) fn imported_module(&self, module: &ModuleScope, name: Ident) -> Option<ModuleScope> {
        self.imported_module_with_import(module, name)
            .map(|(module, _)| module)
    }

    pub(crate) fn imported_module_with_import(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<(ModuleScope, Option<ImportId>)> {
        let imports = &self.modules.get(module)?.imports;
        Some((
            imports.namespace.module(name)?.clone(),
            imports.binding_origin(BindingNamespace::Module, name),
        ))
    }

    pub(crate) fn visible_type_binding_with_import(
        &self,
        module: &ModuleScope,
        name: Ident,
    ) -> Option<(TypeBinding, Option<ImportId>)> {
        self.local_type_binding(module, name)
            .map(|binding| (binding, None))
            .or_else(|| self.imported_type_binding_with_import(module, name))
    }

    pub(crate) fn resolve_type_member(
        &self,
        module: &ModuleScope,
        qualifier: Option<Ident>,
        name: Ident,
    ) -> TypeMemberLookup {
        match qualifier {
            Some(alias) => {
                let Some((target, import)) = self.imported_module_with_import(module, alias) else {
                    return TypeMemberLookup {
                        result: ModuleMemberLookup::Missing,
                        import: None,
                        target: None,
                    };
                };
                let result = self.module_type(&target, name);
                TypeMemberLookup {
                    result,
                    import,
                    target: Some(target),
                }
            }
            None => match self.visible_type_binding_with_import(module, name) {
                Some((binding, import)) => TypeMemberLookup {
                    result: ModuleMemberLookup::Found(binding),
                    import,
                    target: None,
                },
                None => TypeMemberLookup {
                    result: ModuleMemberLookup::Missing,
                    import: None,
                    target: None,
                },
            },
        }
    }

    pub(crate) fn imports_module(&self, module: &ModuleScope, imported: &ModuleScope) -> bool {
        self.modules
            .get(module)
            .is_some_and(|decls| decls.imports.active_modules.contains(imported))
    }

    pub(crate) fn active_import_ids(
        &self,
        module: &ModuleScope,
        imported: &ModuleScope,
    ) -> &[ImportId] {
        self.modules
            .get(module)
            .and_then(|decls| decls.imports.active_module_imports.get(imported))
            .map_or(&[], Vec::as_slice)
    }

    pub(crate) fn set_const_type(&mut self, scope: &ModuleScope, name: Ident, ty: &Type) {
        self.update_values(|value| {
            if value.module == *scope
                && value.name == name
                && let ValueDecl::Const(sig) = &mut value.decl
            {
                sig.ty = ty.clone();
            }
        });
    }

    pub(crate) fn set_func_type(&mut self, scope: &ModuleScope, name: Ident, ty: &Type) {
        self.update_values(|value| {
            if value.module == *scope
                && value.name == name
                && let ValueDecl::Func(sig) = &mut value.decl
            {
                sig.ty = ty.clone();
            }
        });
    }

    pub(crate) fn set_global_type(&mut self, key: &GlobalKey, ty: &Type) {
        self.update_values(|value| {
            if let ValueDecl::Global(sig) = &mut value.decl
                && sig.key == *key
            {
                sig.ty = ty.clone();
            }
        });
    }

    fn update_values(&mut self, mut f: impl FnMut(&mut ResolvedValue)) {
        for decls in self.modules.values_mut() {
            for namespace in [
                &mut decls.locals,
                &mut decls.exports,
                &mut decls.imports.namespace,
            ] {
                for value in namespace.values.values_mut() {
                    f(value);
                }
            }
        }
    }

    pub(crate) fn global(&self, key: &GlobalKey) -> Option<GlobalSig> {
        let value = self.local_value(&key.module, key.name)?;
        match value.decl {
            ValueDecl::Global(sig) => Some(sig),
            ValueDecl::Func(_) | ValueDecl::Const(_) => None,
        }
    }

    pub(crate) fn aggregate(&self, key: &NominalKey) -> Option<&AggregateSchema> {
        let aggregate = self.aggregates.get(key)?;
        debug_assert!(aggregate.promoted.invariants_hold());
        debug_assert!(
            aggregate
                .dependent_embeds
                .iter()
                .all(dependent_embed_template_valid)
        );
        debug_assert!(aggregate.projection_edges.iter().all(projection_path_valid));
        debug_assert!(aggregate.fields.invariants_hold());
        debug_assert!(
            aggregate
                .fields
                .iter()
                .enumerate()
                .all(|(slot, (name, _))| { self.aggregate_field_index(key, name) == Some(slot) })
        );
        Some(aggregate)
    }

    pub(crate) fn aggregate_mut(&mut self, key: &NominalKey) -> Option<&mut AggregateSchema> {
        self.aggregates.get_mut(key)
    }

    pub(super) fn projection_paths_from(&self, source: &Type) -> Vec<ProjectionPath> {
        let mut paths = vec![];
        self.collect_projection_paths(source, &mut vec![], &mut HashSet::new(), &mut paths);
        paths
    }

    fn collect_projection_paths(
        &self,
        source: &Type,
        prefix: &mut Vec<Ident>,
        seen: &mut HashSet<NominalKey>,
        paths: &mut Vec<ProjectionPath>,
    ) {
        let Some(key) = self.key_for_type(source) else {
            return;
        };
        let Some(aggregate) = self.aggregate(&key) else {
            return;
        };
        if !seen.insert(key.clone()) {
            return;
        }

        for edge in &aggregate.projection_edges {
            let target_ty =
                substitute_aggregate_member(source, &aggregate.generics, &edge.target_ty);
            let start = prefix.len();
            prefix.extend(edge.field_path.iter().copied());
            paths.push(ProjectionPath {
                target_ty: target_ty.clone(),
                field_path: prefix.clone(),
            });
            self.collect_projection_paths(&target_ty, prefix, seen, paths);
            prefix.truncate(start);
        }

        seen.remove(&key);
    }

    pub(super) fn bare_embed_paths_to_type(&self, source: &Type, target: &Type) -> Vec<Vec<Ident>> {
        let Some(key) = self.key_for_type(source) else {
            return vec![];
        };
        let Some(aggregate) = self.aggregate(&key) else {
            return vec![];
        };
        let target = CanonicalTypeKey(target.clone());
        aggregate
            .fields
            .iter()
            .filter_map(|(name, field)| {
                let embed = field.embed.as_ref()?;
                if embed.as_projection {
                    return None;
                }
                let field_ty = substitute_aggregate_member(source, &aggregate.generics, &field.ty);
                (CanonicalTypeKey(field_ty) == target).then(|| vec![name])
            })
            .collect()
    }

    pub(crate) fn aggregates(&self) -> impl Iterator<Item = (&NominalKey, &AggregateSchema)> {
        self.aggregates.iter()
    }

    pub(crate) fn enum_schema(&self, key: &NominalKey) -> Option<&EnumSchema> {
        let schema = self.enums.get(key)?;
        debug_assert!(self.enum_schema_invariants_hold(key, schema));
        Some(schema)
    }

    fn enum_schema_invariants_hold(&self, key: &NominalKey, schema: &EnumSchema) -> bool {
        if !schema.variants.invariants_hold()
            || self.enum_repr_for_key(key) != Some(schema.repr)
            || self.enum_repr_for_type(&nominal_type(key)) != Some(schema.repr)
        {
            return false;
        }
        match (schema.repr, self.raw_enum_raw_type(key)) {
            (EnumRepr::Adt, None)
            | (EnumRepr::RawInt, Some(Type::Int))
            | (EnumRepr::RawString, Some(Type::String)) => {}
            _ => return false,
        }
        let mut raw_values = HashSet::new();
        for (slot, (name, variant)) in schema.variants.iter().enumerate() {
            if self.enum_variant_index(key, name) != Some(slot) {
                return false;
            }
            match (schema.repr, variant.raw_value.as_ref()) {
                (EnumRepr::Adt, None) => {}
                (EnumRepr::RawInt, Some(raw @ RawEnumValue::Int(_)))
                | (EnumRepr::RawString, Some(raw @ RawEnumValue::String(_))) => {
                    if !matches!(variant.payload, VariantPayload::Unit) || !raw_values.insert(raw) {
                        return false;
                    }
                }
                _ => return false,
            }
            if let VariantPayload::Struct(fields) = &variant.payload
                && (!fields.invariants_hold()
                    || !fields.iter().enumerate().all(|(slot, (field, _))| {
                        self.enum_struct_field_index(key, name, field) == Some(slot)
                    }))
            {
                return false;
            }
        }
        match schema.repr {
            EnumRepr::Adt => self.raw_enum_values(key).is_none(),
            EnumRepr::RawInt | EnumRepr::RawString => {
                self.raw_enum_values(key).is_some_and(|mut values| {
                    values.all(|(name, value)| {
                        self.raw_enum_value(key, name) == Some(value)
                            && self.raw_enum_variant_from_value(key, value).is_some()
                    })
                })
            }
        }
    }

    pub(crate) fn aggregate_fields(&self, key: &NominalKey) -> Option<&NamedSchemas<FieldSchema>> {
        self.aggregates.get(key).map(|schema| &schema.fields)
    }

    pub(crate) fn enum_variants(&self, key: &NominalKey) -> Option<&NamedSchemas<VariantSchema>> {
        self.enums.get(key).map(|schema| &schema.variants)
    }

    pub(crate) fn enum_repr_for_key(&self, key: &NominalKey) -> Option<EnumRepr> {
        Some(self.enums.get(key)?.repr)
    }

    pub(crate) fn enum_repr_for_type(&self, ty: &Type) -> Option<EnumRepr> {
        self.enum_repr_for_key(&self.key_for_type(ty)?)
    }

    pub(crate) fn raw_enum_raw_type(&self, key: &NominalKey) -> Option<Type> {
        match self.enum_repr_for_key(key)? {
            EnumRepr::Adt => None,
            EnumRepr::RawInt => Some(Type::Int),
            EnumRepr::RawString => Some(Type::String),
        }
    }

    pub(crate) fn raw_enum_value(&self, key: &NominalKey, variant: Ident) -> Option<&RawEnumValue> {
        if self.enum_repr_for_key(key)? == EnumRepr::Adt {
            return None;
        }
        self.enums
            .get(key)?
            .variants
            .get(variant)?
            .raw_value
            .as_ref()
    }

    pub(crate) fn raw_enum_variant_from_value(
        &self,
        key: &NominalKey,
        value: &RawEnumValue,
    ) -> Option<Ident> {
        if self.enum_repr_for_key(key)? == EnumRepr::Adt {
            return None;
        }
        self.enums
            .get(key)?
            .variants
            .iter()
            .find_map(|(name, variant)| (variant.raw_value.as_ref() == Some(value)).then_some(name))
    }

    pub(crate) fn raw_enum_values(
        &self,
        key: &NominalKey,
    ) -> Option<impl Iterator<Item = (Ident, &RawEnumValue)>> {
        if self.enum_repr_for_key(key)? == EnumRepr::Adt {
            return None;
        }
        Some(
            self.enums
                .get(key)?
                .variants
                .iter()
                .filter_map(|(name, variant)| Some((name, variant.raw_value.as_ref()?))),
        )
    }

    pub(crate) fn enum_struct_fields(
        &self,
        key: &NominalKey,
        variant: Ident,
    ) -> Option<&NamedSchemas<FieldSchema>> {
        let variant = self.enums.get(key)?.variants.get(variant)?;
        match &variant.payload {
            VariantPayload::Struct(fields) => Some(fields),
            VariantPayload::Unit | VariantPayload::Tuple(_) => None,
        }
    }

    pub(crate) fn aggregate_field_index(&self, key: &NominalKey, field: Ident) -> Option<usize> {
        self.aggregate_fields(key)?.slot(field)
    }

    pub(crate) fn enum_variant_index(&self, key: &NominalKey, variant: Ident) -> Option<usize> {
        self.enum_variants(key)?.slot(variant)
    }

    pub(crate) fn enum_struct_field_index(
        &self,
        key: &NominalKey,
        variant: Ident,
        field: Ident,
    ) -> Option<usize> {
        self.enum_struct_fields(key, variant)?.slot(field)
    }

    pub(crate) fn enums(&self) -> impl Iterator<Item = (&NominalKey, &EnumSchema)> {
        self.enums.iter()
    }

    pub(super) fn take_pending_raw_enums(&mut self) -> Vec<(NominalKey, PendingRawEnum)> {
        let mut pending = self.pending_raw_enums.drain().collect::<Vec<_>>();
        pending.sort_by_key(|(key, _)| nominal_key_sort_key(key));
        pending
    }

    pub(super) fn sanitize_raw_enum(&mut self, key: &NominalKey) {
        let Some(schema) = self.enums.get_mut(key) else {
            return;
        };
        schema.repr = EnumRepr::Adt;
        for variant in schema.variants.values_mut() {
            variant.raw_value = None;
        }
    }

    pub(super) fn install_raw_enum_metadata(
        &mut self,
        key: &NominalKey,
        metadata: VerifiedRawEnumMetadata,
    ) -> bool {
        let Some(schema) = self.enums.get_mut(key) else {
            return false;
        };
        debug_assert_ne!(metadata.repr, EnumRepr::Adt);
        debug_assert_eq!(metadata.values.len(), schema.variants.entries.len());
        schema.repr = metadata.repr;
        for variant in schema.variants.values_mut() {
            variant.raw_value = None;
        }
        for (name, raw) in metadata.values {
            if let Some(variant) = schema.variants.get_mut(name) {
                variant.raw_value = Some(raw);
            }
        }
        true
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

    pub(crate) fn contract(&self, key: &ContractKey) -> Option<&ContractSchema> {
        self.contracts.get(key)
    }

    pub(crate) fn contracts(&self) -> impl Iterator<Item = &ContractSchema> {
        self.contracts.values()
    }

    pub(crate) fn contracts_mut(&mut self) -> impl Iterator<Item = &mut ContractSchema> {
        self.contracts.values_mut()
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
        let key = nominal_key_for_type(ty)?;
        self.local_nominal_type(&key.module, key.name)
            .filter(|found| found.kind == key.kind)
    }

    pub(crate) fn core_result_key(&self) -> Option<NominalKey> {
        self.core_enum_key("result", "Result")
    }

    pub(crate) fn core_result_of(&self, ok: Type, err: Type) -> Option<Type> {
        let key = self.core_result_key()?;
        Some(nominal_type_with_args(&key, &[ok, err], &[]))
    }

    pub(crate) fn core_option_of(&self, inner: Type) -> Option<Type> {
        let key = self.core_option_key()?;
        Some(nominal_type_with_args(&key, &[inner], &[]))
    }

    pub(crate) fn single_nominal_type_arg<'a>(ty: &'a Type, key: &NominalKey) -> Option<&'a Type> {
        let Type::Nominal(nominal) = ty else {
            return None;
        };
        if nominal_key_for_type(ty)? != *key || !nominal.const_args.is_empty() {
            return None;
        }
        let [inner] = nominal.type_args.as_slice() else {
            return None;
        };
        Some(inner)
    }

    pub(crate) fn semantic_option_parts<'a>(&self, ty: &'a Type) -> Option<(NominalKey, &'a Type)> {
        let key = self.core_option_key()?;
        let inner = Self::single_nominal_type_arg(ty, &key)?;
        Some((key, inner))
    }

    pub(crate) fn semantic_result_parts<'a>(
        &self,
        ty: &'a Type,
    ) -> Option<(NominalKey, &'a Type, &'a Type)> {
        let key = self.core_result_key()?;
        let Type::Nominal(nominal) = ty else {
            return None;
        };
        if nominal_key_for_type(ty)? != key || !nominal.const_args.is_empty() {
            return None;
        }
        let [ok, err] = nominal.type_args.as_slice() else {
            return None;
        };
        Some((key, ok, err))
    }

    pub(crate) fn semantic_option_key(&self, ty: &Type) -> Option<NominalKey> {
        self.semantic_option_parts(ty).map(|(key, _)| key)
    }

    pub(crate) fn semantic_option_inner<'a>(&self, ty: &'a Type) -> Option<&'a Type> {
        self.semantic_option_parts(ty).map(|(_, inner)| inner)
    }

    pub(crate) fn expect_core_option_of(&self, inner: Type) -> Type {
        self.core_option_of(inner)
            .expect("core option declaration must be loaded")
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
        let key = self.core_range_key(self.core_range_kind(ty)?)?;
        Self::single_nominal_type_arg(ty, &key)
    }

    pub(crate) fn map_key_error(&self, ty: &Type) -> Option<MapKeyError> {
        self.map_key_error_inner(ty, &mut HashSet::new())
    }

    fn map_key_error_inner(&self, ty: &Type, seen: &mut HashSet<Type>) -> Option<MapKeyError> {
        if self.semantic_option_inner(ty).is_some() || self.semantic_result_parts(ty).is_some() {
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
            | Type::Dyn(_)
            | Type::List { .. }
            | Type::Array { .. }
            | Type::Map { .. }
            | Type::Slice { .. }
            | Type::Optional { .. } => Some(MapKeyError {
                ty: ty.clone(),
                field: None,
            }),
        }
    }

    fn nominal_map_key_error(&self, ty: &Type, seen: &mut HashSet<Type>) -> Option<MapKeyError> {
        let key = self.key_for_type(ty)?;
        if !seen.insert(ty.clone()) {
            return None;
        }
        let err = match key.kind {
            NominalKind::Struct => self.struct_map_key_error(ty, &key, seen),
            NominalKind::Enum => self.enum_map_key_error(ty, &key, seen),
            NominalKind::DataRef | NominalKind::Extern => Some(MapKeyError {
                ty: ty.clone(),
                field: None,
            }),
        };
        seen.remove(ty);
        err
    }

    fn struct_map_key_error(
        &self,
        ty: &Type,
        key: &NominalKey,
        seen: &mut HashSet<Type>,
    ) -> Option<MapKeyError> {
        let agg = self.aggregate(key)?;
        agg.fields.iter().find_map(|(name, field)| {
            self.member_map_key_error(ty, &agg.generics, &field.ty, Some(name), seen)
        })
    }

    fn enum_map_key_error(
        &self,
        ty: &Type,
        key: &NominalKey,
        seen: &mut HashSet<Type>,
    ) -> Option<MapKeyError> {
        let schema = self.enum_schema(key)?;
        schema
            .variants
            .values()
            .find_map(|variant| match &variant.payload {
                VariantPayload::Unit => None,
                VariantPayload::Tuple(fields) => fields.iter().find_map(|field_ty| {
                    self.member_map_key_error(ty, &schema.generics, field_ty, None, seen)
                }),
                VariantPayload::Struct(fields) => fields.iter().find_map(|(name, field)| {
                    self.member_map_key_error(ty, &schema.generics, &field.ty, Some(name), seen)
                }),
            })
    }

    fn member_map_key_error(
        &self,
        owner: &Type,
        generics: &GenericParams,
        ty: &Type,
        field: Option<Ident>,
        seen: &mut HashSet<Type>,
    ) -> Option<MapKeyError> {
        let ty = substitute_aggregate_member(owner, generics, ty);
        let mut err = self.map_key_error_inner(&ty, seen)?;
        if field.is_some() {
            err.field = field;
        }
        Some(err)
    }

    pub(crate) fn core_option_key(&self) -> Option<NominalKey> {
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
        let field = agg.fields.get(name)?;
        Some(substitute_aggregate_member(
            receiver,
            &agg.generics,
            &field.ty,
        ))
    }

    pub(crate) fn extends(&self) -> impl Iterator<Item = &ExtendSchema> {
        self.extends.iter()
    }

    pub(crate) fn extend(&self, id: &ExtendId) -> Option<&ExtendSchema> {
        self.extends.iter().find(|extend| extend.id == *id)
    }

    pub(crate) fn extend_mut(&mut self, id: &ExtendId) -> Option<&mut ExtendSchema> {
        self.extends.iter_mut().find(|extend| extend.id == *id)
    }

    pub(crate) fn find_cast_conversion(
        &self,
        source: &Type,
        target: &Type,
        visible: impl Fn(&ExtendSchema) -> bool,
    ) -> Option<CastConversionMatch> {
        struct CastCandidate<'a> {
            extend: &'a ExtendSchema,
            index: usize,
            args: GenericArgs,
            target: Type,
        }

        let mut candidates = vec![];
        for extend in self.extends.iter().filter(|extend| visible(extend)) {
            let pattern = ExtendTargetPattern::from(extend);
            let Some(target_match) = match_exact_target(self, &pattern, target, false) else {
                continue;
            };
            for (index, cast) in extend.cast_froms.iter().enumerate() {
                let source_template = generic_template_type(&cast.param.ty, &extend.generics);
                let Some(args) = Self::cast_source_args(
                    &extend.generics,
                    &source_template,
                    source,
                    &target_match.owner_args,
                    &target_match.templated_target,
                    target,
                ) else {
                    continue;
                };
                candidates.push(CastCandidate {
                    extend,
                    index,
                    args,
                    target: target_match.templated_target.clone(),
                });
            }
        }

        let target_matches = candidates
            .iter()
            .map(|candidate| MatchedTargetPattern {
                pattern: ExtendTargetPattern::from(candidate.extend),
                target: &candidate.target,
            })
            .collect::<Vec<_>>();
        let Some(winner) = most_specific_target_match(&target_matches) else {
            return (!candidates.is_empty()).then_some(CastConversionMatch::Ambiguous);
        };
        let winner = &candidates[winner];
        let cast = &winner.extend.cast_froms[winner.index];
        Some(CastConversionMatch::Match(CastFromConversion {
            escape: cast.param.escape,
            origin: winner.extend.origin.clone(),
            instance: CastFromInstanceKey {
                extend: winner.extend.id.clone(),
                index: winner.index,
                args: winner.args.clone(),
            },
        }))
    }

    fn cast_source_args(
        generics: &GenericParams,
        source_template: &Type,
        source: &Type,
        target_args: &Result<GenericArgs, Vec<Ident>>,
        target_template: &Type,
        target: &Type,
    ) -> Option<GenericArgs> {
        if let Ok(args) = target_args {
            let (types, consts) = generics.substitutions(args);
            let source_template = substitute(source_template, &types, &consts);
            return (source_template == *source).then(|| args.clone());
        }

        match_cast_conversion(generics, source_template, source, target_template, target)?.ok()
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
            ext.exported
                && ext.methods.contains_key(&MethodKey::instance(name))
                && self.module_surface_contains(module, &ext.origin)
        })
    }

    pub(crate) fn find_extend_method_in_module_surface(
        &self,
        module: &ModuleScope,
        receiver: &Type,
        name: Ident,
    ) -> Option<ExtendMethodMatch<'_>> {
        self.find_extend_method(MethodSurface::Instance, receiver, name, |ext| {
            ext.exported && self.module_surface_contains(module, &ext.origin)
        })
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
                    default_sites: sig.default_sites.clone(),
                    required_params: sig.required_params,
                    ret: ret.as_ref().clone(),
                },
            },
            receiver_ty: None,
            owner_args: GenericArgs::default(),
            is_stringify_override: false,
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
            |(type_subst, const_subst)| {
                substitute_return_spec(&method.ret, type_subst, const_subst)
            },
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
                    default_sites: method.default_sites.clone(),
                    required_params: method.required_params,
                    ret,
                },
            },
            receiver_ty,
            owner_args: owner_ty.and_then(nominal_generic_args).unwrap_or_default(),
            is_stringify_override: name == Ident::new("to_string")
                && method.is_stringify_override(),
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
            .map(|param| param.map_ty(|ty| generic_template_type(ty, &extend.generics)))
            .collect::<Vec<_>>();
        let template_ret = method
            .ret
            .with_ty(generic_template_type(&method.ret.ty, &extend.generics));

        CallableRef {
            def: CallableDef {
                id: CallableId::extend_method(extend.id.clone(), name, method.mode.surface()),
                sig: CallableSig {
                    owner_generics: extend.generics.clone(),
                    generics: method.generics.clone(),
                    params: substitute_func_params(&template_params, &type_subst, &const_subst),
                    default_sites: method.default_sites.clone(),
                    required_params: method.required_params,
                    ret: substitute_return_spec(&template_ret, &type_subst, &const_subst),
                },
            },
            receiver_ty,
            owner_args,
            is_stringify_override: false,
        }
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
    let owner = owner_ty.as_nominal()?;
    let type_subst: TypeSubst = generics
        .type_params
        .iter()
        .zip(owner.type_args.iter())
        .map(|(param, arg)| (param.id, arg.clone()))
        .collect();
    let const_subst: ConstSubst = generics
        .const_params
        .iter()
        .zip(owner.const_args.iter())
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
        .map(|param| param.map_ty(|ty| substitute(ty, type_subst, const_subst)))
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

impl DeclarationIndex {
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
    module: &ModuleScope,
    type_params: &[TypeParam],
    const_params: &[ConstParam],
    span: Span,
    errors: &mut Vec<GenericContextError>,
) -> GenericTypeContext {
    generic_context_from_params(type_params, const_params, |error| {
        errors.push(GenericContextError {
            module: module.clone(),
            error,
            span,
        });
    })
}

fn extend_generic_context(
    module: &ModuleScope,
    owner: &GenericTypeContext,
    type_params: &[TypeParam],
    const_params: &[ConstParam],
    span: Span,
    errors: &mut Vec<GenericContextError>,
) -> GenericTypeContext {
    extend_generic_context_with_params(owner, type_params, const_params, |error| {
        errors.push(GenericContextError {
            module: module.clone(),
            error,
            span,
        });
    })
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

pub(crate) fn generic_params(
    type_params: &[TypeParam],
    const_params: &[ConstParam],
) -> GenericParams {
    GenericParams {
        type_params: type_params.to_vec(),
        const_params: const_params.to_vec(),
    }
}

fn fold_func_type_uses<F>(
    site: DeclTypeSite,
    ty: Type,
    param_spans: &ParamTypeSpans,
    f: &mut F,
) -> Type
where
    F: FnMut(DeclTypeSite, Type) -> Type,
{
    let Type::Func { params, ret } = ty else {
        return f(site, ty);
    };
    let param_kind = match site.kind {
        DeclTypeUseKind::ExternFunctionReturn => DeclTypeUseKind::ExternFunctionParam,
        _ => DeclTypeUseKind::FunctionParam,
    };
    let ret_kind = site.kind;
    let params = params
        .into_iter()
        .enumerate()
        .map(|(index, param)| {
            let span = param_spans.span_for(index, site.span);
            let ty = f(
                DeclTypeSite {
                    span,
                    kind: param_kind,
                    ..site.clone()
                },
                param.ty.clone(),
            );
            param.with_ty(ty)
        })
        .collect();
    let ret_ty = f(
        DeclTypeSite {
            kind: ret_kind,
            ..site.clone()
        },
        ret.ty.clone(),
    );
    Type::func(params, ret.with_ty(ret_ty))
}

fn method_schema(
    source: SourceId,
    sig: &MethodSig,
    mode: MethodMode,
    policy: AccessPolicy,
) -> MethodSchema {
    MethodSchema {
        mode,
        generics: generic_params(&sig.type_params, &sig.const_params),
        params: resolve_func_params(&sig.params),
        param_spans: ParamTypeSpans::from_params(&sig.params),
        default_sites: ParamDefaultSite::from_params(source, &sig.params),
        required_params: required_param_count(&sig.params),
        ret: sig.ret.clone(),
        policy,
    }
}

pub(crate) fn func_type_from_params(params: &[Param], ret: &ReturnSpec) -> Type {
    Type::func(resolve_func_params(params), ret.clone())
}

pub(crate) fn resolve_func_params(params: &[Param]) -> Vec<FuncParam> {
    params.iter().map(Param::func_param).collect()
}

pub(crate) fn substitute_return_spec(
    ret: &ReturnSpec,
    type_subst: &TypeSubst,
    const_subst: &ConstSubst,
) -> ReturnSpec {
    ret.with_ty(substitute(&ret.ty, type_subst, const_subst))
}

pub(crate) fn required_param_count(params: &[Param]) -> usize {
    params
        .iter()
        .position(|param| param.default.is_some())
        .unwrap_or(params.len())
}

pub(super) fn nominal_key_sort_key(key: &NominalKey) -> String {
    format!("{:?}:{:?}:{}", key.module, key.kind, key.name)
}

fn embed_field_schema(
    field: &ast::StructField,
    as_projection: bool,
    source: SourceId,
) -> Option<EmbedFieldSchema> {
    let embed = field.embed.as_ref()?;
    Some(EmbedFieldSchema {
        selector: embed.selector.clone(),
        exposure: Exposure::from_selector(embed.selector.is_some()),
        as_projection,
        span: SourceSpan::from_byte_span(source, field.span),
    })
}

fn validate_embed_field_schemas(fields: &NamedSchemas<FieldSchema>, errors: &mut Vec<DeclError>) {
    for field in fields.values() {
        let Some(embed) = &field.embed else {
            continue;
        };
        debug_assert_eq!(field.span, Some(embed.span));
        match embed.exposure {
            Exposure::Explicit => debug_assert!(embed.selector.is_some()),
            Exposure::Implicit => debug_assert!(embed.selector.is_none()),
        }
        if embed.as_projection {
            debug_assert!(field.span.is_some());
        }
        if let Some(selector) = &embed.selector {
            validate_embed_selector(selector, embed.span, errors);
        }
    }
}

fn validate_embed_selector(
    selector: &ast::EmbedSelector,
    span: SourceSpan,
    errors: &mut Vec<DeclError>,
) {
    if selector.items.is_empty() {
        errors.push(DeclError::EmptyEmbedSelector { span: Some(span) });
        return;
    }

    let mut fields = HashSet::new();
    let mut methods = HashSet::new();
    for item in &selector.items {
        let exposed = item.alias.unwrap_or(item.name);
        let seen = match item.kind {
            ast::EmbedSelectorKind::Field => &mut fields,
            ast::EmbedSelectorKind::Method => &mut methods,
        };
        if !seen.insert(exposed) {
            errors.push(DeclError::DuplicateEmbedSelector {
                name: exposed,
                kind: item.kind,
                span: Some(span),
            });
        }
    }
}

pub(crate) fn stmt_visibility(stmt: &StmtNode) -> Visibility {
    match &stmt.node {
        Stmt::ExternFunc(n) => n.node.visibility,
        Stmt::ExternType(n) => n.node.visibility,
        Stmt::Extend(n) => n.node.visibility,
        Stmt::Func(n) => n.node.visibility,
        Stmt::Aggregate(n) => n.node.visibility,
        Stmt::Enum(n) => n.node.visibility,
        Stmt::Const(n) => n.node.visibility,
        Stmt::Global(n) => n.node.visibility,
        Stmt::TypeAlias(n) => n.node.visibility,
        Stmt::Contract(n) => n.node.visibility,
        _ => Visibility::Private,
    }
}

pub(crate) fn nominal_key_for_type(ty: &Type) -> Option<NominalKey> {
    let Type::Nominal(nominal) = ty else {
        return None;
    };
    Some(NominalKey {
        module: nominal
            .origin
            .as_ref()
            .map_or(ModuleScope::Root, ModuleScope::from_nominal_origin),
        kind: nominal.kind,
        name: nominal.name,
    })
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
        CallbackEscape, ExternEffects, ExternFunctionDescriptor, ExternModuleDescriptor,
        ExternParam, ExternRep, ExternSignature, ExternTypeDescriptor, ExternTypeExpr,
        ModulePath as ExternModulePath, ParamFlow, ProviderDescriptor, ProviderId,
    };

    use super::*;
    use crate::test_support::{
        ident, module_path_segments, parse_program, resolved_modules,
        resolved_modules_with_external,
    };

    fn scope(name: &str) -> ModuleScope {
        ModuleScope::Named(module_path_segments(&[name]))
    }

    fn provider_scope(name: &str) -> ModuleScope {
        ModuleScope::from_module_id(&ModuleId::provider(
            PackageId::synthetic_root(),
            module_path_segments(&[name]),
        ))
    }

    fn index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse_program(root);
        let resolved = resolved_modules(&root, modules);
        let externs = crate::externs::collect_source_externs(&root, &resolved).unwrap();
        DeclarationIndex::from_root_and_modules(&root, &resolved, &externs)
    }

    fn checked_index(root: &str, modules: &[(&str, &str)]) -> DeclarationIndex {
        let root = parse_program(root);
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

    fn root_key(kind: NominalKind, name: &str) -> NominalKey {
        NominalKey {
            module: ModuleScope::Root,
            kind,
            name: ident(name),
        }
    }

    #[test]
    fn aggregate_fields_keep_decl_order() {
        let index = checked_index("struct Weird { z: int, a: int, m: int }", &[]);
        let key = root_key(NominalKind::Struct, "Weird");
        let fields = index.aggregate_fields(&key).expect("aggregate fields");
        assert_eq!(
            fields.names().collect::<Vec<_>>(),
            vec![ident("z"), ident("a"), ident("m")]
        );
        assert_eq!(index.aggregate_field_index(&key, ident("z")), Some(0));
        assert_eq!(index.aggregate_field_index(&key, ident("a")), Some(1));
        assert_eq!(index.aggregate_field_index(&key, ident("m")), Some(2));
    }

    #[test]
    fn enum_variants_and_struct_fields_keep_decl_order() {
        let index = checked_index("enum Weird { Z, A { b: int, a: string }, M }", &[]);
        let key = root_key(NominalKind::Enum, "Weird");
        let variants = index.enum_variants(&key).expect("enum variants");
        assert_eq!(
            variants.names().collect::<Vec<_>>(),
            vec![ident("Z"), ident("A"), ident("M")]
        );
        assert_eq!(index.enum_variant_index(&key, ident("A")), Some(1));
        let fields = index
            .enum_struct_fields(&key, ident("A"))
            .expect("variant fields");
        assert_eq!(
            fields.names().collect::<Vec<_>>(),
            vec![ident("b"), ident("a")]
        );
        assert_eq!(
            index.enum_struct_field_index(&key, ident("A"), ident("b")),
            Some(0)
        );
        assert_eq!(
            index.enum_struct_field_index(&key, ident("A"), ident("a")),
            Some(1)
        );
    }

    #[test]
    fn aggregate_defaults_keep_expr_identity() {
        let root = parse_program("struct Defaults { z: int = 1, a: int = 2 }");
        let expected = match &root.stmts[0].node {
            Stmt::Aggregate(aggregate) => {
                aggregate.node.fields[1].default.as_ref().unwrap().node.id
            }
            _ => unreachable!(),
        };
        let resolved = resolved_modules(&root, &[]);
        let externs = crate::externs::collect_source_externs(&root, &resolved).unwrap();
        let mut tc = super::super::typechecker_for_modules(
            &root,
            &resolved,
            externs,
            super::super::TypecheckConfig::default(),
        )
        .expect("typecheck failed");
        tc.finish().expect("typecheck failed");
        let index = tc.decls.clone();
        let key = root_key(NominalKind::Struct, "Defaults");
        let fields = index.aggregate_fields(&key).expect("aggregate fields");
        let default = fields
            .iter()
            .nth(1)
            .and_then(|(_, field)| field.default.as_ref())
            .expect("default");
        assert_eq!(default.expr_id, expected);
    }

    fn provider_index(root: &str, provider: ProviderDescriptor) -> DeclarationIndex {
        provider_index_with_modules(root, &[], provider)
    }

    fn provider_index_with_modules(
        root: &str,
        modules: &[(&str, &str)],
        provider: ProviderDescriptor,
    ) -> DeclarationIndex {
        let root = parse_program(root);
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
                    escape: CallbackEscape::NonEscaping,
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
            variants: vec![],
            init: None,
            methods: vec![],
            statics: vec![],
            operators: vec![],
        }
    }

    fn finalize_type(
        index: &DeclarationIndex,
        module: &ModuleScope,
        ty: &Type,
    ) -> Result<Type, TypeRefError> {
        TypeRefResolver::module_only(index)
            .finalize_at(module, &GenericTypeContext::default(), ty, None)
            .map(|finalized| finalized.ty)
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
        let result = finalize_type(&index, &scope, &ty).unwrap();

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
        let result = finalize_type(&index, &scope, &ty).unwrap();

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
        let error = finalize_type(&index, &ModuleScope::Root, &ty)
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

        let lookup =
            index.resolve_type_member(&ModuleScope::Root, Some(ident("shapes")), ident("Point"));
        let ModuleMemberLookup::Found(TypeBinding::Nominal(key)) = lookup.result else {
            panic!("missing qualified type");
        };

        assert_eq!(key.module, scope("shapes"));
        assert_eq!(key.name, ident("Point"));
        assert_eq!(key.kind, NominalKind::Struct);
    }

    #[test]
    fn qualified_visible_requires_binding() {
        let index = index("", &[("shapes", "pub struct Point { x: int }")]);

        let lookup =
            index.resolve_type_member(&ModuleScope::Root, Some(ident("shapes")), ident("Point"));

        assert!(matches!(lookup.result, ModuleMemberLookup::Missing));
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
    fn alias_module_namespace() {
        let index = index("import facade { self as f };", &[("facade", "")]);

        assert_eq!(
            index.imported_module(&ModuleScope::Root, ident("f")),
            Some(scope("facade")),
        );
    }

    #[test]
    fn import_records_keep_stable_ids_and_metadata() {
        let index = index(
            "import math; pub import tools { * }; import facade as f;",
            &[("math", ""), ("tools", ""), ("facade", "")],
        );
        let records = index.import_records();

        assert_eq!(records.len(), 3);
        assert_eq!(records[0].id.module, ModuleScope::Root);
        assert_eq!(records[0].id.ordinal, 0);
        assert_eq!(records[0].visibility, Visibility::Private);
        assert_eq!(records[0].target_module, scope("math"));
        assert_eq!(records[0].span.byte().start, 0);

        assert_eq!(records[1].id.ordinal, 1);
        assert_eq!(records[1].visibility, Visibility::Public);
        assert_eq!(records[1].target_module, scope("tools"));

        assert_eq!(records[2].id.ordinal, 2);
        assert_eq!(records[2].target_module, scope("facade"));
    }

    #[test]
    fn imported_lookup_returns_import_provenance() {
        let index = index(
            "import math { add, Point }; import facade as f;",
            &[
                (
                    "math",
                    "pub struct Point { x: int } pub fn add() -> int { 1 }",
                ),
                ("facade", ""),
            ],
        );

        let (_, value_import) = index
            .imported_value_with_import(&ModuleScope::Root, ident("add"))
            .expect("missing imported value");
        let (_, type_import) = index
            .imported_type_binding_with_import(&ModuleScope::Root, ident("Point"))
            .expect("missing imported type");
        let (_, module_import) = index
            .imported_module_with_import(&ModuleScope::Root, ident("f"))
            .expect("missing imported module");

        assert_eq!(value_import.unwrap().ordinal, 0);
        assert_eq!(type_import.unwrap().ordinal, 0);
        assert_eq!(module_import.unwrap().ordinal, 1);
    }

    #[test]
    fn reexported_binding_uses_root_import_provenance() {
        let index = index(
            "import facade { dup };",
            &[
                ("tools", "pub fn id() -> int { 1 }"),
                ("facade", "pub import tools { id as dup };"),
            ],
        );

        let (_, import) = index
            .imported_value_with_import(&ModuleScope::Root, ident("dup"))
            .expect("missing imported value");

        let import = import.unwrap();
        assert_eq!(import.module, ModuleScope::Root);
        assert_eq!(import.ordinal, 0);
    }

    #[test]
    fn implicit_core_prelude_creates_no_import_record() {
        let index = checked_index("fn main() {}", &[]);

        assert!(index.import_records().is_empty());
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
    fn direct_import_records_activation_provenance() {
        let index = index("import tools;", &[("tools", "")]);
        let imports = index.active_import_ids(&ModuleScope::Root, &scope("tools"));

        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].ordinal, 0);
    }

    #[test]
    fn wildcard_import_records_activation_provenance() {
        let index = index("import tools { * };", &[("tools", "pub fn f() {}")]);
        let imports = index.active_import_ids(&ModuleScope::Root, &scope("tools"));

        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].ordinal, 0);
    }

    #[test]
    fn transitive_reexport_activation_keeps_root_import_provenance() {
        let index = index(
            "import facade;",
            &[
                ("tools", "pub fn f() {}"),
                ("facade", "pub import tools { * };"),
            ],
        );
        let imports = index.active_import_ids(&ModuleScope::Root, &scope("tools"));

        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].module, ModuleScope::Root);
        assert_eq!(imports[0].ordinal, 0);
    }

    #[test]
    fn duplicate_activators_preserve_all_import_ids() {
        let index = index("import tools; import tools as t;", &[("tools", "")]);
        let imports = index.active_import_ids(&ModuleScope::Root, &scope("tools"));

        assert_eq!(
            imports.iter().map(|id| id.ordinal).collect::<Vec<_>>(),
            [0, 1]
        );
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
                        readable: true,
                        writable: true,
                        get_receiver: anvyx_externs::ReceiverMode::Shared,
                        set_receiver: anvyx_externs::ReceiverMode::Mutable,
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
        assert_eq!(instance_ref.def.sig.ret.ty, Type::Int);
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
            .find_extend_method(MethodSurface::Instance, &Type::Int, ident("id"), |_| true)
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
        assert_eq!(callable.def.sig.ret.ty, Type::Int);
    }

    #[test]
    fn set_const_type_syncs_copies() {
        let mut index = index(
            "import tools { SIZE };",
            &[("tools", "pub const SIZE = 1;")],
        );
        index.set_const_type(&scope("tools"), ident("SIZE"), &Type::Int);

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
