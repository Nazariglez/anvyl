#[cfg(test)]
mod tests;

use std::{
    collections::{HashMap, HashSet},
    fmt,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    ast::{self, Ident, Program, Stmt},
    source::SourceId,
    span::{SourceSpan, Spanned},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PackageId(String);

impl PackageId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    pub fn synthetic_root() -> Self {
        Self::new("<root>")
    }

    pub fn core() -> Self {
        Self::new("<core>")
    }

    pub fn std() -> Self {
        Self::new("<std>")
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for PackageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath(Vec<String>);

impl ModulePath {
    pub fn new(segments: Vec<String>) -> Result<Self, ModulePathError> {
        if segments.is_empty() {
            return Err(ModulePathError::new("module path must not be empty"));
        }
        if segments.iter().any(String::is_empty) {
            return Err(ModulePathError::new(format!(
                "module path must not contain empty segments: {}",
                display_module_path(&segments)
            )));
        }
        Ok(Self(segments))
    }

    fn from_valid_segments(segments: Vec<String>) -> Self {
        Self(segments)
    }

    pub fn from_idents(idents: &[Ident]) -> Self {
        Self::from_valid_segments(idents.iter().map(ToString::to_string).collect())
    }

    pub fn segments(&self) -> &[String] {
        &self.0
    }

    pub fn first_segment(&self) -> Option<&str> {
        self.0.first().map(String::as_ref)
    }

    pub fn to_ast_path(&self) -> ast::ModulePath {
        Rc::from(self.0.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModulePathError {
    message: String,
}

impl ModulePathError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for ModulePathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ModulePathError {}

fn display_module_path(path: &[String]) -> String {
    if path.is_empty() {
        "<empty>".to_string()
    } else {
        path.join(".")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceFileId(PathBuf);

impl SourceFileId {
    pub fn new(path: impl Into<PathBuf>) -> Result<Self, SourceFileIdError> {
        let path = path.into();
        if path.as_os_str().is_empty() {
            return Err(SourceFileIdError::new("source file path must not be empty"));
        }
        if !path.is_absolute() {
            return Err(SourceFileIdError::new(format!(
                "source file path must be absolute: {}",
                path.display()
            )));
        }
        Ok(Self(path))
    }

    pub fn path(&self) -> &Path {
        &self.0
    }
}

impl fmt::Display for SourceFileId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFileIdError {
    message: String,
}

impl SourceFileIdError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for SourceFileIdError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for SourceFileIdError {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PackageModulePath {
    Root,
    Named(ModulePath),
    Provider(ModulePath),
    Source(SourceFileId),
}

impl PackageModulePath {
    pub fn named_path(&self) -> Option<&ModulePath> {
        match self {
            Self::Named(path) => Some(path),
            Self::Root | Self::Provider(_) | Self::Source(_) => None,
        }
    }

    pub fn provider_path(&self) -> Option<&ModulePath> {
        match self {
            Self::Provider(path) => Some(path),
            Self::Root | Self::Named(_) | Self::Source(_) => None,
        }
    }

    pub fn module_path(&self) -> Option<&ModulePath> {
        match self {
            Self::Named(path) | Self::Provider(path) => Some(path),
            Self::Root | Self::Source(_) => None,
        }
    }

    pub fn source_file(&self) -> Option<&SourceFileId> {
        match self {
            Self::Source(file) => Some(file),
            Self::Root | Self::Named(_) | Self::Provider(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    package: Option<PackageId>,
    path: PackageModulePath,
}

impl ModuleId {
    pub fn root(package: PackageId) -> Self {
        Self {
            package: Some(package),
            path: PackageModulePath::Root,
        }
    }

    pub fn named(package: PackageId, path: ModulePath) -> Self {
        Self {
            package: Some(package),
            path: PackageModulePath::Named(path),
        }
    }

    pub fn provider(package: PackageId, path: ModulePath) -> Self {
        Self {
            package: Some(package),
            path: PackageModulePath::Provider(path),
        }
    }

    pub fn source(package: PackageId, file: SourceFileId) -> Self {
        Self::source_with_context(Some(package), file)
    }

    pub fn source_with_context(package: Option<PackageId>, file: SourceFileId) -> Self {
        Self {
            package,
            path: PackageModulePath::Source(file),
        }
    }

    pub fn source_without_package(file: SourceFileId) -> Self {
        Self::source_with_context(None, file)
    }

    pub fn package(&self) -> &PackageId {
        self.package
            .as_ref()
            .expect("source module has no package context")
    }

    pub fn package_context(&self) -> Option<&PackageId> {
        self.package.as_ref()
    }

    pub fn path(&self) -> &PackageModulePath {
        &self.path
    }

    pub fn named_path(&self) -> Option<&ModulePath> {
        self.path.named_path()
    }

    pub fn provider_path(&self) -> Option<&ModulePath> {
        self.path.provider_path()
    }

    pub fn module_path(&self) -> Option<&ModulePath> {
        self.path.module_path()
    }

    pub fn source_file(&self) -> Option<&SourceFileId> {
        self.path.source_file()
    }

    fn is_ignored(&self, ignored_roots: &HashSet<String>) -> bool {
        self.named_path()
            .and_then(ModulePath::first_segment)
            .is_some_and(|s| ignored_roots.contains(s))
    }
}

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub module: ModuleId,
    pub source: SourceId,
    pub program: Program,
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub key: ModuleId,
    pub source: SourceId,
    pub program: Program,
}

type ModuleAliases = HashMap<ModuleId, ModuleId>;
type ImportEdges = HashMap<ModuleId, Vec<ResolvedImportTarget>>;
type PreloadedRoot = (ModuleId, SourceId, Program);

#[derive(Debug, Clone)]
pub struct ResolveResult {
    pub root: ModuleId,
    pub root_source: SourceId,
    pub module_groups: Vec<Vec<ResolvedModule>>,
    pub dependencies: HashMap<PackageId, HashMap<String, PackageId>>,
    pub system: SystemPackages,
    pub module_aliases: ModuleAliases,
    pub(crate) import_edges: ImportEdges,
}

impl ResolveResult {
    pub fn canonical_module<'a>(&'a self, module: &'a ModuleId) -> &'a ModuleId {
        self.module_aliases.get(module).unwrap_or(module)
    }

    pub(crate) fn import_target(
        &self,
        module: &ModuleId,
        ordinal: usize,
    ) -> Option<&ResolvedImportTarget> {
        self.import_edges.get(module)?.get(ordinal)
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SystemPackages {
    pub std: Option<PackageId>,
    pub core: Option<PackageId>,
}

#[derive(Debug, Clone)]
pub struct PreloadedModule {
    pub module: ModuleId,
    pub source: SourceId,
    pub program: Program,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum PackageKind {
    #[default]
    Source,
    NativeOnly,
}

#[derive(Debug, Clone, Default)]
pub struct PackageInput {
    pub root: Option<LoadedModule>,
    pub dependencies: HashMap<String, PackageId>,
    pub kind: PackageKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalSourceRequest {
    pub importer: SourceFileId,
    pub ascend: usize,
    pub path: ModulePath,
}

#[derive(Debug, Clone)]
pub enum LocalSourceLoad {
    Loaded(LoadedModule),
    Missing { candidate: Option<PathBuf> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResolvedImportBaseKind {
    Source,
    Provider,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ResolvedImportTarget {
    pub(crate) base: ModuleId,
    pub(crate) exported_path: Vec<Ident>,
    pub(crate) default_name: Ident,
    pub(crate) base_kind: ResolvedImportBaseKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    ModuleNotFound {
        module: ModuleId,
        span: SourceSpan,
    },
    SourceImportNotFound {
        importer: SourceFileId,
        path: ModulePath,
        candidate: Option<PathBuf>,
        span: SourceSpan,
    },
    LoadFailed {
        module: ModuleId,
        span: SourceSpan,
        message: String,
    },
    DuplicatePreloadedModule {
        module: ModuleId,
    },
    UnknownDependency {
        package: PackageId,
        alias: String,
        span: SourceSpan,
    },
    PackageImportUnavailable {
        file: SourceFileId,
        alias: String,
        span: SourceSpan,
    },
    UnsupportedImportRoot {
        root: &'static str,
        span: SourceSpan,
    },
    NativeProviderUnavailable {
        package: Option<PackageId>,
        span: SourceSpan,
    },
    UnknownNativeProviderModule {
        package: PackageId,
        module: ModulePath,
        span: SourceSpan,
    },
    UnknownNativeDepProviderModule {
        package: PackageId,
        alias: String,
        module: ModulePath,
        span: SourceSpan,
    },
    NativeOnlyPkgRootImport {
        package: PackageId,
        alias: String,
        span: SourceSpan,
    },
}

impl ResolveError {
    #[must_use]
    pub fn span(&self) -> Option<SourceSpan> {
        match self {
            Self::ModuleNotFound { span, .. }
            | Self::SourceImportNotFound { span, .. }
            | Self::LoadFailed { span, .. }
            | Self::UnknownDependency { span, .. }
            | Self::PackageImportUnavailable { span, .. }
            | Self::UnsupportedImportRoot { span, .. }
            | Self::NativeProviderUnavailable { span, .. }
            | Self::UnknownNativeProviderModule { span, .. }
            | Self::UnknownNativeDepProviderModule { span, .. }
            | Self::NativeOnlyPkgRootImport { span, .. } => Some(*span),
            Self::DuplicatePreloadedModule { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleLoadError<E> {
    LoadFailed(String),
    Fatal(E),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveFailure<E> {
    Resolve(Vec<ResolveError>),
    Fatal(E),
}

pub trait ModuleLoader {
    type FatalError;

    fn load(
        &mut self,
        module: &ModuleId,
    ) -> Result<Option<LoadedModule>, ModuleLoadError<Self::FatalError>>;

    fn load_local_source(
        &mut self,
        _request: &LocalSourceRequest,
    ) -> Result<LocalSourceLoad, ModuleLoadError<Self::FatalError>> {
        Ok(LocalSourceLoad::Missing { candidate: None })
    }
}

pub fn resolve_modules<L: ModuleLoader>(
    root: LoadedModule,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut L,
    ignored_roots: &HashSet<String>,
    external_modules: &HashSet<ModulePath>,
) -> Result<ResolveResult, ResolveFailure<L::FatalError>> {
    let root_package = root.module.package().clone();
    let packages = HashMap::from([(root_package.clone(), PackageInput::default())]);
    let external_modules = external_modules
        .iter()
        .cloned()
        .map(|path| ModuleId::provider(root_package.clone(), path))
        .collect::<HashSet<_>>();
    resolve_package_modules(
        root,
        &packages,
        preloaded_modules,
        loader,
        ignored_roots,
        &external_modules,
        SystemPackages::default(),
    )
}

pub fn resolve_package_modules<L: ModuleLoader>(
    root: LoadedModule,
    packages: &HashMap<PackageId, PackageInput>,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut L,
    ignored_roots: &HashSet<String>,
    external_modules: &HashSet<ModuleId>,
    system: SystemPackages,
) -> Result<ResolveResult, ResolveFailure<L::FatalError>> {
    let preloaded =
        prepare_preloaded_modules(preloaded_modules).map_err(ResolveFailure::Resolve)?;
    let root_id = root.module.clone();
    let dependencies = package_dependencies(packages);
    let mut resolver = Resolver {
        root: root_id.clone(),
        packages,
        dependencies: &dependencies,
        loader,
        ignored_roots,
        external_modules,
        std_package: system.std.as_ref(),
        preloaded: preloaded
            .iter()
            .map(|(module, source, program)| (module.clone(), (*source, program.clone())))
            .collect(),
        visiting: HashSet::new(),
        loaded: HashSet::new(),
        modules: vec![],
        module_aliases: HashMap::new(),
        import_edges: HashMap::new(),
        errors: vec![],
        fatal: None,
    };

    for (module, source, program) in preloaded {
        resolver.resolve_module(module, source, program);
    }
    resolver.resolve_module(root_id.clone(), root.source, root.program);

    if let Some(error) = resolver.fatal {
        return Err(ResolveFailure::Fatal(error));
    }
    if !resolver.errors.is_empty() {
        return Err(ResolveFailure::Resolve(resolver.errors));
    }

    let (modules, module_aliases, import_edges) = resolver.into_parts();
    let module_groups = build_dependency_groups(modules, &module_aliases, &import_edges);
    Ok(ResolveResult {
        root: root.module,
        root_source: root.source,
        module_groups,
        dependencies,
        system,
        module_aliases,
        import_edges,
    })
}

fn package_dependencies(
    packages: &HashMap<PackageId, PackageInput>,
) -> HashMap<PackageId, HashMap<String, PackageId>> {
    packages
        .iter()
        .map(|(id, package)| (id.clone(), package.dependencies.clone()))
        .collect()
}

fn prepare_preloaded_modules(
    modules: Vec<PreloadedModule>,
) -> Result<Vec<PreloadedRoot>, Vec<ResolveError>> {
    let mut seen = HashSet::new();
    let mut roots = vec![];
    let mut errors = vec![];

    for module in modules {
        if !seen.insert(module.module.clone()) {
            errors.push(ResolveError::DuplicatePreloadedModule {
                module: module.module,
            });
            continue;
        }
        roots.push((module.module, module.source, module.program));
    }

    if errors.is_empty() {
        Ok(roots)
    } else {
        Err(errors)
    }
}

struct Resolver<'a, L: ModuleLoader> {
    root: ModuleId,
    packages: &'a HashMap<PackageId, PackageInput>,
    dependencies: &'a HashMap<PackageId, HashMap<String, PackageId>>,
    loader: &'a mut L,
    ignored_roots: &'a HashSet<String>,
    external_modules: &'a HashSet<ModuleId>,
    std_package: Option<&'a PackageId>,
    preloaded: HashMap<ModuleId, (SourceId, Program)>,
    visiting: HashSet<ModuleId>,
    loaded: HashSet<ModuleId>,
    modules: Vec<ResolvedModule>,
    module_aliases: ModuleAliases,
    import_edges: ImportEdges,
    errors: Vec<ResolveError>,
    fatal: Option<L::FatalError>,
}

impl<L: ModuleLoader> Resolver<'_, L> {
    fn resolve_module(&mut self, key: ModuleId, source: SourceId, program: Program) {
        if self.fatal.is_some() || self.loaded.contains(&key) || self.visiting.contains(&key) {
            return;
        }

        self.visiting.insert(key.clone());

        let mut edges = vec![];
        for import in import_nodes(&program) {
            let span = SourceSpan::new(source, import.span.start, import.span.end);
            let Some(target) = self.resolve_module_import(&key, import, span) else {
                continue;
            };
            edges.push(target.clone());
            self.resolve_import_base(&target, span);
        }
        self.import_edges.insert(key.clone(), edges);

        self.visiting.remove(&key);
        self.loaded.insert(key.clone());
        self.modules.push(ResolvedModule {
            key,
            source,
            program,
        });
    }

    fn resolve_module_import(
        &mut self,
        current: &ModuleId,
        import: &Spanned<ast::Import>,
        span: SourceSpan,
    ) -> Option<ResolvedImportTarget> {
        if let ast::ImportRoot::Local { ascend } = &import.node.target.root
            && let Some(importer) = current.source_file()
        {
            return self.resolve_local_source_import(importer.clone(), *ascend, import, span);
        }
        resolve_import_target(
            current,
            import,
            span,
            self.packages,
            self.dependencies,
            self.std_package,
            self.external_modules,
            &mut self.errors,
        )
    }

    fn resolve_local_source_import(
        &mut self,
        importer: SourceFileId,
        ascend: usize,
        import: &Spanned<ast::Import>,
        span: SourceSpan,
    ) -> Option<ResolvedImportTarget> {
        let (_, path) = import.node.target.local_path()?;
        let path = ModulePath::from_idents(path);
        let default_name = path_default_name(&path);
        let request = LocalSourceRequest {
            importer,
            ascend,
            path,
        };
        match self.loader.load_local_source(&request) {
            Ok(LocalSourceLoad::Loaded(loaded)) => {
                let module = loaded.module.clone();
                self.resolve_module(loaded.module, loaded.source, loaded.program);
                Some(ResolvedImportTarget {
                    base: module,
                    exported_path: vec![],
                    default_name,
                    base_kind: ResolvedImportBaseKind::Source,
                })
            }
            Ok(LocalSourceLoad::Missing { candidate }) => {
                self.errors.push(ResolveError::SourceImportNotFound {
                    importer: request.importer,
                    path: request.path,
                    candidate,
                    span,
                });
                None
            }
            Err(ModuleLoadError::LoadFailed(message)) => {
                self.errors.push(ResolveError::LoadFailed {
                    module: ModuleId::source_without_package(request.importer),
                    span,
                    message,
                });
                None
            }
            Err(ModuleLoadError::Fatal(error)) => {
                self.fatal = Some(error);
                None
            }
        }
    }

    fn resolve_import_base(&mut self, target: &ResolvedImportTarget, span: SourceSpan) {
        let import_key = target.base.clone();
        if target.base_kind == ResolvedImportBaseKind::Provider {
            if self.external_modules.contains(&import_key) {
                self.loaded.insert(import_key);
            } else {
                self.errors.push(ResolveError::ModuleNotFound {
                    module: import_key,
                    span,
                });
            }
            return;
        }
        if import_key.is_ignored(self.ignored_roots)
            || self.loaded.contains(&import_key)
            || self.visiting.contains(&import_key)
        {
            return;
        }
        if self
            .module_aliases
            .get(&import_key)
            .is_some_and(|module| self.loaded.contains(module) || self.visiting.contains(module))
        {
            return;
        }

        if let Some((source, module_program)) = self.preloaded.get(&import_key).cloned() {
            self.resolve_module(import_key, source, module_program);
            return;
        }

        if matches!(import_key.path(), PackageModulePath::Root) {
            match self
                .packages
                .get(import_key.package())
                .and_then(|package| package.root.clone())
            {
                Some(loaded) => {
                    self.module_aliases
                        .insert(import_key.clone(), loaded.module.clone());
                    self.resolve_module(loaded.module, loaded.source, loaded.program);
                }
                None if import_key == self.root => {}
                None => self.errors.push(ResolveError::ModuleNotFound {
                    module: import_key,
                    span,
                }),
            }
            return;
        }

        match self.loader.load(&import_key) {
            Ok(Some(loaded)) => {
                self.module_aliases
                    .insert(import_key.clone(), loaded.module.clone());
                self.resolve_module(loaded.module, loaded.source, loaded.program);
            }
            Ok(None) => {
                self.errors.push(ResolveError::ModuleNotFound {
                    module: import_key,
                    span,
                });
            }
            Err(ModuleLoadError::LoadFailed(message)) => {
                self.errors.push(ResolveError::LoadFailed {
                    module: import_key,
                    span,
                    message,
                });
            }
            Err(ModuleLoadError::Fatal(error)) => {
                self.fatal = Some(error);
            }
        }
    }

    fn into_parts(self) -> (Vec<ResolvedModule>, ModuleAliases, ImportEdges) {
        (self.modules, self.module_aliases, self.import_edges)
    }
}

fn build_dependency_groups(
    modules: Vec<ResolvedModule>,
    module_aliases: &ModuleAliases,
    import_edges: &ImportEdges,
) -> Vec<Vec<ResolvedModule>> {
    if modules.is_empty() {
        return vec![];
    }

    let index: HashMap<ModuleId, usize> = modules
        .iter()
        .enumerate()
        .map(|(i, m)| (m.key.clone(), i))
        .collect();

    let mut adj: Vec<Vec<usize>> = vec![vec![]; modules.len()];
    for (i, module) in modules.iter().enumerate() {
        for target in import_edges.get(&module.key).into_iter().flatten() {
            let base = module_aliases.get(&target.base).unwrap_or(&target.base);
            if let Some(&j) = index.get(base) {
                adj[i].push(j);
            }
        }
    }

    let sccs = tarjan_scc(&adj);

    let mut slots: Vec<Option<ResolvedModule>> = modules.into_iter().map(Some).collect();
    sccs.into_iter()
        .map(|scc| scc.into_iter().map(|i| slots[i].take().unwrap()).collect())
        .collect()
}

pub(crate) fn resolve_import_target(
    current: &ModuleId,
    import: &Spanned<ast::Import>,
    span: SourceSpan,
    packages: &HashMap<PackageId, PackageInput>,
    dependencies: &HashMap<PackageId, HashMap<String, PackageId>>,
    std_package: Option<&PackageId>,
    external_modules: &HashSet<ModuleId>,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    match &import.node.target.root {
        ast::ImportRoot::Local { .. } => local_import_target(current, import),
        ast::ImportRoot::Package(alias) => dependency_import_target(
            current,
            import,
            span,
            *alias,
            packages,
            dependencies,
            external_modules,
            errors,
        ),
        ast::ImportRoot::NativeProvider => {
            native_provider_import_target(current, import, span, external_modules, errors)
        }
        ast::ImportRoot::Std => std_import_target(std_package, import, span, errors),
    }
}

fn unsupported_import_root(
    root: &'static str,
    span: SourceSpan,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    errors.push(ResolveError::UnsupportedImportRoot { root, span });
    None
}

fn local_import_target(
    current: &ModuleId,
    import: &Spanned<ast::Import>,
) -> Option<ResolvedImportTarget> {
    let package = current.package_context()?.clone();
    let (_, path) = import.node.target.local_path()?;
    let path = ModulePath::from_idents(path);
    Some(ResolvedImportTarget {
        default_name: path_default_name(&path),
        base: ModuleId::named(package, path),
        exported_path: vec![],
        base_kind: ResolvedImportBaseKind::Source,
    })
}

fn dependency_import_target(
    current: &ModuleId,
    import: &Spanned<ast::Import>,
    span: SourceSpan,
    alias: Ident,
    packages: &HashMap<PackageId, PackageInput>,
    dependencies: &HashMap<PackageId, HashMap<String, PackageId>>,
    provider_modules: &HashSet<ModuleId>,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    let Some(package) = current.package_context() else {
        if let Some(file) = current.source_file() {
            errors.push(ResolveError::PackageImportUnavailable {
                file: file.clone(),
                alias: alias.to_string(),
                span,
            });
        }
        return None;
    };
    let dependency = dependencies
        .get(package)
        .and_then(|dependencies| dependencies.get(alias.as_str()));
    let Some(dependency) = dependency.cloned() else {
        errors.push(ResolveError::UnknownDependency {
            package: package.clone(),
            alias: alias.to_string(),
            span,
        });
        return None;
    };

    let exported_path = target_path_idents(&import.node.target.path);
    if packages
        .get(&dependency)
        .is_some_and(|package| package.kind == PackageKind::NativeOnly)
    {
        if exported_path.is_empty() {
            errors.push(ResolveError::NativeOnlyPkgRootImport {
                package: dependency,
                alias: alias.to_string(),
                span,
            });
            return None;
        }
        let path = ModulePath::from_idents(&exported_path);
        let module = ModuleId::provider(dependency.clone(), path.clone());
        if !provider_modules.contains(&module) {
            errors.push(ResolveError::UnknownNativeDepProviderModule {
                package: dependency,
                alias: alias.to_string(),
                module: path,
                span,
            });
            return None;
        }
        return Some(ResolvedImportTarget {
            base: module,
            default_name: exported_path.last().copied().unwrap_or(alias),
            exported_path: vec![],
            base_kind: ResolvedImportBaseKind::Provider,
        });
    }

    Some(ResolvedImportTarget {
        base: ModuleId::root(dependency),
        default_name: exported_path.last().copied().unwrap_or(alias),
        exported_path,
        base_kind: ResolvedImportBaseKind::Source,
    })
}

fn native_provider_import_target(
    current: &ModuleId,
    import: &Spanned<ast::Import>,
    span: SourceSpan,
    provider_modules: &HashSet<ModuleId>,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    let Some(package) = current.package_context().cloned() else {
        errors.push(ResolveError::NativeProviderUnavailable {
            package: None,
            span,
        });
        return None;
    };
    let ast::PackageModulePath::Named(path) = &import.node.target.path else {
        errors.push(ResolveError::NativeProviderUnavailable {
            package: Some(package),
            span,
        });
        return None;
    };
    let path = ModulePath::from_idents(path);
    let module = ModuleId::provider(package.clone(), path.clone());
    if !provider_modules.contains(&module) {
        errors.push(ResolveError::UnknownNativeProviderModule {
            package,
            module: path,
            span,
        });
        return None;
    }
    Some(ResolvedImportTarget {
        default_name: path_default_name(&path),
        base: module,
        exported_path: vec![],
        base_kind: ResolvedImportBaseKind::Provider,
    })
}

fn std_import_target(
    std_package: Option<&PackageId>,
    import: &Spanned<ast::Import>,
    span: SourceSpan,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    let Some(package) = std_package else {
        return unsupported_import_root("std", span, errors);
    };
    let exported_path = target_path_idents(&import.node.target.path);
    Some(ResolvedImportTarget {
        base: ModuleId::root(package.clone()),
        default_name: exported_path
            .last()
            .copied()
            .unwrap_or_else(|| Ident::new("std")),
        exported_path,
        base_kind: ResolvedImportBaseKind::Source,
    })
}

fn target_path_idents(path: &ast::PackageModulePath) -> Vec<Ident> {
    match path {
        ast::PackageModulePath::Root => vec![],
        ast::PackageModulePath::Named(path) => path.clone(),
    }
}

fn path_default_name(path: &ModulePath) -> Ident {
    path.segments()
        .last()
        .map_or_else(|| Ident::new(""), Ident::new)
}

fn import_nodes(program: &Program) -> impl Iterator<Item = &Spanned<ast::Import>> {
    program.stmts.iter().filter_map(|stmt| match &stmt.node {
        Stmt::Import(import) => Some(import),
        _ => None,
    })
}

fn tarjan_scc(adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let n = adj.len();
    let mut index_counter = 0u32;
    let mut stack: Vec<usize> = vec![];
    let mut on_stack = vec![false; n];
    let mut indices = vec![u32::MAX; n];
    let mut lowlinks = vec![0u32; n];
    let mut sccs: Vec<Vec<usize>> = vec![];

    for i in 0..n {
        if indices[i] == u32::MAX {
            tarjan_visit(
                i,
                adj,
                &mut index_counter,
                &mut stack,
                &mut on_stack,
                &mut indices,
                &mut lowlinks,
                &mut sccs,
            );
        }
    }

    sccs
}

fn tarjan_visit(
    v: usize,
    adj: &[Vec<usize>],
    index_counter: &mut u32,
    stack: &mut Vec<usize>,
    on_stack: &mut [bool],
    indices: &mut [u32],
    low_links: &mut [u32],
    sccs: &mut Vec<Vec<usize>>,
) {
    indices[v] = *index_counter;
    low_links[v] = *index_counter;
    *index_counter += 1;
    stack.push(v);
    on_stack[v] = true;

    for &w in &adj[v] {
        let is_unvisited = indices[w] == u32::MAX;
        if is_unvisited {
            tarjan_visit(
                w,
                adj,
                index_counter,
                stack,
                on_stack,
                indices,
                low_links,
                sccs,
            );
            low_links[v] = low_links[v].min(low_links[w]);
        } else if on_stack[w] {
            low_links[v] = low_links[v].min(indices[w]);
        }
    }

    let is_scc = low_links[v] == indices[v];
    if is_scc {
        let mut scc = vec![];
        loop {
            let w = stack.pop().unwrap();
            on_stack[w] = false;
            scc.push(w);
            if w == v {
                break;
            }
        }
        sccs.push(scc);
    }
}
