#[cfg(test)]
mod tests;

use std::{
    collections::{HashMap, HashSet},
    fmt,
    rc::Rc,
};

use crate::{
    ast::{self, Ident, Program, Stmt},
    span::{Span, Spanned},
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PackageModulePath {
    Root,
    Named(ModulePath),
}

impl PackageModulePath {
    pub fn named_path(&self) -> Option<&ModulePath> {
        match self {
            Self::Root => None,
            Self::Named(path) => Some(path),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId {
    package: PackageId,
    path: PackageModulePath,
}

impl ModuleId {
    pub fn root(package: PackageId) -> Self {
        Self {
            package,
            path: PackageModulePath::Root,
        }
    }

    pub fn named(package: PackageId, path: ModulePath) -> Self {
        Self {
            package,
            path: PackageModulePath::Named(path),
        }
    }

    pub fn package(&self) -> &PackageId {
        &self.package
    }

    pub fn path(&self) -> &PackageModulePath {
        &self.path
    }

    pub fn named_path(&self) -> Option<&ModulePath> {
        self.path.named_path()
    }

    fn is_ignored(&self, ignored_roots: &HashSet<String>) -> bool {
        self.named_path()
            .and_then(ModulePath::first_segment)
            .is_some_and(|s| ignored_roots.contains(s))
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub key: ModuleId,
    pub program: Program,
}

#[derive(Debug, Clone)]
pub struct ResolveResult {
    pub root: ModuleId,
    pub module_groups: Vec<Vec<ResolvedModule>>,
    pub dependencies: HashMap<PackageId, HashMap<String, PackageId>>,
    pub system: SystemPackages,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SystemPackages {
    pub std: Option<PackageId>,
    pub core: Option<PackageId>,
}

#[derive(Debug, Clone)]
pub struct PreloadedModule {
    pub module: ModuleId,
    pub program: Program,
}

#[derive(Debug, Clone, Default)]
pub struct PackageInput {
    pub root: Option<Program>,
    pub dependencies: HashMap<String, PackageId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ResolvedImportTarget {
    pub(crate) base: ModuleId,
    pub(crate) exported_path: Vec<Ident>,
    pub(crate) default_name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    ModuleNotFound {
        module: ModuleId,
        span: Span,
    },
    LoadFailed {
        module: ModuleId,
        span: Span,
        message: String,
    },
    DuplicatePreloadedModule {
        module: ModuleId,
    },
    UnknownDependency {
        package: PackageId,
        alias: String,
        span: Span,
    },
    UnsupportedImportRoot {
        root: &'static str,
        span: Span,
    },
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
    ) -> Result<Option<Program>, ModuleLoadError<Self::FatalError>>;
}

pub fn resolve_modules<L: ModuleLoader>(
    root: Program,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut L,
    ignored_roots: &HashSet<String>,
    external_modules: &HashSet<ModulePath>,
) -> Result<ResolveResult, ResolveFailure<L::FatalError>> {
    let root_package = PackageId::synthetic_root();
    let packages = HashMap::from([(root_package.clone(), PackageInput::default())]);
    let external_modules = external_modules
        .iter()
        .cloned()
        .map(|path| ModuleId::named(root_package.clone(), path))
        .collect::<HashSet<_>>();
    resolve_package_modules(
        root_package,
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
    root_package: PackageId,
    root: Program,
    packages: &HashMap<PackageId, PackageInput>,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut L,
    ignored_roots: &HashSet<String>,
    external_modules: &HashSet<ModuleId>,
    system: SystemPackages,
) -> Result<ResolveResult, ResolveFailure<L::FatalError>> {
    let preloaded =
        prepare_preloaded_modules(preloaded_modules).map_err(ResolveFailure::Resolve)?;
    let root_id = ModuleId::root(root_package);
    let dependencies = package_dependencies(packages);
    let mut resolver = Resolver {
        root: root_id.clone(),
        packages,
        dependencies: &dependencies,
        loader,
        ignored_roots,
        external_modules,
        std_package: system.std.as_ref(),
        preloaded: preloaded.iter().cloned().collect(),
        visiting: HashSet::new(),
        loaded: HashSet::new(),
        modules: vec![],
        errors: vec![],
        fatal: None,
    };

    for (module, program) in preloaded {
        resolver.resolve_module(module, program);
    }
    resolver.resolve_module(root_id.clone(), root);

    if let Some(error) = resolver.fatal {
        return Err(ResolveFailure::Fatal(error));
    }
    if !resolver.errors.is_empty() {
        return Err(ResolveFailure::Resolve(resolver.errors));
    }

    let modules = resolver.into_modules();
    let groups = build_dependency_groups(modules, &dependencies, system.std.as_ref());
    Ok(ResolveResult {
        root: root_id,
        module_groups: groups,
        dependencies,
        system,
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
) -> Result<Vec<(ModuleId, Program)>, Vec<ResolveError>> {
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
        roots.push((module.module, module.program));
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
    preloaded: HashMap<ModuleId, Program>,
    visiting: HashSet<ModuleId>,
    loaded: HashSet<ModuleId>,
    modules: Vec<ResolvedModule>,
    errors: Vec<ResolveError>,
    fatal: Option<L::FatalError>,
}

impl<L: ModuleLoader> Resolver<'_, L> {
    fn resolve_module(&mut self, key: ModuleId, program: Program) {
        if self.fatal.is_some() || self.loaded.contains(&key) || self.visiting.contains(&key) {
            return;
        }

        self.visiting.insert(key.clone());

        for import in import_nodes(&program) {
            let Some(target) = resolve_import_target(
                &key,
                import,
                self.dependencies,
                self.std_package,
                &mut self.errors,
            ) else {
                continue;
            };

            self.resolve_import_base(target.base, import.span);
        }

        self.visiting.remove(&key);
        self.loaded.insert(key.clone());
        self.modules.push(ResolvedModule { key, program });
    }

    fn resolve_import_base(&mut self, import_key: ModuleId, span: Span) {
        if import_key.is_ignored(self.ignored_roots) || self.loaded.contains(&import_key) {
            return;
        }

        if let Some(module_program) = self.preloaded.get(&import_key).cloned() {
            self.resolve_module(import_key, module_program);
            return;
        }

        if matches!(import_key.path(), PackageModulePath::Root) {
            match self
                .packages
                .get(import_key.package())
                .and_then(|package| package.root.clone())
            {
                Some(module_program) => self.resolve_module(import_key, module_program),
                None if import_key == self.root => {}
                None => self.errors.push(ResolveError::ModuleNotFound {
                    module: import_key,
                    span,
                }),
            }
            return;
        }

        match self.loader.load(&import_key) {
            Ok(Some(module_program)) => {
                self.resolve_module(import_key, module_program);
            }
            Ok(None) if self.external_modules.contains(&import_key) => {
                self.loaded.insert(import_key);
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

    fn into_modules(self) -> Vec<ResolvedModule> {
        self.modules
    }
}

fn build_dependency_groups(
    modules: Vec<ResolvedModule>,
    dependencies: &HashMap<PackageId, HashMap<String, PackageId>>,
    std_package: Option<&PackageId>,
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
        for import in import_nodes(&module.program) {
            let mut errors = vec![];
            let Some(target) =
                resolve_import_target(&module.key, import, dependencies, std_package, &mut errors)
            else {
                continue;
            };
            if let Some(&j) = index.get(&target.base) {
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
    dependencies: &HashMap<PackageId, HashMap<String, PackageId>>,
    std_package: Option<&PackageId>,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    match &import.node.target.root {
        ast::ImportRoot::Local => local_import_target(current.package().clone(), import),
        ast::ImportRoot::Dependency(alias) => {
            dependency_import_target(current, import, *alias, dependencies, errors)
        }
        ast::ImportRoot::NativeProvider => unsupported_import_root("ext", import, errors),
        ast::ImportRoot::Std => std_import_target(std_package, import, errors),
    }
}

fn unsupported_import_root(
    root: &'static str,
    import: &Spanned<ast::Import>,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    errors.push(ResolveError::UnsupportedImportRoot {
        root,
        span: import.span,
    });
    None
}

fn local_import_target(
    package: PackageId,
    import: &Spanned<ast::Import>,
) -> Option<ResolvedImportTarget> {
    let path = import
        .node
        .target
        .local_path()
        .map(ModulePath::from_idents)?;
    Some(ResolvedImportTarget {
        default_name: path_default_name(&path),
        base: ModuleId::named(package, path),
        exported_path: vec![],
    })
}

fn dependency_import_target(
    current: &ModuleId,
    import: &Spanned<ast::Import>,
    alias: Ident,
    dependencies: &HashMap<PackageId, HashMap<String, PackageId>>,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    let package = dependencies.get(current.package());
    let dependency = package.and_then(|dependencies| dependencies.get(alias.as_str()));
    let Some(dependency) = dependency.cloned() else {
        errors.push(ResolveError::UnknownDependency {
            package: current.package().clone(),
            alias: alias.to_string(),
            span: import.span,
        });
        return None;
    };

    let exported_path = target_path_idents(&import.node.target.path);
    Some(ResolvedImportTarget {
        base: ModuleId::root(dependency),
        default_name: exported_path.last().copied().unwrap_or(alias),
        exported_path,
    })
}

fn std_import_target(
    std_package: Option<&PackageId>,
    import: &Spanned<ast::Import>,
    errors: &mut Vec<ResolveError>,
) -> Option<ResolvedImportTarget> {
    let Some(package) = std_package else {
        return unsupported_import_root("std", import, errors);
    };
    let exported_path = target_path_idents(&import.node.target.path);
    Some(ResolvedImportTarget {
        base: ModuleId::root(package.clone()),
        default_name: exported_path
            .last()
            .copied()
            .unwrap_or_else(|| Ident::new("std")),
        exported_path,
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
