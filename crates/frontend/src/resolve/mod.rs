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
        Self::from_valid_segments(idents.iter().map(|i| i.to_string()).collect())
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
pub enum ModuleKey {
    Root,
    Named(ModulePath),
}

impl ModuleKey {
    fn is_ignored(&self, ignored_roots: &HashSet<String>) -> bool {
        match self {
            ModuleKey::Root => false,
            ModuleKey::Named(path) => path
                .first_segment()
                .is_some_and(|s| ignored_roots.contains(s)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub key: ModuleKey,
    pub program: Program,
}

#[derive(Debug, Clone)]
pub struct ResolveResult {
    pub module_groups: Vec<Vec<ResolvedModule>>,
}

#[derive(Debug, Clone)]
pub struct PreloadedModule {
    pub path: ModulePath,
    pub program: Program,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    ModuleNotFound {
        path: ModulePath,
        span: Span,
    },
    LoadFailed {
        path: ModulePath,
        span: Span,
        message: String,
    },
    DuplicatePreloadedModule {
        path: ModulePath,
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
        path: &ModulePath,
    ) -> Result<Option<Program>, ModuleLoadError<Self::FatalError>>;
}

pub fn resolve_modules<L: ModuleLoader>(
    root: Program,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut L,
    ignored_roots: &HashSet<String>,
) -> Result<ResolveResult, ResolveFailure<L::FatalError>> {
    let preloaded_roots =
        prepare_preloaded_modules(preloaded_modules).map_err(ResolveFailure::Resolve)?;
    let mut resolver = Resolver {
        loader,
        ignored_roots,
        preloaded: preloaded_roots.iter().cloned().collect(),
        visiting: HashSet::new(),
        loaded: HashSet::new(),
        modules: Vec::new(),
        errors: Vec::new(),
        fatal: None,
    };
    for (path, program) in preloaded_roots {
        resolver.resolve_module(ModuleKey::Named(path), program);
    }
    resolver.resolve_root(root);
    if let Some(error) = resolver.fatal {
        return Err(ResolveFailure::Fatal(error));
    }
    if resolver.errors.is_empty() {
        let modules = resolver.into_modules();
        let groups = build_dependency_groups(modules, ignored_roots);
        return Ok(ResolveResult {
            module_groups: groups,
        });
    }

    Err(ResolveFailure::Resolve(resolver.errors))
}

fn prepare_preloaded_modules(
    modules: Vec<PreloadedModule>,
) -> Result<Vec<(ModulePath, Program)>, Vec<ResolveError>> {
    let mut seen = HashSet::new();
    let mut roots = vec![];
    let mut errors = vec![];

    for module in modules {
        if !seen.insert(module.path.clone()) {
            errors.push(ResolveError::DuplicatePreloadedModule { path: module.path });
            continue;
        }
        roots.push((module.path, module.program));
    }

    if errors.is_empty() {
        Ok(roots)
    } else {
        Err(errors)
    }
}

struct Resolver<'a, L: ModuleLoader> {
    loader: &'a mut L,
    ignored_roots: &'a HashSet<String>,
    preloaded: HashMap<ModulePath, Program>,
    visiting: HashSet<ModuleKey>,
    loaded: HashSet<ModuleKey>,
    modules: Vec<ResolvedModule>,
    errors: Vec<ResolveError>,
    fatal: Option<L::FatalError>,
}

impl<L: ModuleLoader> Resolver<'_, L> {
    fn resolve_root(&mut self, root: Program) {
        self.resolve_module(ModuleKey::Root, root);
    }

    fn resolve_module(&mut self, key: ModuleKey, program: Program) {
        if self.fatal.is_some() {
            return;
        }

        if self.loaded.contains(&key) {
            return;
        }

        if self.visiting.contains(&key) {
            return;
        }

        self.visiting.insert(key.clone());

        for import in import_nodes(&program) {
            let import_key = ModuleKey::Named(ModulePath::from_idents(&import.node.path));

            if import_key.is_ignored(self.ignored_roots) {
                continue;
            }

            if self.loaded.contains(&import_key) {
                continue;
            }

            let ModuleKey::Named(path) = &import_key else {
                continue;
            };

            if let Some(module_program) = self.preloaded.get(path).cloned() {
                self.resolve_module(import_key.clone(), module_program);
                continue;
            }

            match self.loader.load(path) {
                Ok(Some(module_program)) => {
                    self.resolve_module(import_key.clone(), module_program);
                }
                Ok(None) => {
                    self.errors.push(ResolveError::ModuleNotFound {
                        path: path.clone(),
                        span: import.span,
                    });
                }
                Err(ModuleLoadError::LoadFailed(message)) => {
                    self.errors.push(ResolveError::LoadFailed {
                        path: path.clone(),
                        span: import.span,
                        message,
                    });
                }
                Err(ModuleLoadError::Fatal(error)) => {
                    self.fatal = Some(error);
                    return;
                }
            }
        }

        self.visiting.remove(&key);
        self.loaded.insert(key.clone());

        self.modules.push(ResolvedModule { key, program });
    }

    fn into_modules(self) -> Vec<ResolvedModule> {
        self.modules
    }
}

fn build_dependency_groups(
    modules: Vec<ResolvedModule>,
    ignored_roots: &HashSet<String>,
) -> Vec<Vec<ResolvedModule>> {
    if modules.is_empty() {
        return vec![];
    }

    let index: HashMap<ModuleKey, usize> = modules
        .iter()
        .enumerate()
        .map(|(i, m)| (m.key.clone(), i))
        .collect();

    let mut adj: Vec<Vec<usize>> = vec![vec![]; modules.len()];
    for (i, module) in modules.iter().enumerate() {
        for import in import_nodes(&module.program) {
            let import_key = ModuleKey::Named(ModulePath::from_idents(&import.node.path));
            if import_key.is_ignored(ignored_roots) {
                continue;
            }
            if let Some(&j) = index.get(&import_key) {
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

fn import_nodes(program: &Program) -> Vec<&Spanned<ast::Import>> {
    let mut imports = Vec::new();
    for stmt in &program.stmts {
        let Stmt::Import(import_node) = &stmt.node else {
            continue;
        };
        imports.push(import_node);
    }
    imports
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
