use std::{
    collections::{HashMap, HashSet},
    convert::Infallible,
};

use crate::{
    ast::Program,
    lexer, parser,
    resolve::{
        self, LoadedModule, ModuleId, ModuleLoadError, ModuleLoader, ModulePath, PackageId,
        PackageInput, PackageKind, PreloadedModule, ResolveError, ResolveFailure, ResolveResult,
        SourceFileId,
    },
    source::{SourceId, SourceKind, SourceTable},
};

fn parse_source(src: &str) -> (SourceId, Program) {
    let mut sources = SourceTable::default();
    let source = sources.add(SourceKind::Virtual, "test", None, src);
    let tokens =
        lexer::tokenize(source, src).unwrap_or_else(|errs| panic!("failed to tokenize: {errs:?}"));
    let program =
        parser::parse_ast(&tokens).unwrap_or_else(|errs| panic!("failed to parse: {errs:?}"));
    (source, program)
}

pub fn parse_program(src: &str) -> Program {
    parse_source(src).1
}

pub fn loaded_module(module: ModuleId, source: &str) -> LoadedModule {
    let (source, program) = parse_source(source);
    LoadedModule {
        module,
        source,
        program,
    }
}

pub fn package(name: &str) -> PackageId {
    PackageId::new(name)
}

pub fn root_package() -> PackageId {
    PackageId::synthetic_root()
}

pub fn module_path<I, S>(segments: I) -> ModulePath
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    ModulePath::new(
        segments
            .into_iter()
            .map(|segment| segment.as_ref().to_owned())
            .collect(),
    )
    .unwrap()
}

pub fn module_id(package: &PackageId, segments: &[&str]) -> ModuleId {
    ModuleId::named(package.clone(), module_path(segments))
}

pub fn provider_id(package: &PackageId, segments: &[&str]) -> ModuleId {
    ModuleId::provider(package.clone(), module_path(segments))
}

pub fn root_id(package: &PackageId) -> ModuleId {
    ModuleId::root(package.clone())
}

pub fn source_id(package: &PackageId, path: &str) -> ModuleId {
    ModuleId::source(package.clone(), SourceFileId::new(path).unwrap())
}

pub fn package_input(dependencies: &[(&str, PackageId)]) -> PackageInput {
    package_input_with_kind(dependencies, PackageKind::Source)
}

pub fn native_package_input(dependencies: &[(&str, PackageId)]) -> PackageInput {
    package_input_with_kind(dependencies, PackageKind::NativeOnly)
}

fn package_input_with_kind(dependencies: &[(&str, PackageId)], kind: PackageKind) -> PackageInput {
    PackageInput {
        root: None,
        dependencies: dependencies
            .iter()
            .map(|(alias, package)| ((*alias).to_string(), package.clone()))
            .collect(),
        kind,
    }
}

pub fn package_root(
    package: &PackageId,
    source: &str,
    dependencies: &[(&str, PackageId)],
) -> PackageInput {
    PackageInput {
        root: Some(loaded_module(ModuleId::root(package.clone()), source)),
        dependencies: dependencies
            .iter()
            .map(|(alias, package)| ((*alias).to_string(), package.clone()))
            .collect(),
        kind: PackageKind::Source,
    }
}

pub fn ignored_roots(roots: &[&str]) -> HashSet<String> {
    roots.iter().map(ToString::to_string).collect()
}

#[derive(Default)]
pub struct InMemoryLoader {
    modules: HashMap<ModuleId, LoadedModule>,
    missing: Vec<ModuleId>,
    failures: HashMap<ModuleId, String>,
    loads: Vec<ModuleId>,
}

impl InMemoryLoader {
    pub fn add_source(&mut self, path: ModulePath, source: &str) {
        let package = root_package();
        self.add_package_source(&package, path, source);
    }

    pub fn add_package_source(&mut self, package: &PackageId, path: ModulePath, source: &str) {
        let module = ModuleId::named(package.clone(), path);
        self.modules
            .insert(module.clone(), loaded_module(module, source));
    }

    pub fn add_missing(&mut self, path: ModulePath) {
        let package = root_package();
        self.add_package_missing(&package, path);
    }

    pub fn add_package_missing(&mut self, package: &PackageId, path: ModulePath) {
        self.missing.push(ModuleId::named(package.clone(), path));
    }

    pub fn add_failure(&mut self, path: ModulePath, msg: &str) {
        let package = root_package();
        self.add_package_failure(&package, path, msg);
    }

    pub fn add_package_failure(&mut self, package: &PackageId, path: ModulePath, msg: &str) {
        self.failures
            .insert(ModuleId::named(package.clone(), path), msg.to_string());
    }

    pub fn load_count(&self, path: &ModulePath) -> usize {
        let package = root_package();
        self.load_count_module(&ModuleId::named(package, path.clone()))
    }

    pub fn load_count_module(&self, module: &ModuleId) -> usize {
        self.loads.iter().filter(|loaded| *loaded == module).count()
    }
}

impl ModuleLoader for InMemoryLoader {
    type FatalError = Infallible;

    fn load(
        &mut self,
        module: &ModuleId,
    ) -> Result<Option<LoadedModule>, ModuleLoadError<Infallible>> {
        self.loads.push(module.clone());
        if let Some(msg) = self.failures.get(module) {
            return Err(ModuleLoadError::LoadFailed(msg.clone()));
        }
        if self.missing.iter().any(|missing| missing == module) {
            return Ok(None);
        }
        Ok(self.modules.get(module).cloned())
    }
}

pub fn resolve(
    source: &str,
    loader: &mut InMemoryLoader,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_modules(
        loaded_module(ModuleId::root(root_package()), source),
        vec![],
        loader,
        &HashSet::new(),
        &HashSet::new(),
    )
}

pub fn resolve_with_ignored(
    source: &str,
    loader: &mut InMemoryLoader,
    ignored: &HashSet<String>,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_modules(
        loaded_module(ModuleId::root(root_package()), source),
        vec![],
        loader,
        ignored,
        &HashSet::new(),
    )
}

pub fn resolve_package(
    root_package: PackageId,
    source: &str,
    packages: &HashMap<PackageId, PackageInput>,
    loader: &mut InMemoryLoader,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_package_modules(
        loaded_module(ModuleId::root(root_package), source),
        packages,
        vec![],
        loader,
        &HashSet::new(),
        &HashSet::new(),
        resolve::SystemPackages::default(),
    )
}

pub fn preloaded(path: &[&str], source: &str) -> PreloadedModule {
    let (source, program) = parse_source(source);
    PreloadedModule {
        module: module_id(&root_package(), path),
        source,
        program,
    }
}

pub fn resolve_with_preloaded(
    source: &str,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut InMemoryLoader,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve_with_preloaded_and_external(source, preloaded_modules, loader, &HashSet::new())
}

pub fn resolve_with_external(
    source: &str,
    loader: &mut InMemoryLoader,
    external_modules: &HashSet<ModulePath>,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_modules(
        loaded_module(ModuleId::root(root_package()), source),
        vec![],
        loader,
        &HashSet::new(),
        external_modules,
    )
}

pub fn resolve_with_preloaded_and_external(
    source: &str,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut InMemoryLoader,
    external_modules: &HashSet<ModulePath>,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_modules(
        loaded_module(ModuleId::root(root_package()), source),
        preloaded_modules,
        loader,
        &HashSet::new(),
        external_modules,
    )
}

pub fn resolve_errors(
    result: Result<ResolveResult, ResolveFailure<Infallible>>,
) -> Vec<ResolveError> {
    match result {
        Err(ResolveFailure::Resolve(errors)) => errors,
        Err(ResolveFailure::Fatal(error)) => match error {},
        Ok(_) => panic!("expected resolve errors"),
    }
}
