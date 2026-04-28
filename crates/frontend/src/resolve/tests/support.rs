use std::{
    collections::{HashMap, HashSet},
    convert::Infallible,
};

use crate::{
    ast::Program,
    lexer, parser,
    resolve::{
        self, ModuleLoadError, ModuleLoader, ModulePath, PreloadedModule, ResolveError,
        ResolveFailure, ResolveResult,
    },
};

pub fn parse_program(src: &str) -> Program {
    let tokens = lexer::tokenize(src).unwrap_or_else(|errs| panic!("failed to tokenize: {errs:?}"));
    parser::parse_ast(&tokens).unwrap_or_else(|errs| panic!("failed to parse: {errs:?}"))
}

pub fn module_path(segments: Vec<&str>) -> ModulePath {
    ModulePath::new(segments.into_iter().map(String::from).collect()).unwrap()
}

pub fn ignored_roots(roots: &[&str]) -> HashSet<String> {
    roots.iter().map(|s| s.to_string()).collect()
}

#[derive(Default)]
pub struct InMemoryLoader {
    modules: HashMap<ModulePath, Program>,
    missing: Vec<ModulePath>,
    failures: HashMap<ModulePath, String>,
    loads: Vec<ModulePath>,
}

impl InMemoryLoader {
    pub fn add_source(&mut self, path: ModulePath, source: &str) {
        let program = parse_program(source);
        self.modules.insert(path, program);
    }

    pub fn add_missing(&mut self, path: ModulePath) {
        self.missing.push(path);
    }

    pub fn add_failure(&mut self, path: ModulePath, msg: &str) {
        self.failures.insert(path, msg.to_string());
    }

    pub fn load_count(&self, path: &ModulePath) -> usize {
        self.loads.iter().filter(|loaded| *loaded == path).count()
    }
}

impl ModuleLoader for InMemoryLoader {
    type FatalError = Infallible;

    fn load(&mut self, path: &ModulePath) -> Result<Option<Program>, ModuleLoadError<Infallible>> {
        self.loads.push(path.clone());
        if let Some(msg) = self.failures.get(path) {
            return Err(ModuleLoadError::LoadFailed(msg.clone()));
        }
        if self.missing.iter().any(|p| p == path) {
            return Ok(None);
        }
        Ok(self.modules.get(path).cloned())
    }
}

pub fn resolve(
    source: &str,
    loader: &mut InMemoryLoader,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_modules(parse_program(source), vec![], loader, &HashSet::new())
}

pub fn resolve_with_ignored(
    source: &str,
    loader: &mut InMemoryLoader,
    ignored: &HashSet<String>,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_modules(parse_program(source), vec![], loader, ignored)
}

pub fn preloaded(path: &[&str], source: &str) -> PreloadedModule {
    PreloadedModule {
        path: module_path(path.to_vec()),
        program: parse_program(source),
    }
}

pub fn resolve_with_preloaded(
    source: &str,
    preloaded_modules: Vec<PreloadedModule>,
    loader: &mut InMemoryLoader,
) -> Result<ResolveResult, ResolveFailure<Infallible>> {
    resolve::resolve_modules(
        parse_program(source),
        preloaded_modules,
        loader,
        &HashSet::new(),
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
