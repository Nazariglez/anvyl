use std::collections::HashMap;

use crate::{
    ast::Program,
    lexer, parser,
    resolve::{self, ModuleLoader, ModulePath, ResolveError, ResolveResult},
};

pub fn parse_program(src: &str) -> Program {
    let tokens = lexer::tokenize(src).unwrap_or_else(|errs| panic!("failed to tokenize: {errs:?}"));
    parser::parse_ast(&tokens).unwrap_or_else(|errs| panic!("failed to parse: {errs:?}"))
}

pub fn module_path(segments: Vec<&str>) -> ModulePath {
    ModulePath::new(segments.into_iter().map(String::from).collect())
}

pub fn ignored_roots(roots: &[&str]) -> HashSet<String> {
    roots.iter().map(|s| s.to_string()).collect()
}

#[derive(Default)]
pub struct InMemoryLoader {
    modules: HashMap<ModulePath, Program>,
    missing: Vec<ModulePath>,
    failures: HashMap<ModulePath, String>,
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
}

impl ModuleLoader for InMemoryLoader {
    fn load(&mut self, path: &ModulePath) -> Result<Option<Program>, String> {
        if let Some(msg) = self.failures.get(path) {
            return Err(msg.clone());
        }
        if self.missing.iter().any(|p| p == path) {
            return Ok(None);
        }
        Ok(self.modules.get(path).cloned())
    }
}

use std::collections::HashSet;

pub fn resolve(
    source: &str,
    loader: &mut InMemoryLoader,
) -> Result<ResolveResult, Vec<ResolveError>> {
    resolve::resolve_modules(parse_program(source), loader, &HashSet::new())
}

pub fn resolve_with_ignored(
    source: &str,
    loader: &mut InMemoryLoader,
    ignored: &HashSet<String>,
) -> Result<ResolveResult, Vec<ResolveError>> {
    resolve::resolve_modules(parse_program(source), loader, ignored)
}
