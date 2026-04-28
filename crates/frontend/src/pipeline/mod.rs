// Lexer -> Parser -> Resolver -> Typechecker -> AIR lowering

use std::collections::{HashMap, HashSet};

use crate::{
    ast::Program,
    lexer, parser,
    resolve::{self, ModuleLoader, ModulePath},
    typecheck,
};

pub struct Source {
    pub code: String,
    pub path: String,
}

pub trait SourceLoader {
    fn load(&mut self, module_path: &[String]) -> Result<Option<Source>, String>;
}

pub struct ProgramInput<'a> {
    pub main: Source,
    pub prelude: String,
    pub core_modules: Vec<Source>,
    pub source_loader: &'a mut dyn SourceLoader,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckOk;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckError {
    Lex { path: String, messages: Vec<String> },
    Parse { path: String, messages: Vec<String> },
    Resolve { messages: Vec<String> },
    Type { messages: Vec<String> },
}

pub fn check(input: ProgramInput<'_>) -> Result<CheckOk, CheckError> {
    let mut root = parse_source(&input.main)?;
    if !input.prelude.is_empty() {
        prepend_prelude(&mut root, &input.prelude)?;
    }

    let mut loader = InputModuleLoader::new(input.source_loader);
    for source in input.core_modules {
        loader.add(source)?;
    }

    let resolved =
        resolve::resolve_modules(root.clone(), &mut loader, &HashSet::new()).map_err(|errors| {
            CheckError::Resolve {
                messages: errors
                    .into_iter()
                    .map(|error| format!("{error:?}"))
                    .collect(),
            }
        })?;

    typecheck::check_with_modules(&root, &resolved).map_err(|errors| CheckError::Type {
        messages: errors
            .into_iter()
            .map(|error| format!("{error:?}"))
            .collect(),
    })?;

    Ok(CheckOk)
}

fn prepend_prelude(program: &mut Program, prelude: &str) -> Result<(), CheckError> {
    let mut prelude = parse_source(&Source {
        code: prelude.to_string(),
        path: "<prelude>".to_string(),
    })?;
    prelude.stmts.append(&mut program.stmts);
    *program = prelude;
    Ok(())
}

fn parse_source(source: &Source) -> Result<Program, CheckError> {
    let tokens = lexer::tokenize(&source.code).map_err(|errors| CheckError::Lex {
        path: source.path.clone(),
        messages: errors
            .into_iter()
            .map(|error| format!("{error:?}"))
            .collect(),
    })?;

    parser::parse_ast(&tokens).map_err(|errors| CheckError::Parse {
        path: source.path.clone(),
        messages: errors
            .into_iter()
            .map(|error| format!("{error:?}"))
            .collect(),
    })
}

struct InputModuleLoader<'a> {
    sources: HashMap<ModulePath, Program>,
    loader: &'a mut dyn SourceLoader,
}

impl<'a> InputModuleLoader<'a> {
    fn new(loader: &'a mut dyn SourceLoader) -> Self {
        Self {
            sources: HashMap::new(),
            loader,
        }
    }

    fn add(&mut self, source: Source) -> Result<(), CheckError> {
        let path = source_path(&source.path);
        let program = parse_source(&source)?;
        self.sources.insert(path, program);
        Ok(())
    }
}

impl ModuleLoader for InputModuleLoader<'_> {
    fn load(&mut self, path: &ModulePath) -> Result<Option<Program>, String> {
        if let Some(program) = self.sources.get(path) {
            return Ok(Some(program.clone()));
        }

        let Some(source) = self.loader.load(path.segments())? else {
            return Ok(None);
        };

        let program = parse_source(&source).map_err(|error| format!("{error:?}"))?;
        Ok(Some(program))
    }
}

fn source_path(path: &str) -> ModulePath {
    ModulePath::new(
        path.split([':', '/', '\\'])
            .filter(|segment| !segment.is_empty())
            .map(str::to_string)
            .collect(),
    )
}

#[cfg(test)]
mod tests {
    use super::{CheckError, ProgramInput, Source, SourceLoader, check};

    struct EmptyLoader;

    impl SourceLoader for EmptyLoader {
        fn load(&mut self, _module_path: &[String]) -> Result<Option<Source>, String> {
            Ok(None)
        }
    }

    fn check_source(source: &str) -> Result<super::CheckOk, CheckError> {
        let mut loader = EmptyLoader;
        check(ProgramInput {
            main: Source {
                code: source.to_string(),
                path: "main.anv".to_string(),
            },
            prelude: String::new(),
            core_modules: vec![],
            source_loader: &mut loader,
        })
    }

    #[test]
    fn classifies_lex_errors() {
        let err = check_source("fn main() { \"unterminated }").unwrap_err();

        assert!(matches!(err, CheckError::Lex { .. }));
    }

    #[test]
    fn classifies_parse_errors() {
        let err = check_source("fn main( {}").unwrap_err();

        assert!(matches!(err, CheckError::Parse { .. }));
    }

    #[test]
    fn classifies_resolve_errors() {
        let err = check_source("import missing as m; fn main() {} ").unwrap_err();

        assert!(matches!(err, CheckError::Resolve { .. }));
    }

    #[test]
    fn classifies_type_errors() {
        let err = check_source("fn main() { let x: int = true; } ").unwrap_err();

        assert!(matches!(err, CheckError::Type { .. }));
    }
}
