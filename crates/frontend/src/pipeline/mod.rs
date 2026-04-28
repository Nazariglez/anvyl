// Lexer -> Parser -> Resolver -> Typechecker -> AIR lowering

use std::collections::HashSet;

use crate::{
    ast::Program,
    lexer, parser,
    resolve::{
        self, ModuleKey, ModuleLoadError, ModuleLoader, ModulePath, PreloadedModule, ResolveFailure,
    },
    typecheck::{self, ModuleScope},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source {
    pub code: String,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInput {
    pub path: ModulePath,
    pub source: Source,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceLoadError<E> {
    LoadFailed(String),
    Fatal(E),
}

pub trait SourceLoader {
    type FatalError;

    fn load(
        &mut self,
        module_path: &ModulePath,
    ) -> Result<Option<Source>, SourceLoadError<Self::FatalError>>;
}

pub struct ProgramInput<'a, L: SourceLoader> {
    pub main: Source,
    pub prelude: Option<Source>,
    pub preloaded_modules: Vec<ModuleInput>,
    pub always_active_modules: Vec<ModulePath>,
    pub source_loader: &'a mut L,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckOk;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckError<E = std::convert::Infallible> {
    Lex {
        label: String,
        messages: Vec<String>,
    },
    Parse {
        label: String,
        messages: Vec<String>,
    },
    Resolve {
        messages: Vec<String>,
    },
    Type {
        messages: Vec<String>,
    },
    Source(Box<E>),
}

pub fn check<L: SourceLoader>(
    input: ProgramInput<'_, L>,
) -> Result<CheckOk, CheckError<L::FatalError>> {
    let mut root = parse_source(&input.main)?;
    if let Some(prelude) = input.prelude {
        prepend_prelude(&mut root, &prelude)?;
    }

    let preloaded_modules = parse_preloaded_modules(input.preloaded_modules)?;
    let always_active_modules = input
        .always_active_modules
        .into_iter()
        .map(module_scope)
        .collect::<HashSet<_>>();

    let mut loader = InputModuleLoader::new(input.source_loader);
    let resolved = match resolve::resolve_modules(
        root.clone(),
        preloaded_modules,
        &mut loader,
        &HashSet::new(),
    ) {
        Ok(resolved) => resolved,
        Err(ResolveFailure::Fatal(error)) => return Err(error),
        Err(ResolveFailure::Resolve(errors)) => {
            return Err(CheckError::Resolve {
                messages: errors
                    .into_iter()
                    .map(|error| format!("{error:?}"))
                    .collect(),
            });
        }
    };

    validate_always_active_modules(&resolved, &always_active_modules)?;

    typecheck::check_with_modules(&root, &resolved, always_active_modules).map_err(|errors| {
        CheckError::Type {
            messages: errors
                .into_iter()
                .map(|error| format!("{error:?}"))
                .collect(),
        }
    })?;

    Ok(CheckOk)
}

fn parse_preloaded_modules<E>(
    modules: Vec<ModuleInput>,
) -> Result<Vec<PreloadedModule>, CheckError<E>> {
    modules
        .into_iter()
        .map(|module| {
            Ok(PreloadedModule {
                path: module.path,
                program: parse_source(&module.source)?,
            })
        })
        .collect()
}

fn validate_always_active_modules<E>(
    resolved: &resolve::ResolveResult,
    always_active_modules: &HashSet<ModuleScope>,
) -> Result<(), CheckError<E>> {
    let resolved_modules = resolved
        .module_groups
        .iter()
        .flat_map(|group| group.iter())
        .filter_map(|module| match &module.key {
            ModuleKey::Root => None,
            ModuleKey::Named(path) => Some(ModuleScope::Named(path.clone())),
        })
        .collect::<HashSet<_>>();

    let missing = always_active_modules
        .iter()
        .filter(|module| !resolved_modules.contains(module))
        .map(format_module_scope)
        .collect::<Vec<_>>();

    if missing.is_empty() {
        Ok(())
    } else {
        Err(CheckError::Resolve {
            messages: missing
                .into_iter()
                .map(|module| format!("always-active module was not resolved: {module}"))
                .collect(),
        })
    }
}

fn format_module_scope(module: &ModuleScope) -> String {
    match module {
        ModuleScope::Root => "<root>".to_string(),
        ModuleScope::Named(path) => path.segments().join("."),
    }
}

fn prepend_prelude<E>(program: &mut Program, prelude: &Source) -> Result<(), CheckError<E>> {
    let mut prelude = parse_source(prelude)?;
    prelude.stmts.append(&mut program.stmts);
    *program = prelude;
    Ok(())
}

fn parse_source<E>(source: &Source) -> Result<Program, CheckError<E>> {
    let tokens = lexer::tokenize(&source.code).map_err(|errors| CheckError::Lex {
        label: source.label.clone(),
        messages: errors
            .into_iter()
            .map(|error| format!("{error:?}"))
            .collect(),
    })?;

    parser::parse_ast(&tokens).map_err(|errors| CheckError::Parse {
        label: source.label.clone(),
        messages: errors
            .into_iter()
            .map(|error| format!("{error:?}"))
            .collect(),
    })
}

fn module_scope(path: ModulePath) -> ModuleScope {
    ModuleScope::Named(path)
}

struct InputModuleLoader<'a, L: SourceLoader> {
    loader: &'a mut L,
}

impl<'a, L: SourceLoader> InputModuleLoader<'a, L> {
    fn new(loader: &'a mut L) -> Self {
        Self { loader }
    }
}

impl<L: SourceLoader> ModuleLoader for InputModuleLoader<'_, L> {
    type FatalError = CheckError<L::FatalError>;

    fn load(
        &mut self,
        path: &ModulePath,
    ) -> Result<Option<Program>, ModuleLoadError<Self::FatalError>> {
        let source = match self.loader.load(path) {
            Ok(Some(source)) => source,
            Ok(None) => return Ok(None),
            Err(SourceLoadError::LoadFailed(message)) => {
                return Err(ModuleLoadError::LoadFailed(message));
            }
            Err(SourceLoadError::Fatal(error)) => {
                return Err(ModuleLoadError::Fatal(CheckError::Source(Box::new(error))));
            }
        };

        parse_source(&source)
            .map(Some)
            .map_err(ModuleLoadError::Fatal)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{
        CheckError, ModuleInput, ProgramInput, Source, SourceLoadError, SourceLoader, check,
    };
    use crate::resolve::ModulePath;

    #[derive(Default)]
    struct TestLoader {
        sources: HashMap<Vec<String>, Source>,
        failures: HashMap<Vec<String>, String>,
        loads: Vec<Vec<String>>,
    }

    impl TestLoader {
        fn source(&mut self, path: &[&str], code: &str) {
            let path = path.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            self.sources.insert(
                path.clone(),
                Source {
                    code: code.to_string(),
                    label: path.join("."),
                },
            );
        }

        fn failure(&mut self, path: &[&str], message: &str) {
            self.failures.insert(
                path.iter().map(|s| s.to_string()).collect(),
                message.to_string(),
            );
        }
    }

    impl SourceLoader for TestLoader {
        type FatalError = std::convert::Infallible;

        fn load(
            &mut self,
            module_path: &ModulePath,
        ) -> Result<Option<Source>, SourceLoadError<Self::FatalError>> {
            self.loads.push(module_path.segments().to_vec());
            if let Some(message) = self.failures.get(module_path.segments()) {
                return Err(SourceLoadError::LoadFailed(message.clone()));
            }
            Ok(self.sources.get(module_path.segments()).cloned())
        }
    }

    fn source(code: &str, label: &str) -> Source {
        Source {
            code: code.to_string(),
            label: label.to_string(),
        }
    }

    fn module_path(path: &[&str]) -> ModulePath {
        ModulePath::new(path.iter().map(|s| s.to_string()).collect()).unwrap()
    }

    fn module(path: &[&str], code: &str) -> ModuleInput {
        ModuleInput {
            path: module_path(path),
            source: source(code, &path.join(".")),
        }
    }

    fn check_source(source_code: &str) -> Result<super::CheckOk, CheckError> {
        let mut loader = TestLoader::default();
        check(ProgramInput {
            main: source(source_code, "main.anv"),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![],
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

    #[test]
    fn prelude_declarations_are_visible() {
        let mut loader = TestLoader::default();
        check(ProgramInput {
            main: source("fn main() { let x: int = prelude_value(); }", "main.anv"),
            prelude: Some(source("fn prelude_value() -> int { 1 }", "<prelude>")),
            preloaded_modules: vec![],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();
    }

    #[test]
    fn always_active_extend_is_visible_without_import() {
        let mut loader = TestLoader::default();
        check(ProgramInput {
            main: source("fn main() { let x: int = 1.plus_one(); }", "main.anv"),
            prelude: None,
            preloaded_modules: vec![module(
                &["core_int"],
                "pub extend int { fn plus_one(self) -> int { self + 1 } }",
            )],
            always_active_modules: vec![module_path(&["core_int"])],
            source_loader: &mut loader,
        })
        .unwrap();
    }

    #[test]
    fn duplicate_preloaded_modules_are_resolve_errors() {
        let mut loader = TestLoader::default();
        let err = check(ProgramInput {
            main: source("fn main() {}", "main.anv"),
            prelude: None,
            preloaded_modules: vec![module(&["core_int"], ""), module(&["core_int"], "")],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap_err();
        assert!(matches!(err, CheckError::Resolve { .. }));
        assert!(loader.loads.is_empty());
    }

    #[test]
    fn ordinary_preloaded_extend_is_not_visible_without_import() {
        let mut loader = TestLoader::default();
        let err = check(ProgramInput {
            main: source("fn main() { let x: int = 1.plus_one(); }", "main.anv"),
            prelude: None,
            preloaded_modules: vec![module(
                &["core_int"],
                "pub extend int { fn plus_one(self) -> int { self + 1 } }",
            )],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap_err();
        assert!(matches!(err, CheckError::Type { .. }));
    }

    #[test]
    fn always_active_module_names_are_not_imported() {
        let mut loader = TestLoader::default();
        let err = check(ProgramInput {
            main: source("fn main() { let x: int = hidden(); }", "main.anv"),
            prelude: None,
            preloaded_modules: vec![module(&["helpers"], "pub fn hidden() -> int { 1 }")],
            always_active_modules: vec![module_path(&["helpers"])],
            source_loader: &mut loader,
        })
        .unwrap_err();
        assert!(matches!(err, CheckError::Type { .. }));
    }

    #[test]
    fn local_import_resolved_through_loader() {
        let mut loader = TestLoader::default();
        loader.source(&["foo"], "pub fn bar() -> int { 1 }");
        check(ProgramInput {
            main: source(
                "import foo { bar }; fn main() { let x: int = bar(); }",
                "main.anv",
            ),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();
    }

    #[test]
    fn std_like_import_resolved_through_loader() {
        let mut loader = TestLoader::default();
        loader.source(&["std", "math"], "pub fn sqrt(x: float) -> float { x }");
        check(ProgramInput {
            main: source(
                "import std.math { sqrt }; fn main() { let x: float = sqrt(4.0); }",
                "main.anv",
            ),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();
    }

    #[test]
    fn loaded_module_parse_error_keeps_parse_phase() {
        let mut loader = TestLoader::default();
        loader.source(&["broken"], "fn nope( {}");
        let err = check(ProgramInput {
            main: source("import broken; fn main() {}", "main.anv"),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap_err();
        assert!(matches!(err, CheckError::Parse { label, .. } if label == "broken"));
    }

    #[test]
    fn loaded_module_lex_error_keeps_lex_phase() {
        let mut loader = TestLoader::default();
        loader.source(&["broken"], "fn main() { \"unterminated }");
        let err = check(ProgramInput {
            main: source("import broken; fn main() {}", "main.anv"),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap_err();
        assert!(matches!(err, CheckError::Lex { label, .. } if label == "broken"));
    }

    #[test]
    fn loader_failure_stays_resolve_error() {
        let mut loader = TestLoader::default();
        loader.failure(&["broken"], "disk error");
        let err = check(ProgramInput {
            main: source("import broken; fn main() {}", "main.anv"),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap_err();
        assert!(matches!(err, CheckError::Resolve { .. }));
    }
}
