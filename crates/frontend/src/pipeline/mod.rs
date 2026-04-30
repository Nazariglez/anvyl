// Lexer -> Parser -> Resolver -> Typechecker -> AIR lowering

mod diagnostics;
use std::collections::HashSet;

pub use diagnostics::Diagnostic;

use self::diagnostics::{
    diagnose_extern_input_error, diagnose_lex_error, diagnose_parse_error, diagnose_resolve_error,
    diagnose_type_error, diagnose_unresolved_always_active_module,
};
use crate::{
    ast::Program,
    externs::{self, ExternInputs},
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

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FrontendConfig {
    pub externs: ExternInputs,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckOk;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckError<E = std::convert::Infallible> {
    Lex {
        label: String,
        diagnostics: Vec<Diagnostic>,
    },
    Parse {
        label: String,
        diagnostics: Vec<Diagnostic>,
    },
    Resolve {
        diagnostics: Vec<Diagnostic>,
    },
    Type {
        diagnostics: Vec<Diagnostic>,
    },
    Extern {
        diagnostics: Vec<Diagnostic>,
    },
    Source(Box<E>),
}

pub fn check<L: SourceLoader>(
    input: ProgramInput<'_, L>,
    config: FrontendConfig,
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

    let mut raw_externs = externs::ingest_providers(config.externs).map_err(extern_error)?;
    let external_modules = externs::raw_extern_module_paths(&raw_externs);

    let mut loader = InputModuleLoader::new(input.source_loader);
    let resolved = match resolve::resolve_modules(
        root.clone(),
        preloaded_modules,
        &mut loader,
        &HashSet::new(),
        &external_modules,
    ) {
        Ok(resolved) => resolved,
        Err(ResolveFailure::Fatal(error)) => return Err(error),
        Err(ResolveFailure::Resolve(errors)) => {
            return Err(CheckError::Resolve {
                diagnostics: errors.iter().map(diagnose_resolve_error).collect(),
            });
        }
    };

    validate_always_active_modules(&resolved, &always_active_modules)?;

    let source_externs = externs::collect_source_externs(&root, &resolved).map_err(extern_error)?;
    raw_externs.append(source_externs);
    externs::validate_raw_shapes(&raw_externs).map_err(extern_error)?;
    externs::validate_raw_identities(&raw_externs).map_err(extern_error)?;

    typecheck::check_with_modules(&root, &resolved, always_active_modules, raw_externs).map_err(
        |errors| CheckError::Type {
            diagnostics: errors.iter().map(diagnose_type_error).collect(),
        },
    )?;

    Ok(CheckOk)
}

fn extern_error<E>(errors: Vec<externs::ExternInputError>) -> CheckError<E> {
    CheckError::Extern {
        diagnostics: errors
            .into_iter()
            .map(|error| diagnose_extern_input_error(&error))
            .collect(),
    }
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

    let diagnostics = always_active_modules
        .iter()
        .filter(|module| !resolved_modules.contains(module))
        .map(diagnose_unresolved_always_active_module)
        .collect::<Vec<_>>();

    if diagnostics.is_empty() {
        Ok(())
    } else {
        Err(CheckError::Resolve { diagnostics })
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
        diagnostics: errors.iter().map(diagnose_lex_error).collect(),
    })?;

    parser::parse_ast(&tokens).map_err(|errors| CheckError::Parse {
        label: source.label.clone(),
        diagnostics: errors.iter().map(diagnose_parse_error).collect(),
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
        CheckError, CheckOk, Diagnostic, FrontendConfig, ModuleInput, ProgramInput, Source,
        SourceLoadError, SourceLoader, check as pipeline_check,
    };
    use crate::{externs::ExternInputs, resolve::ModulePath};

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

    fn check<L: SourceLoader>(
        input: ProgramInput<'_, L>,
    ) -> Result<CheckOk, CheckError<L::FatalError>> {
        pipeline_check(input, FrontendConfig::default())
    }

    fn check_source(source_code: &str) -> Result<CheckOk, CheckError> {
        let mut loader = TestLoader::default();
        check(ProgramInput {
            main: source(source_code, "main.anv"),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![],
            source_loader: &mut loader,
        })
    }

    fn extern_messages(source: &str) -> Vec<String> {
        let CheckError::Extern { diagnostics } = check_source(source).unwrap_err() else {
            panic!("expected extern error");
        };
        diagnostic_messages(&diagnostics)
    }

    #[test]
    fn default_config_has_empty_externs() {
        assert_eq!(
            FrontendConfig::default(),
            FrontendConfig {
                externs: ExternInputs::default(),
            }
        );
    }

    #[test]
    fn accepts_valid_provider_descriptors() {
        let mut loader = TestLoader::default();
        pipeline_check(
            ProgramInput {
                main: source("fn main() {}", "main.anv"),
                prelude: None,
                preloaded_modules: vec![],
                always_active_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: ExternInputs {
                    providers: vec![valid_provider_descriptor()],
                },
            },
        )
        .unwrap();
    }

    #[test]
    fn provider_only_module_import_resolves() {
        let mut loader = TestLoader::default();
        pipeline_check(
            ProgramInput {
                main: source("import math { dot }; fn main() {}", "main.anv"),
                prelude: None,
                preloaded_modules: vec![],
                always_active_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: ExternInputs {
                    providers: vec![valid_provider_descriptor()],
                },
            },
        )
        .unwrap();
    }

    #[test]
    fn provider_import_does_not_hide_loader_failure() {
        let mut loader = TestLoader::default();
        loader.failure(&["math"], "disk error");
        let err = pipeline_check(
            ProgramInput {
                main: source("import math { dot }; fn main() {}", "main.anv"),
                prelude: None,
                preloaded_modules: vec![],
                always_active_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: ExternInputs {
                    providers: vec![valid_provider_descriptor()],
                },
            },
        )
        .unwrap_err();

        assert!(matches!(err, CheckError::Resolve { .. }));
    }

    #[test]
    fn invalid_provider_descriptor_is_extern_error() {
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            ProgramInput {
                main: source("fn main() {}", "main.anv"),
                prelude: None,
                preloaded_modules: vec![],
                always_active_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: ExternInputs {
                    providers: vec![invalid_provider_descriptor()],
                },
            },
        )
        .unwrap_err();

        let CheckError::Extern { diagnostics } = err else {
            panic!("expected extern error");
        };
        assert_eq!(
            diagnostic_messages(&diagnostics),
            [
                "invalid extern descriptor from provider 'math': duplicate function 'dot' in module 'math'"
            ]
        );
    }

    #[test]
    fn duplicate_raw_extern_identities_are_extern_errors() {
        assert_eq!(
            extern_messages("extern fn f() -> void; extern fn f() -> void;"),
            ["duplicate extern function '<root>.f' declared in source root and source root"]
        );
    }

    #[test]
    fn duplicate_provider_externs_are_rejected_before_typechecking() {
        let mut provider = valid_provider_descriptor();
        provider.provider.name = "other_math".to_string();
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            ProgramInput {
                main: source("fn main() { missing; }", "main.anv"),
                prelude: None,
                preloaded_modules: vec![],
                always_active_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: ExternInputs {
                    providers: vec![valid_provider_descriptor(), provider],
                },
            },
        )
        .unwrap_err();

        let CheckError::Extern { diagnostics } = err else {
            panic!("expected extern error");
        };
        assert_eq!(
            diagnostic_messages(&diagnostics),
            [
                "duplicate extern function 'math.dot' declared in provider 'math' and provider 'other_math'"
            ]
        );
    }

    #[test]
    fn source_extern_normalization_errors_are_extern_errors() {
        assert_eq!(
            extern_messages("extern fn f(x: (int, int)) -> void;"),
            ["unsupported source extern type '(int, int)'"]
        );
    }

    #[test]
    fn source_extern_shape_errors_are_extern_errors() {
        assert_eq!(
            extern_messages("extern fn f(x: void) -> void;"),
            [
                "invalid extern descriptor from source root: void type is not allowed in parameter position"
            ]
        );
    }

    #[test]
    fn source_extern_new_forms_reach_typechecking() {
        check_source(
            r#"
            extern type Vec2 rep inline {
                init(x: float, y: float);
                x: float;
                var y: float;
                let length: float;
                computed bounds: Rect;
                computed var label: string;
                fn magnitude(shared self) -> float;
                fn translate(var self, dx: float, dy: float) -> void;
                fn zero() -> Self;
                op Self != Self -> bool;
                op Self < Self -> bool;
                op Self > Self -> bool;
                op Self <= Self -> bool;
                op Self >= Self -> bool;
            }
            extern type Rect;
            fn main() {}
            "#,
        )
        .unwrap();
    }

    #[test]
    fn new_source_extern_shape_errors_are_extern_errors() {
        let err =
            check_source("extern type T { op Self < Self -> int; } fn main() {}").unwrap_err();

        assert!(matches!(err, CheckError::Extern { .. }));
    }

    #[test]
    fn new_source_extern_parser_errors_take_precedence() {
        let err = check_source("extern type T { computed let x: int; } fn main() {}").unwrap_err();

        assert!(matches!(err, CheckError::Parse { .. }));
    }

    #[test]
    fn renders_root_scope() {
        assert_eq!(
            extern_messages("extern type T { op Self + int -> void; }"),
            [
                "invalid extern descriptor from source root: invalid operator '+' on extern type '<root>.T': expected non-void return type, found 'void'"
            ]
        );
    }

    #[test]
    fn param_decorations_are_extern_errors() {
        assert_eq!(
            extern_messages("extern fn f(var x: int) -> void;"),
            [
                "unsupported source extern parameter 'x': mutable parameters are not supported in source extern declarations"
            ]
        );
    }

    #[test]
    fn invalid_unary_operand_is_parse_error() {
        let err = check_source("extern type T { op -int -> int; }").unwrap_err();

        assert!(matches!(err, CheckError::Parse { .. }));
    }

    #[test]
    fn source_only_program_without_externs_still_passes() {
        check_source("fn main() {}").unwrap();
    }

    #[test]
    fn parse_errors_take_precedence_over_provider_errors() {
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            ProgramInput {
                main: source("fn main( {}", "main.anv"),
                prelude: None,
                preloaded_modules: vec![],
                always_active_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: ExternInputs {
                    providers: vec![invalid_provider_descriptor()],
                },
            },
        )
        .unwrap_err();

        assert!(matches!(err, CheckError::Parse { .. }));
    }

    fn valid_provider_descriptor() -> anvyx_externs::ProviderDescriptor {
        anvyx_externs::ProviderDescriptor {
            provider: anvyx_externs::ProviderId {
                name: "math".to_string(),
            },
            modules: vec![anvyx_externs::ExternModuleDescriptor {
                path: extern_module_path(&["math"]),
                types: vec![],
                functions: vec![anvyx_externs::ExternFunctionDescriptor {
                    name: "dot".to_string(),
                    doc: None,
                    signature: anvyx_externs::ExternSignature {
                        params: vec![],
                        ret: anvyx_externs::ExternTypeExpr::Float,
                    },
                    effects: anvyx_externs::ExternEffects::default(),
                }],
            }],
        }
    }

    fn invalid_provider_descriptor() -> anvyx_externs::ProviderDescriptor {
        let mut provider = valid_provider_descriptor();
        let function = provider.modules[0].functions[0].clone();
        provider.modules[0].functions.push(function);
        provider
    }

    fn extern_module_path(path: &[&str]) -> anvyx_externs::ModulePath {
        anvyx_externs::ModulePath {
            segments: path.iter().map(|segment| (*segment).to_string()).collect(),
        }
    }

    fn diagnostic_messages(diagnostics: &[Diagnostic]) -> Vec<String> {
        diagnostics.iter().map(ToString::to_string).collect()
    }

    fn assert_user_diagnostics(diagnostics: &[Diagnostic]) {
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn renders_lex_errors_through_check() {
        let err = check_source("fn main() { \"unterminated }").unwrap_err();
        let CheckError::Lex { diagnostics, .. } = err else {
            panic!("expected lex error");
        };
        assert_user_diagnostics(&diagnostics);
    }

    #[test]
    fn renders_parse_errors_through_check() {
        let err = check_source("fn main( {}").unwrap_err();
        let CheckError::Parse { diagnostics, .. } = err else {
            panic!("expected parse error");
        };
        assert_user_diagnostics(&diagnostics);
    }

    #[test]
    fn renders_resolve_errors_through_check() {
        let err = check_source("import missing as m; fn main() {} ").unwrap_err();
        let CheckError::Resolve { diagnostics } = err else {
            panic!("expected resolve error");
        };
        assert!(diagnostics[0].message().contains("Cannot find module file"));
        assert_user_diagnostics(&diagnostics);
    }

    #[test]
    fn renders_type_errors_through_check() {
        let err = check_source("fn main() { let x: int = true; } ").unwrap_err();
        let CheckError::Type { diagnostics } = err else {
            panic!("expected type error");
        };
        assert_eq!(
            diagnostics[0].message(),
            "Mismatched types: expected 'int', found 'bool'"
        );
        assert_user_diagnostics(&diagnostics);
    }

    #[test]
    fn missing_method_through_check_has_no_call_cascade() {
        let err = check_source("fn main() { 1.foo(); }").unwrap_err();
        let CheckError::Type { diagnostics } = err else {
            panic!("expected type error");
        };
        assert_eq!(diagnostics.len(), 1, "diagnostics: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].message(),
            "Unknown method 'foo' for type 'int'"
        );
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
    fn unresolved_always_active_module_is_resolve_error() {
        let mut loader = TestLoader::default();
        let err = check(ProgramInput {
            main: source("fn main() {}", "main.anv"),
            prelude: None,
            preloaded_modules: vec![],
            always_active_modules: vec![module_path(&["missing", "helpers"])],
            source_loader: &mut loader,
        })
        .unwrap_err();
        let CheckError::Resolve { diagnostics } = err else {
            panic!("expected resolve error");
        };
        assert_eq!(
            diagnostic_messages(&diagnostics),
            ["always-active module was not resolved: missing.helpers"]
        );
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
        let CheckError::Resolve { diagnostics } = err else {
            panic!("expected resolve error");
        };
        assert_eq!(
            diagnostic_messages(&diagnostics),
            ["module 'core_int' is preloaded more than once"]
        );
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
        let CheckError::Resolve { diagnostics } = err else {
            panic!("expected resolve error");
        };
        assert_eq!(
            diagnostic_messages(&diagnostics),
            ["Cannot load module 'broken': disk error"]
        );
        assert_user_diagnostics(&diagnostics);
    }
}
