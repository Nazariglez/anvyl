// Lexer -> Parser -> Resolver -> Typechecker

mod diagnostics;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use self::diagnostics::{
    diagnose_compile_warning, diagnose_conditional_error, diagnose_extern_input_error,
    diagnose_lex_error, diagnose_parse_error, diagnose_resolve_error, diagnose_type_error,
};
use crate::{
    air,
    ast::Program,
    conditional,
    config::{CompilationContext, LintConfig},
    externs::{self, ExternInputs},
    lexer,
    lint::{LintEvent, apply_lints},
    parser,
    resolve::{
        self, LoadedModule, LocalSourceLoad, LocalSourceRequest, ModuleId, ModuleLoadError,
        ModuleLoader, PackageId, PackageInput as ResolvePackageInput, PackageKind, PreloadedModule,
        ResolveFailure, SystemPackages,
    },
    source::{SourceId, SourceKind, SourceTable},
    typecheck,
};
pub use crate::{
    config::LintLevelOrigin,
    diagnostic::{
        Diagnostic, DiagnosticCode, DiagnosticCodeKind, DiagnosticLabel, DiagnosticProjection,
        DiagnosticReport, DiagnosticSeverity, DiagnosticTag, LabelStyle, Severity,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source {
    pub code: String,
    pub label: String,
    pub path: Option<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageModuleInput {
    pub module: ModuleId,
    pub source: Source,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct PackageSourceInput {
    pub root: Option<PackageModuleInput>,
    pub dependencies: HashMap<String, PackageId>,
    pub kind: PackageKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceLoadError<E> {
    LoadFailed(String),
    Fatal(E),
}

#[derive(Debug, Clone)]
pub enum SourceLoad {
    Loaded(PackageModuleInput),
    Missing { candidate: Option<PathBuf> },
}

pub trait PackageSourceLoader {
    type FatalError;

    fn load(
        &mut self,
        module: &ModuleId,
    ) -> Result<Option<PackageModuleInput>, SourceLoadError<Self::FatalError>>;

    fn load_local_source(
        &mut self,
        _request: &LocalSourceRequest,
    ) -> Result<SourceLoad, SourceLoadError<Self::FatalError>> {
        Ok(SourceLoad::Missing { candidate: None })
    }
}

pub struct PackageProgramInput<'a, L: PackageSourceLoader> {
    pub root_package: PackageId,
    pub main: PackageModuleInput,
    pub system: SystemPackages,
    pub packages: HashMap<PackageId, PackageSourceInput>,
    pub preloaded_modules: Vec<PackageModuleInput>,
    pub source_loader: &'a mut L,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct FrontendConfig {
    pub externs: ExternInputs,
    pub lint: LintConfig,
    pub context: CompilationContext,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckPhase {
    Lex,
    Parse,
    Resolve,
    Extern,
    Type,
}

impl CheckPhase {
    #[must_use]
    pub fn summary(self) -> &'static str {
        match self {
            Self::Lex => "Failed to lex program",
            Self::Parse => "Failed to parse program",
            Self::Resolve => "Failed to resolve program",
            Self::Extern => "Failed to ingest extern inputs",
            Self::Type => "Failed to typecheck program",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CheckStatus {
    Passed,
    Failed { phase: CheckPhase },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckOutput {
    pub report: DiagnosticReport,
    pub status: CheckStatus,
}

impl CheckOutput {
    #[must_use]
    pub fn passed(report: DiagnosticReport) -> Self {
        debug_assert!(!report.has_errors());
        Self {
            report,
            status: CheckStatus::Passed,
        }
    }

    #[must_use]
    pub fn failed(phase: CheckPhase, report: DiagnosticReport) -> Self {
        debug_assert!(report.has_errors());
        Self {
            report,
            status: CheckStatus::Failed { phase },
        }
    }

    #[must_use]
    pub fn has_errors(&self) -> bool {
        self.report.has_errors()
    }

    #[must_use]
    pub fn summary(&self) -> &'static str {
        match self.status {
            CheckStatus::Passed => "Program checked successfully",
            CheckStatus::Failed { phase } => phase.summary(),
        }
    }
}

enum PipelineStop<E> {
    Diagnostic(CheckOutput),
    Fatal(E),
}

impl<E> PipelineStop<E> {
    fn into_check_result(self) -> Result<CheckOutput, E> {
        match self {
            Self::Diagnostic(output) => Ok(output),
            Self::Fatal(error) => Err(error),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AirRootConfig {
    pub entry: Option<String>,
    pub callables: Vec<String>,
}

impl AirRootConfig {
    pub fn entry_main() -> Self {
        Self {
            entry: Some("main".to_string()),
            callables: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AirBuildOutput {
    pub report: DiagnosticReport,
    pub air: air::OwnedVerifiedProgram,
}

#[derive(Debug)]
pub enum AirBuildError<E> {
    Diagnostic(CheckOutput),
    Lower(String),
    Fatal(E),
}

pub fn check_packages<L: PackageSourceLoader>(
    input: PackageProgramInput<'_, L>,
    config: FrontendConfig,
) -> Result<CheckOutput, L::FatalError> {
    let prepared = match prepare_pipeline(input, config) {
        Ok(prepared) => prepared,
        Err(stop) => return stop.into_check_result(),
    };
    let report = typecheck_success_report(&prepared.sources, &prepared.lint, prepared.semantic);
    Ok(if report.has_errors() {
        CheckOutput::failed(CheckPhase::Type, report)
    } else {
        CheckOutput::passed(report)
    })
}

pub fn build_air_packages<L: PackageSourceLoader>(
    input: PackageProgramInput<'_, L>,
    config: FrontendConfig,
    roots: &AirRootConfig,
) -> Result<AirBuildOutput, AirBuildError<L::FatalError>> {
    let prepared = prepare_pipeline(input, config).map_err(air_build_stop)?;
    let facts = prepared.semantic.validated_public_facts();
    let report =
        typecheck_success_report_ref(&prepared.sources, &prepared.lint, &prepared.semantic);
    if report.has_errors() {
        return Err(AirBuildError::Diagnostic(CheckOutput::failed(
            CheckPhase::Type,
            report,
        )));
    }
    let air = air::lower::lower_with_source_index(
        prepared.source_index,
        &prepared.semantic.program,
        facts,
        air::lower::AirLowerConfig {
            roots: air::lower::AirRoots {
                entry: roots.entry.as_deref().map(|name| {
                    root_callable(&prepared.semantic.program, &prepared.root_module, name)
                }),
                callables: roots
                    .callables
                    .iter()
                    .map(|name| {
                        root_callable(&prepared.semantic.program, &prepared.root_module, name)
                    })
                    .collect(),
            },
        },
    )
    .map_err(|error| AirBuildError::Lower(format!("{error:?}")))?;
    let air = air::OwnedVerifiedProgram::new(air)
        .map_err(|errors| AirBuildError::Lower(format!("{errors:?}")))?;
    Ok(AirBuildOutput { report, air })
}

struct PreparedPipeline {
    sources: SourceTable,
    root_module: ModuleId,
    source_index: crate::source_ast::SourceAstIndex,
    semantic: typecheck::SemanticCheckOutput,
    lint: LintConfig,
}

fn prepare_pipeline<L: PackageSourceLoader>(
    input: PackageProgramInput<'_, L>,
    config: FrontendConfig,
) -> Result<PreparedPipeline, PipelineStop<L::FatalError>> {
    let FrontendConfig {
        externs: extern_inputs,
        lint,
        context,
    } = config;
    let mut sources = SourceTable::default();
    let root = parse_package_module(&mut sources, input.main, SourceKind::Root, &context)?;
    let packages = parse_package_inputs(&mut sources, input.packages, &context)?;
    let preloaded_modules = parse_package_modules(&mut sources, input.preloaded_modules, &context)?;

    let raw_externs = externs::ingest_providers(extern_inputs)
        .map_err(|errors| PipelineStop::Diagnostic(extern_failure(&sources, errors)))?;
    let external_modules = externs::raw_extern_module_ids(&raw_externs);

    let resolved = {
        let mut loader = InputModuleLoader::new(input.source_loader, &mut sources, context.clone());
        loader.cache_loaded(root.clone());
        for package in packages.values() {
            if let Some(root) = &package.root {
                loader.cache_loaded(root.clone());
            }
        }
        for module in &preloaded_modules {
            loader.cache_loaded(LoadedModule {
                module: module.module.clone(),
                source: module.source,
                program: module.program.clone(),
            });
        }
        resolve::resolve_package_modules(
            root.clone(),
            &packages,
            preloaded_modules,
            &mut loader,
            &HashSet::new(),
            &external_modules,
            input.system.clone(),
        )
    };
    let resolved = match resolved {
        Ok(resolved) => resolved,
        Err(ResolveFailure::Fatal(stop)) => return Err(stop),
        Err(ResolveFailure::Resolve(errors)) => {
            let report = diagnostic_report(&sources, errors.iter().map(diagnose_resolve_error));
            return Err(PipelineStop::Diagnostic(CheckOutput::failed(
                CheckPhase::Resolve,
                report,
            )));
        }
    };

    let raw_externs = externs::prepare_raw_externs(raw_externs, &root.program, &resolved)
        .map_err(|errors| PipelineStop::Diagnostic(extern_failure(&sources, errors)))?;

    let source_index = crate::source_ast::SourceAstIndex::new(&root.program, &resolved);
    let semantic = typecheck::check_semantic_with_source_index(
        &root.program,
        &resolved,
        &source_index,
        raw_externs,
        typecheck::TypecheckConfig { context },
    )
    .map_err(|failure| {
        let report = typecheck_failure_report(&sources, &lint, failure);
        PipelineStop::Diagnostic(CheckOutput::failed(CheckPhase::Type, report))
    })?;

    Ok(PreparedPipeline {
        sources,
        root_module: root.module,
        source_index,
        semantic,
        lint,
    })
}

fn air_build_stop<E>(stop: PipelineStop<E>) -> AirBuildError<E> {
    match stop {
        PipelineStop::Diagnostic(output) => AirBuildError::Diagnostic(output),
        PipelineStop::Fatal(error) => AirBuildError::Fatal(error),
    }
}

fn root_callable(
    semantic: &typecheck::SemanticProgram,
    module: &ModuleId,
    name: &str,
) -> typecheck::CallableInstanceKey {
    let scope = typecheck::ModuleScope::from_module_id(module);
    if let Some(fact) = semantic.declaration_facts.functions.iter().find(|fact| {
        fact.id.name.as_str() == name
            && fact.id.module == scope
            && fact.id.parent.is_none()
            && fact.id.kind == typecheck::CallableKind::Function
    }) {
        typecheck::CallableInstanceKey {
            target: fact.id.clone(),
            args: fact.args.clone(),
        }
    } else {
        typecheck::CallableInstanceKey {
            target: typecheck::CallableId::function(scope, crate::ast::Ident::new(name)),
            args: typecheck::GenericArgs::default(),
        }
    }
}

fn extern_failure(sources: &SourceTable, errors: Vec<externs::ExternInputError>) -> CheckOutput {
    let report = diagnostic_report(
        sources,
        errors
            .into_iter()
            .map(|error| diagnose_extern_input_error(&error)),
    );
    CheckOutput::failed(CheckPhase::Extern, report)
}

fn typecheck_failure_report(
    sources: &SourceTable,
    lint: &LintConfig,
    failure: typecheck::TypecheckFailure,
) -> DiagnosticReport {
    let diagnostic_context = failure
        .diagnostic_context
        .clone()
        .with_sources(sources.clone());
    let diagnostics = failure
        .errors
        .iter()
        .map(|error| diagnose_type_error(error, &diagnostic_context))
        .chain(failure.warnings.iter().map(diagnose_compile_warning))
        .chain(apply_lints(lint, failure.lint_events));
    diagnostic_report(sources, diagnostics)
}

fn typecheck_success_report(
    sources: &SourceTable,
    lint: &LintConfig,
    mut semantic: typecheck::SemanticCheckOutput,
) -> DiagnosticReport {
    let warnings = std::mem::take(&mut semantic.warnings);
    let lint_events = std::mem::take(&mut semantic.lint_events);
    let facts = typecheck::TypecheckFacts::from_semantic(semantic);
    typecheck_success_report_parts(sources, lint, &warnings, lint_events, &facts)
}

fn typecheck_success_report_ref(
    sources: &SourceTable,
    lint: &LintConfig,
    semantic: &typecheck::SemanticCheckOutput,
) -> DiagnosticReport {
    typecheck_success_report_parts(
        sources,
        lint,
        &semantic.warnings,
        semantic.lint_events.clone(),
        &semantic.public_facts,
    )
}

fn typecheck_success_report_parts(
    sources: &SourceTable,
    lint: &LintConfig,
    warnings: &[typecheck::CompileWarning],
    mut lint_events: Vec<LintEvent>,
    facts: &typecheck::TypecheckFacts,
) -> DiagnosticReport {
    lint_events.extend(facts.unused_import_events());
    let diagnostics = warnings
        .iter()
        .map(diagnose_compile_warning)
        .chain(apply_lints(lint, lint_events));
    diagnostic_report(sources, diagnostics)
}

fn diagnostic_report(
    sources: &SourceTable,
    diagnostics: impl IntoIterator<Item = Diagnostic>,
) -> DiagnosticReport {
    DiagnosticReport::new(sources.clone(), diagnostics.into_iter().collect()).sorted()
}

fn parse_package_inputs<E>(
    sources: &mut SourceTable,
    packages: HashMap<PackageId, PackageSourceInput>,
    ctx: &CompilationContext,
) -> Result<HashMap<PackageId, ResolvePackageInput>, PipelineStop<E>> {
    let mut packages = packages.into_iter().collect::<Vec<_>>();
    packages.sort_by(|(left, _), (right, _)| left.cmp(right));
    packages
        .into_iter()
        .map(|(id, package)| {
            let root = package
                .root
                .map(|module| {
                    parse_package_module(
                        sources,
                        module,
                        SourceKind::PackageRoot {
                            package: id.clone(),
                        },
                        ctx,
                    )
                })
                .transpose()?;
            Ok((
                id,
                ResolvePackageInput {
                    root,
                    dependencies: package.dependencies,
                    kind: package.kind,
                },
            ))
        })
        .collect()
}

fn parse_package_module<E>(
    sources: &mut SourceTable,
    module: PackageModuleInput,
    kind: SourceKind,
    ctx: &CompilationContext,
) -> Result<LoadedModule, PipelineStop<E>> {
    let parsed = parse_source(sources, &module.source, kind, ctx)?;
    Ok(LoadedModule {
        module: module.module,
        source: parsed.id,
        program: parsed.program,
    })
}

fn parse_package_modules<E>(
    sources: &mut SourceTable,
    modules: Vec<PackageModuleInput>,
    ctx: &CompilationContext,
) -> Result<Vec<PreloadedModule>, PipelineStop<E>> {
    modules
        .into_iter()
        .map(|module| {
            let parsed = parse_source(sources, &module.source, SourceKind::Prelude, ctx)?;
            Ok(PreloadedModule {
                module: module.module,
                source: parsed.id,
                program: parsed.program,
            })
        })
        .collect()
}

struct ParsedSource {
    id: SourceId,
    program: Program,
}

fn parse_source<E>(
    sources: &mut SourceTable,
    source: &Source,
    kind: SourceKind,
    ctx: &CompilationContext,
) -> Result<ParsedSource, PipelineStop<E>> {
    let source_id = register_module_source(sources, source, kind);
    let code = match conditional::filter_with_context(&source.code, ctx) {
        Ok(code) => code,
        Err(errors) => {
            let report = diagnostic_report(
                sources,
                errors
                    .iter()
                    .map(|error| diagnose_conditional_error(source_id, error)),
            );
            return Err(PipelineStop::Diagnostic(CheckOutput::failed(
                CheckPhase::Parse,
                report,
            )));
        }
    };

    let tokens = match lexer::tokenize(source_id, &code) {
        Ok(tokens) => tokens,
        Err(errors) => {
            let report = diagnostic_report(
                sources,
                errors
                    .iter()
                    .map(|error| diagnose_lex_error(source_id, source.code.len(), error)),
            );
            return Err(PipelineStop::Diagnostic(CheckOutput::failed(
                CheckPhase::Lex,
                report,
            )));
        }
    };

    let program = match parser::parse_ast(&tokens) {
        Ok(program) => program,
        Err(errors) => {
            let report = diagnostic_report(sources, errors.iter().map(diagnose_parse_error));
            return Err(PipelineStop::Diagnostic(CheckOutput::failed(
                CheckPhase::Parse,
                report,
            )));
        }
    };
    Ok(ParsedSource {
        id: source_id,
        program,
    })
}

fn register_module_source(
    sources: &mut SourceTable,
    source: &Source,
    kind: SourceKind,
) -> SourceId {
    sources.add(
        kind,
        source.label.clone(),
        source.path.clone(),
        source.code.clone(),
    )
}

struct InputModuleLoader<'a, L: PackageSourceLoader> {
    loader: &'a mut L,
    sources: &'a mut SourceTable,
    parsed: HashMap<ModuleId, LoadedModule>,
    context: CompilationContext,
}

impl<'a, L: PackageSourceLoader> InputModuleLoader<'a, L> {
    fn new(loader: &'a mut L, sources: &'a mut SourceTable, context: CompilationContext) -> Self {
        Self {
            loader,
            sources,
            parsed: HashMap::new(),
            context,
        }
    }

    fn cache_loaded(&mut self, module: LoadedModule) {
        self.parsed.insert(module.module.clone(), module);
    }

    fn parse_loaded(
        &mut self,
        module: PackageModuleInput,
    ) -> Result<LoadedModule, ModuleLoadError<PipelineStop<L::FatalError>>> {
        if let Some(loaded) = self.parsed.get(&module.module) {
            return Ok(loaded.clone());
        }
        let kind = SourceKind::Module {
            module: module.module.clone(),
        };
        let loaded = parse_package_module(self.sources, module, kind, &self.context)
            .map_err(ModuleLoadError::Fatal)?;
        self.cache_loaded(loaded.clone());
        Ok(loaded)
    }
}

fn module_load_error<E>(error: SourceLoadError<E>) -> ModuleLoadError<PipelineStop<E>> {
    match error {
        SourceLoadError::LoadFailed(message) => ModuleLoadError::LoadFailed(message),
        SourceLoadError::Fatal(error) => ModuleLoadError::Fatal(PipelineStop::Fatal(error)),
    }
}

impl<L: PackageSourceLoader> ModuleLoader for InputModuleLoader<'_, L> {
    type FatalError = PipelineStop<L::FatalError>;

    fn load(
        &mut self,
        module: &ModuleId,
    ) -> Result<Option<LoadedModule>, ModuleLoadError<Self::FatalError>> {
        let Some(loaded) = self.loader.load(module).map_err(module_load_error)? else {
            return Ok(None);
        };

        self.parse_loaded(loaded).map(Some)
    }

    fn load_local_source(
        &mut self,
        request: &LocalSourceRequest,
    ) -> Result<LocalSourceLoad, ModuleLoadError<Self::FatalError>> {
        let loaded = match self
            .loader
            .load_local_source(request)
            .map_err(module_load_error)?
        {
            SourceLoad::Loaded(loaded) => loaded,
            SourceLoad::Missing { candidate } => {
                return Ok(LocalSourceLoad::Missing { candidate });
            }
        };

        self.parse_loaded(loaded).map(LocalSourceLoad::Loaded)
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, convert::Infallible};

    use super::{
        CheckOutput, CheckPhase, CheckStatus, Diagnostic, DiagnosticReport, FrontendConfig,
        PackageModuleInput, PackageProgramInput, PackageSourceInput, PackageSourceLoader, Source,
        SourceLoadError, check_packages as pipeline_check,
    };
    use crate::{
        externs::{ExternInputs, PackageExternInputs},
        resolve::{ModuleId, PackageId, PackageKind, SystemPackages},
        source::{SourceFile, SourceKind},
        test_support::module_path_segments,
    };

    #[derive(Default)]
    struct TestLoader {
        sources: HashMap<(PackageId, Vec<String>), Source>,
        failures: HashMap<Vec<String>, String>,
        loads: Vec<Vec<String>>,
    }

    impl TestLoader {
        fn source(&mut self, path: &[&str], code: &str) {
            self.package_source(&root_package(), path, code);
        }

        fn package_source(&mut self, package: &PackageId, path: &[&str], code: &str) {
            let path = path.iter().map(ToString::to_string).collect::<Vec<_>>();
            self.sources.insert(
                (package.clone(), path.clone()),
                Source {
                    code: code.to_string(),
                    label: path.join("."),
                    path: None,
                },
            );
        }

        fn failure(&mut self, path: &[&str], message: &str) {
            self.failures.insert(
                path.iter().map(ToString::to_string).collect(),
                message.to_string(),
            );
        }
    }

    impl PackageSourceLoader for TestLoader {
        type FatalError = Infallible;

        fn load(
            &mut self,
            module: &ModuleId,
        ) -> Result<Option<PackageModuleInput>, SourceLoadError<Self::FatalError>> {
            let Some(path) = module.named_path() else {
                return Ok(None);
            };
            self.loads.push(path.segments().to_vec());
            if let Some(message) = self.failures.get(path.segments()) {
                return Err(SourceLoadError::LoadFailed(message.clone()));
            }
            Ok(self
                .sources
                .get(&(module.package().clone(), path.segments().to_vec()))
                .cloned()
                .map(|source| PackageModuleInput {
                    module: module.clone(),
                    source,
                }))
        }
    }

    fn source(code: &str, label: &str) -> Source {
        Source {
            code: code.to_string(),
            label: label.to_string(),
            path: None,
        }
    }

    fn root_package() -> PackageId {
        PackageId::synthetic_root()
    }

    fn module_id(path: &[&str]) -> ModuleId {
        ModuleId::named(root_package(), module_path_segments(path))
    }

    fn extern_inputs(providers: Vec<anvyx_externs::ProviderDescriptor>) -> ExternInputs {
        package_extern_inputs(root_package(), providers)
    }

    fn package_extern_inputs(
        package: PackageId,
        providers: Vec<anvyx_externs::ProviderDescriptor>,
    ) -> ExternInputs {
        ExternInputs {
            packages: vec![PackageExternInputs { package, providers }],
        }
    }

    fn module(path: &[&str], code: &str) -> PackageModuleInput {
        PackageModuleInput {
            module: module_id(path),
            source: source(code, &path.join(".")),
        }
    }

    fn root_source(package: &PackageId, code: &str, label: &str) -> PackageModuleInput {
        PackageModuleInput {
            module: ModuleId::root(package.clone()),
            source: source(code, label),
        }
    }

    fn input<L: PackageSourceLoader>(
        loader: &mut L,
        main: Source,
        prelude: Option<Source>,
        preloaded_modules: Vec<PackageModuleInput>,
    ) -> PackageProgramInput<'_, L> {
        let root_package = root_package();
        let core_package = prelude.as_ref().map(|_| PackageId::core());
        let mut packages = HashMap::from([(root_package.clone(), PackageSourceInput::default())]);
        if let (Some(package), Some(root)) = (core_package.clone(), prelude) {
            packages.insert(
                package.clone(),
                PackageSourceInput {
                    root: Some(PackageModuleInput {
                        module: ModuleId::root(package),
                        source: root,
                    }),
                    dependencies: HashMap::new(),
                    kind: PackageKind::Source,
                },
            );
        }
        let mut preloaded_modules = preloaded_modules;
        if let Some(root) = core_package.clone().and_then(|package| {
            packages
                .get(&package)
                .and_then(|package| package.root.clone())
        }) {
            preloaded_modules.push(root);
        }
        PackageProgramInput {
            root_package: root_package.clone(),
            main: PackageModuleInput {
                module: ModuleId::root(root_package.clone()),
                source: main,
            },
            system: SystemPackages {
                core: core_package,
                std: None,
            },
            packages,
            preloaded_modules,
            source_loader: loader,
        }
    }

    fn package_input(
        package: &PackageId,
        root: &str,
        dependencies: &[(&str, PackageId)],
    ) -> PackageSourceInput {
        PackageSourceInput {
            root: Some(PackageModuleInput {
                module: ModuleId::root(package.clone()),
                source: source(root, "package.anv"),
            }),
            dependencies: dependency_map(dependencies),
            kind: PackageKind::Source,
        }
    }

    fn native_package_input(dependencies: &[(&str, PackageId)]) -> PackageSourceInput {
        PackageSourceInput {
            root: None,
            dependencies: dependency_map(dependencies),
            kind: PackageKind::NativeOnly,
        }
    }

    fn dependency_map(dependencies: &[(&str, PackageId)]) -> HashMap<String, PackageId> {
        dependencies
            .iter()
            .map(|(alias, package)| ((*alias).to_string(), package.clone()))
            .collect()
    }

    fn check<L: PackageSourceLoader>(
        input: PackageProgramInput<'_, L>,
    ) -> Result<CheckOutput, L::FatalError> {
        pipeline_check(input, FrontendConfig::default())
    }

    fn check_source(source_code: &str) -> Result<CheckOutput, Infallible> {
        check_source_with_config(source_code, FrontendConfig::default())
    }

    fn check_source_with_config(
        source_code: &str,
        config: FrontendConfig,
    ) -> Result<CheckOutput, Infallible> {
        let mut loader = TestLoader::default();
        pipeline_check(
            input(&mut loader, source(source_code, "main.anv"), None, vec![]),
            config,
        )
    }

    #[test]
    fn accepts_valid_provider_descriptors() {
        let mut loader = TestLoader::default();
        let output = pipeline_check(
            input(
                &mut loader,
                source("fn main() {}", "main.anv"),
                None,
                vec![],
            ),
            FrontendConfig {
                externs: extern_inputs(vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();
        assert_passed(&output);
    }

    #[test]
    fn provider_only_module_import_does_not_resolve() {
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            input(
                &mut loader,
                source("import math { dot }; fn main() {}", "main.anv"),
                None,
                vec![],
            ),
            FrontendConfig {
                externs: extern_inputs(vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        assert_failed(&err, CheckPhase::Resolve);
    }
    #[test]
    fn native_only_dependency_root_import_is_resolve_error() {
        let game = PackageId::new("game");
        let host = PackageId::new("host");
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            PackageProgramInput {
                root_package: game.clone(),
                main: root_source(&game, "import pkg:host; fn main() {}", "main.anv"),
                system: SystemPackages::default(),
                packages: HashMap::from([
                    (
                        game.clone(),
                        PackageSourceInput {
                            root: None,
                            dependencies: dependency_map(&[("host", host.clone())]),
                            kind: PackageKind::Source,
                        },
                    ),
                    (host.clone(), native_package_input(&[])),
                ]),
                preloaded_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: package_extern_inputs(host, vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        assert_failed(&err, CheckPhase::Resolve);
    }

    #[test]
    fn source_native_dependency_provider_is_hidden_without_reexport() {
        let game = PackageId::new("game");
        let math = PackageId::new("math");
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            PackageProgramInput {
                root_package: game.clone(),
                main: root_source(
                    &game,
                    "import pkg:math.math { dot }; fn main() { let x: float = dot(); }",
                    "main.anv",
                ),
                system: SystemPackages::default(),
                packages: HashMap::from([
                    (
                        game.clone(),
                        PackageSourceInput {
                            root: None,
                            dependencies: dependency_map(&[("math", math.clone())]),
                            kind: PackageKind::Source,
                        },
                    ),
                    (math.clone(), package_input(&math, "", &[])),
                ]),
                preloaded_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: package_extern_inputs(math, vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        assert_failed(&err, CheckPhase::Type);
    }

    #[test]
    fn source_native_dependency_provider_module_reexport_typechecks() {
        let game = PackageId::new("game");
        let math = PackageId::new("math");
        let mut loader = TestLoader::default();
        let output = pipeline_check(
            PackageProgramInput {
                root_package: game.clone(),
                main: root_source(
                    &game,
                    "import pkg:math.math { dot }; fn main() { let x: float = dot(); }",
                    "main.anv",
                ),
                system: SystemPackages::default(),
                packages: HashMap::from([
                    (
                        game.clone(),
                        PackageSourceInput {
                            root: None,
                            dependencies: dependency_map(&[("math", math.clone())]),
                            kind: PackageKind::Source,
                        },
                    ),
                    (
                        math.clone(),
                        package_input(&math, "pub import ext:math;", &[]),
                    ),
                ]),
                preloaded_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: package_extern_inputs(math, vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();
        assert_passed(&output);
    }

    #[test]
    fn source_native_dependency_provider_member_reexport_typechecks() {
        let game = PackageId::new("game");
        let math = PackageId::new("math");
        let mut loader = TestLoader::default();
        let output = pipeline_check(
            PackageProgramInput {
                root_package: game.clone(),
                main: root_source(
                    &game,
                    "import pkg:math { dot }; fn main() { let x: float = dot(); }",
                    "main.anv",
                ),
                system: SystemPackages::default(),
                packages: HashMap::from([
                    (
                        game.clone(),
                        PackageSourceInput {
                            root: None,
                            dependencies: dependency_map(&[("math", math.clone())]),
                            kind: PackageKind::Source,
                        },
                    ),
                    (
                        math.clone(),
                        package_input(&math, "pub import ext:math { dot };", &[]),
                    ),
                ]),
                preloaded_modules: vec![],
                source_loader: &mut loader,
            },
            FrontendConfig {
                externs: package_extern_inputs(math, vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();
        assert_passed(&output);
    }

    #[test]
    fn unknown_ext_module_is_resolve_error() {
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            input(
                &mut loader,
                source("import ext:audio; fn main() {}", "main.anv"),
                None,
                vec![],
            ),
            FrontendConfig {
                externs: extern_inputs(vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        assert_failed(&err, CheckPhase::Resolve);
    }

    #[test]
    fn provider_import_does_not_hide_loader_failure() {
        let mut loader = TestLoader::default();
        loader.failure(&["math"], "disk error");
        let err = pipeline_check(
            input(
                &mut loader,
                source("import math { dot }; fn main() {}", "main.anv"),
                None,
                vec![],
            ),
            FrontendConfig {
                externs: extern_inputs(vec![valid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        assert_failed(&err, CheckPhase::Resolve);
    }

    #[test]
    fn invalid_provider_descriptor_is_extern_failure() {
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            input(
                &mut loader,
                source("fn main() {}", "main.anv"),
                None,
                vec![],
            ),
            FrontendConfig {
                externs: extern_inputs(vec![invalid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        let report = assert_failed(&err, CheckPhase::Extern);
        assert_eq!(
            diagnostic_messages(report.diagnostics()),
            [
                "invalid extern descriptor from provider 'math' in package '<root>': duplicate function 'dot' in module 'math'"
            ]
        );
    }

    #[test]
    fn duplicate_provider_externs_are_rejected_before_typechecking() {
        let mut provider = valid_provider_descriptor();
        provider.provider.name = "other_math".to_string();
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            input(
                &mut loader,
                source("fn main() { missing; }", "main.anv"),
                None,
                vec![],
            ),
            FrontendConfig {
                externs: extern_inputs(vec![valid_provider_descriptor(), provider]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        let report = assert_failed(&err, CheckPhase::Extern);
        assert_eq!(
            diagnostic_messages(report.diagnostics()),
            [
                "duplicate provider module 'math' in package '<root>' declared by providers 'math' and 'other_math'"
            ]
        );
    }

    #[test]
    fn new_source_extern_parser_errors_take_precedence() {
        let err = check_source("extern type T { computed let x: int; } fn main() {}").unwrap();

        assert_failed(&err, CheckPhase::Parse);
    }

    #[test]
    fn invalid_unary_operand_is_parse_error() {
        let err = check_source("extern type T { op -int -> int; }").unwrap();

        assert_failed(&err, CheckPhase::Parse);
    }

    #[test]
    fn parse_errors_take_precedence_over_provider_errors() {
        let mut loader = TestLoader::default();
        let err = pipeline_check(
            input(&mut loader, source("fn main( {}", "main.anv"), None, vec![]),
            FrontendConfig {
                externs: extern_inputs(vec![invalid_provider_descriptor()]),
                ..FrontendConfig::default()
            },
        )
        .unwrap();

        assert_failed(&err, CheckPhase::Parse);
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

    fn assert_passed(output: &CheckOutput) {
        assert_eq!(output.status, CheckStatus::Passed);
        assert!(!output.has_errors());
    }

    fn assert_failed(output: &CheckOutput, phase: CheckPhase) -> &DiagnosticReport {
        assert_eq!(output.status, CheckStatus::Failed { phase });
        assert!(output.has_errors());
        &output.report
    }

    fn assert_primary_label(report: &DiagnosticReport) {
        let file = report.sources.iter().next().expect("expected source");
        let label = &report.diagnostics()[0].labels()[0];
        assert_eq!(label.span.source(), file.id());
    }

    fn assert_primary_label_message(report: &DiagnosticReport, message: &str) {
        assert_eq!(
            report.diagnostics()[0]
                .primary_label()
                .unwrap()
                .message
                .as_deref(),
            Some(message)
        );
    }

    #[test]
    fn renders_lex_errors_through_check() {
        let err = check_source("fn main() { \"unterminated }").unwrap();
        let report = assert_failed(&err, CheckPhase::Lex);
        assert_user_diagnostics(report.diagnostics());
        assert_primary_label(report);
    }

    #[test]
    fn lex_error_report_carries_registered_source() {
        let err = check_source("fn main() { \"unterminated }").unwrap();
        let report = assert_failed(&err, CheckPhase::Lex);
        let file = report.sources.iter().next().expect("expected source");

        assert_eq!(report.sources.len(), 1);
        assert_eq!(file.label(), "main.anv");
        assert_eq!(file.text(), "fn main() { \"unterminated }");
    }

    #[test]
    fn renders_parse_errors_through_check() {
        let err = check_source("fn main( {}").unwrap();
        let report = assert_failed(&err, CheckPhase::Parse);
        assert_user_diagnostics(report.diagnostics());
        assert_primary_label(report);
    }

    #[test]
    fn conditional_errors_have_source_labels() {
        let source = "#if platform(macos)\n#end\nfn main() {}";
        let err = check_source(source).unwrap();
        let report = assert_failed(&err, CheckPhase::Parse);
        let label = report.diagnostics()[0].primary_label().unwrap();

        assert_eq!(
            report.diagnostics()[0].message(),
            "unknown conditional predicate"
        );
        assert_eq!(label.message.as_deref(), Some("unknown predicate"));
        assert_eq!(&source[label.span.start()..label.span.end()], "platform");
    }

    #[test]
    fn eof_parse_error_uses_empty_eof_label() {
        let source = "fn";
        let err = check_source(source).unwrap();
        let report = assert_failed(&err, CheckPhase::Parse);
        assert_primary_label(report);
        let label = &report.diagnostics()[0].labels()[0];
        assert_eq!(label.span.start(), source.len());
        assert_eq!(label.span.end(), source.len());
    }

    #[test]
    fn renders_resolve_errors_through_check() {
        let err = check_source("import missing as m; fn main() {} ").unwrap();
        let report = assert_failed(&err, CheckPhase::Resolve);
        assert!(
            report.diagnostics()[0]
                .message()
                .contains("Cannot find module file")
        );
        assert_user_diagnostics(report.diagnostics());
        assert_primary_label(report);
    }

    #[test]
    fn renders_type_errors_through_check() {
        let err = check_source("fn main() { let x: int = true; } ").unwrap();
        let report = assert_failed(&err, CheckPhase::Type);
        assert_eq!(report.diagnostics()[0].message(), "mismatched types");
        assert_user_diagnostics(report.diagnostics());
        assert_primary_label(report);
        assert_primary_label_message(report, "expected `int`, found `bool`");
    }

    #[test]
    fn missing_method_through_check_has_no_call_cascade() {
        let err = check_source("fn main() { 1.foo(); }").unwrap();
        let report = assert_failed(&err, CheckPhase::Type);
        let diagnostics = report.diagnostics();
        assert_eq!(diagnostics.len(), 1, "diagnostics: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].message(),
            "Unknown method 'foo' for type 'int'"
        );
    }

    #[test]
    fn poisoned_chain_through_check_has_no_cascade() {
        let err = check_source("fn main() { let x: int = missing.foo(1).bar; }").unwrap();
        let report = assert_failed(&err, CheckPhase::Type);
        let diagnostics = report.diagnostics();

        assert_eq!(diagnostics.len(), 1, "diagnostics: {diagnostics:?}");
        assert_eq!(diagnostics[0].message(), "unknown variable");
    }

    #[test]
    fn poisoned_compound_expression_has_no_mismatch_cascade() {
        let err = check_source("fn main() { let x: int = [missing]; }").unwrap();
        let report = assert_failed(&err, CheckPhase::Type);
        let diagnostics = report.diagnostics();

        assert_eq!(diagnostics.len(), 1, "diagnostics: {diagnostics:?}");
        assert_eq!(diagnostics[0].message(), "unknown variable");
    }

    #[test]
    fn poisoned_compound_receiver_has_no_member_cascade() {
        let err = check_source("fn main() { [missing].foo(); }").unwrap();
        let report = assert_failed(&err, CheckPhase::Type);
        let diagnostics = report.diagnostics();

        assert_eq!(diagnostics.len(), 1, "diagnostics: {diagnostics:?}");
        assert_eq!(diagnostics[0].message(), "unknown variable");
    }

    #[test]
    fn poisoned_tuple_index_target_has_no_cascade() {
        let err = check_source("fn main() { let x: int = (missing).0; }").unwrap();
        let report = assert_failed(&err, CheckPhase::Type);
        let diagnostics = report.diagnostics();

        assert_eq!(diagnostics.len(), 1, "diagnostics: {diagnostics:?}");
        assert_eq!(diagnostics[0].message(), "unknown variable");
    }

    #[test]
    fn poisoned_operator_operand_has_no_cascade() {
        for source in [
            "fn main() { let x: int = missing + 1; }",
            "fn main() { let x: int = -missing; }",
        ] {
            let err = check_source(source).unwrap();
            let report = assert_failed(&err, CheckPhase::Type);
            let diagnostics = report.diagnostics();

            assert_eq!(diagnostics.len(), 1, "diagnostics: {diagnostics:?}");
            assert_eq!(diagnostics[0].message(), "unknown variable");
        }
    }

    #[test]
    fn report_registers_package_roots_and_preloaded_modules() {
        let game = PackageId::new("game");
        let dep = PackageId::new("dep");
        let mut loader = TestLoader::default();

        let ok = check(PackageProgramInput {
            root_package: game.clone(),
            main: root_source(&game, "fn main() {}", "main.anv"),
            system: SystemPackages::default(),
            packages: HashMap::from([(dep.clone(), package_input(&dep, "", &[]))]),
            preloaded_modules: vec![module(&["helper"], "pub fn h() -> int { 1 }")],
            source_loader: &mut loader,
        })
        .unwrap();
        assert_passed(&ok);

        assert_eq!(ok.report.sources.len(), 3);
        assert!(
            ok.report
                .sources
                .iter()
                .any(|file| matches!(file.kind(), SourceKind::Root))
        );
        assert!(ok.report.sources.iter().any(|file| matches!(
            file.kind(),
            SourceKind::PackageRoot { package } if package == &dep
        )));
        assert!(
            ok.report
                .sources
                .iter()
                .any(|file| matches!(file.kind(), SourceKind::Prelude))
        );
    }

    #[test]
    fn repeated_module_load_reuses_source_id() {
        let mut loader = TestLoader::default();
        loader.source(&["foo"], "pub fn a() -> int { 1 } pub fn b() -> int { 2 }");

        let ok = check(input(
            &mut loader,
            source(
                "import foo { a }; import foo { b }; fn main() { let x: int = a() + b(); }",
                "main.anv",
            ),
            None,
            vec![],
        ))
        .unwrap();
        assert_passed(&ok);
        let labels = ok
            .report
            .sources
            .iter()
            .map(SourceFile::label)
            .collect::<Vec<_>>();

        assert_eq!(loader.loads, [vec!["foo".to_string()]]);
        assert_eq!(ok.report.sources.len(), 2);
        assert_eq!(labels.iter().filter(|label| **label == "foo").count(), 1);
    }
    #[test]
    fn duplicate_preloaded_modules_are_resolve_errors() {
        let mut loader = TestLoader::default();
        let err = check(input(
            &mut loader,
            source("fn main() {}", "main.anv"),
            None,
            vec![module(&["core_int"], ""), module(&["core_int"], "")],
        ))
        .unwrap();
        let report = assert_failed(&err, CheckPhase::Resolve);
        assert_eq!(
            diagnostic_messages(report.diagnostics()),
            ["module 'core_int' is preloaded more than once"]
        );
        assert!(loader.loads.is_empty());
    }

    #[test]
    fn ordinary_preloaded_extend_is_not_visible_without_import() {
        let mut loader = TestLoader::default();
        let err = check(input(
            &mut loader,
            source("fn main() { let x: int = 1.plus_one(); }", "main.anv"),
            None,
            vec![module(
                &["core_int"],
                "pub extend int { fn plus_one(self) -> int { self + 1 } }",
            )],
        ))
        .unwrap();
        assert_failed(&err, CheckPhase::Type);
    }

    #[test]
    fn preloaded_module_names_are_not_imported() {
        let mut loader = TestLoader::default();
        let err = check(input(
            &mut loader,
            source("fn main() { let x: int = hidden(); }", "main.anv"),
            None,
            vec![module(&["helpers"], "pub fn hidden() -> int { 1 }")],
        ))
        .unwrap();
        assert_failed(&err, CheckPhase::Type);
    }
    #[test]
    fn core_root_is_not_preluded_into_core_modules() {
        let core = PackageId::core();
        let mut loader = TestLoader::default();
        loader.package_source(&core, &["helper"], "pub fn leak(x: RootOnly) {}");

        let err = check(PackageProgramInput {
            root_package: core.clone(),
            main: root_source(
                &core,
                "struct RootOnly {} import helper { leak }; fn main() {}",
                "<core>",
            ),
            system: SystemPackages {
                core: Some(core.clone()),
                std: None,
            },
            packages: HashMap::from([(core, PackageSourceInput::default())]),
            preloaded_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();

        assert_failed(&err, CheckPhase::Type);
    }

    #[test]
    fn dependency_private_module_is_not_importable() {
        let game = PackageId::new("game");
        let physics = PackageId::new("physics");
        let mut loader = TestLoader::default();
        loader.package_source(&physics, &["internals"], "pub struct Hidden {}");

        let err = check(PackageProgramInput {
            root_package: game.clone(),
            main: root_source(
                &game,
                "import pkg:physics.internals { Hidden };",
                "main.anv",
            ),
            system: SystemPackages::default(),
            packages: HashMap::from([
                (
                    game.clone(),
                    package_input(&game, "", &[("physics", physics.clone())]),
                ),
                (
                    physics.clone(),
                    package_input(&physics, "import internals { Hidden };", &[]),
                ),
            ]),
            preloaded_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();

        assert_failed(&err, CheckPhase::Type);
    }

    #[test]
    fn dependency_module_reexport_controls_public_path() {
        let game = PackageId::new("game");
        let physics = PackageId::new("physics");
        let mut loader = TestLoader::default();
        loader.package_source(&physics, &["types"], "pub struct Vec2 {}");

        let output = check(PackageProgramInput {
            root_package: game.clone(),
            main: root_source(&game, "import pkg:physics.types { Vec2 };", "main.anv"),
            system: SystemPackages::default(),
            packages: HashMap::from([
                (
                    game.clone(),
                    package_input(&game, "", &[("physics", physics.clone())]),
                ),
                (
                    physics.clone(),
                    package_input(&physics, "pub import types;", &[]),
                ),
            ]),
            preloaded_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();
        assert_passed(&output);
    }

    #[test]
    fn dependency_root_source_extern_is_package_scoped() {
        let game = PackageId::new("game");
        let native = PackageId::new("native");
        let mut loader = TestLoader::default();

        let output = check(PackageProgramInput {
            root_package: game.clone(),
            main: root_source(
                &game,
                "import pkg:native { tick }; fn main() { let x: int = tick(); }",
                "main.anv",
            ),
            system: SystemPackages::default(),
            packages: HashMap::from([
                (
                    game.clone(),
                    package_input(&game, "", &[("native", native.clone())]),
                ),
                (
                    native.clone(),
                    package_input(&native, "pub extern fn tick() -> int;", &[]),
                ),
            ]),
            preloaded_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();
        assert_passed(&output);
    }

    #[test]
    fn dependency_named_source_extern_is_package_scoped() {
        let game = PackageId::new("game");
        let native = PackageId::new("native");
        let mut loader = TestLoader::default();
        loader.package_source(&native, &["host"], "pub extern fn tick() -> int;");

        let output = check(PackageProgramInput {
            root_package: game.clone(),
            main: root_source(
                &game,
                "import pkg:native.host { tick }; fn main() { let x: int = tick(); }",
                "main.anv",
            ),
            system: SystemPackages::default(),
            packages: HashMap::from([
                (
                    game.clone(),
                    package_input(&game, "", &[("native", native.clone())]),
                ),
                (
                    native.clone(),
                    package_input(&native, "pub import host;", &[]),
                ),
            ]),
            preloaded_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();
        assert_passed(&output);
    }

    #[test]
    fn dependency_exported_module_self_alias_uses_shared_import_target() {
        let game = PackageId::new("game");
        let physics = PackageId::new("physics");
        let mut loader = TestLoader::default();
        loader.package_source(
            &physics,
            &["types"],
            "pub struct Vec2 {} pub fn make() -> Vec2 { Vec2 {} }",
        );

        let output = check(PackageProgramInput {
            root_package: game.clone(),
            main: root_source(
                &game,
                "import pkg:physics.types { self as types, Vec2 }; fn main() { let x: Vec2 = types.make(); }",
                "main.anv",
            ),
            system: SystemPackages::default(),
            packages: HashMap::from([
                (
                    game.clone(),
                    package_input(&game, "", &[("physics", physics.clone())]),
                ),
                (physics.clone(), package_input(&physics, "pub import types;", &[])),
            ]),
            preloaded_modules: vec![],
            source_loader: &mut loader,
        })
        .unwrap();
        assert_passed(&output);
    }
    #[test]
    fn std_import_reports_unsupported_root() {
        let mut loader = TestLoader::default();
        let err = check(input(
            &mut loader,
            source(
                "import std:math { sqrt }; fn main() { let x: float = sqrt(4.0); }",
                "main.anv",
            ),
            None,
            vec![],
        ))
        .unwrap();

        let report = assert_failed(&err, CheckPhase::Resolve);
        assert_eq!(
            report.diagnostics()[0].message(),
            "import root 'std' is not supported yet"
        );
    }

    #[test]
    fn loaded_module_parse_error_keeps_parse_phase() {
        let mut loader = TestLoader::default();
        loader.source(&["broken"], "fn nope( {}");
        let err = check(input(
            &mut loader,
            source("import broken; fn main() {}", "main.anv"),
            None,
            vec![],
        ))
        .unwrap();
        let report = assert_failed(&err, CheckPhase::Parse);
        assert!(
            report
                .sources
                .iter()
                .any(|source| source.label() == "broken")
        );
    }

    #[test]
    fn loaded_module_lex_error_keeps_lex_phase() {
        let mut loader = TestLoader::default();
        loader.source(&["broken"], "fn main() { \"unterminated }");
        let err = check(input(
            &mut loader,
            source("import broken; fn main() {}", "main.anv"),
            None,
            vec![],
        ))
        .unwrap();
        let report = assert_failed(&err, CheckPhase::Lex);
        assert!(
            report
                .sources
                .iter()
                .any(|source| source.label() == "broken")
        );
    }

    #[test]
    fn loader_failure_stays_resolve_error() {
        let mut loader = TestLoader::default();
        loader.failure(&["broken"], "disk error");
        let err = check(input(
            &mut loader,
            source("import broken; fn main() {}", "main.anv"),
            None,
            vec![],
        ))
        .unwrap();
        let report = assert_failed(&err, CheckPhase::Resolve);
        assert_eq!(
            diagnostic_messages(report.diagnostics()),
            ["Cannot load module 'broken': disk error"]
        );
        assert_user_diagnostics(report.diagnostics());
    }
}
