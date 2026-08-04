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
        &prepared.source_index,
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
