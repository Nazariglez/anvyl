use std::path::Path;

use super::{
    BindingPromotionMap, CompileWarning, ForStepRuntimeCheckMap, ImportId, ImportRecord,
    LambdaCaptureMap, LambdaEscapeMap, ModuleScope, NominalKey, SemanticFactMaps, TypeError,
    decls::DeclarationIndex, infer::SourceExprTypes, semantic_use::map_delta,
};
use crate::{
    ast::Visibility,
    diagnostic::DiagnosticTag,
    externs::catalog::ExternCatalog,
    lint::{LintEvent, LintId},
    resolve::PackageId,
};

#[derive(Clone)]
pub(crate) struct SemanticProgram {
    pub(crate) facts: SemanticFactMaps,
    pub(crate) declarations: DeclarationIndex,
    pub(crate) externs: ExternCatalog,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct TypeDiagnosticContext {
    core_option: Option<NominalKey>,
}

impl TypeDiagnosticContext {
    pub(crate) fn core_option(&self) -> Option<&NominalKey> {
        self.core_option.as_ref()
    }

    pub(crate) fn from_core_option(core_option: Option<NominalKey>) -> Self {
        Self { core_option }
    }

    pub(crate) fn from_decls(decls: &DeclarationIndex) -> Self {
        Self {
            core_option: decls.core_option_key(),
        }
    }
}

pub(crate) struct SemanticCheckOutput {
    pub(crate) warnings: Vec<CompileWarning>,
    pub(crate) lint_events: Vec<LintEvent>,
    pub(crate) diagnostic_context: TypeDiagnosticContext,
    pub(crate) public_facts: TypecheckFacts,
    pub(crate) source_types: SourceExprTypes,
    pub(crate) program: SemanticProgram,
}

#[derive(Debug)]
pub(crate) struct TypecheckFailure {
    pub(crate) errors: Vec<TypeError>,
    pub(crate) warnings: Vec<CompileWarning>,
    pub(crate) lint_events: Vec<LintEvent>,
    pub(crate) diagnostic_context: TypeDiagnosticContext,
}

impl TypecheckFailure {
    pub(crate) fn errors(errors: Vec<TypeError>) -> Self {
        Self {
            errors,
            warnings: vec![],
            lint_events: vec![],
            diagnostic_context: TypeDiagnosticContext::from_core_option(None),
        }
    }
}

pub struct TypecheckOutput {
    errors: Vec<TypeError>,
    warnings: Vec<CompileWarning>,
    lint_events: Vec<LintEvent>,
    diagnostic_context: TypeDiagnosticContext,
    facts: Option<TypecheckFacts>,
}

impl TypecheckOutput {
    pub(crate) fn success(
        warnings: Vec<CompileWarning>,
        lint_events: Vec<LintEvent>,
        diagnostic_context: TypeDiagnosticContext,
        facts: TypecheckFacts,
    ) -> Self {
        facts.validate();
        Self {
            errors: vec![],
            warnings,
            lint_events,
            diagnostic_context,
            facts: Some(facts),
        }
    }

    pub(crate) fn failed(
        errors: Vec<TypeError>,
        warnings: Vec<CompileWarning>,
        lint_events: Vec<LintEvent>,
        diagnostic_context: TypeDiagnosticContext,
    ) -> Self {
        debug_assert!(!errors.is_empty());
        Self {
            errors,
            warnings,
            lint_events,
            diagnostic_context,
            facts: None,
        }
    }

    pub(crate) fn into_parts(
        self,
    ) -> (
        Vec<TypeError>,
        Vec<CompileWarning>,
        Vec<LintEvent>,
        TypeDiagnosticContext,
        Option<TypecheckFacts>,
    ) {
        (
            self.errors,
            self.warnings,
            self.lint_events,
            self.diagnostic_context,
            self.facts,
        )
    }
}

#[derive(Clone, Default)]
pub struct TypecheckFacts {
    pub(super) lambda_escapes: LambdaEscapeMap,
    pub(super) lambda_captures: LambdaCaptureMap,
    pub(super) binding_promotions: BindingPromotionMap,
    pub(super) for_step_runtime_checks: ForStepRuntimeCheckMap,
    pub(super) import_records: Vec<ImportRecord>,
    pub(super) used_imports: std::collections::HashSet<ImportId>,
}

impl TypecheckFacts {
    pub(crate) fn from_semantic(output: SemanticCheckOutput) -> Self {
        output.program.facts.validate_finished();
        debug_assert!(output.source_types.values().all(|(span, _)| span.is_some()));
        debug_assert_eq!(
            output.program.declarations.import_records().len(),
            output.public_facts.import_records.len()
        );
        debug_assert!(
            output.program.externs.functions().all(|function| output
                .program
                .externs
                .function(function.id)
                == function)
        );
        output.public_facts
    }

    pub fn lambda_escapes(&self) -> &LambdaEscapeMap {
        &self.lambda_escapes
    }

    pub fn lambda_captures(&self) -> &LambdaCaptureMap {
        &self.lambda_captures
    }

    pub fn binding_promotions(&self) -> &BindingPromotionMap {
        &self.binding_promotions
    }

    pub fn for_step_runtime_checks(&self) -> &ForStepRuntimeCheckMap {
        &self.for_step_runtime_checks
    }

    pub(crate) fn unused_import_events(&self) -> Vec<LintEvent> {
        self.import_records
            .iter()
            .filter(|import| self.unused_import_candidate(import))
            .map(unused_import_event)
            .collect()
    }

    fn unused_import_candidate(&self, import: &ImportRecord) -> bool {
        import.visibility == Visibility::Private
            && !is_system_import(import)
            && !self.used_imports.contains(&import.id)
    }

    pub(crate) fn delta_since(&self, old: &Self) -> Self {
        Self {
            lambda_escapes: map_delta(&old.lambda_escapes, &self.lambda_escapes),
            lambda_captures: map_delta(&old.lambda_captures, &self.lambda_captures),
            binding_promotions: map_delta(&old.binding_promotions, &self.binding_promotions),
            for_step_runtime_checks: map_delta(
                &old.for_step_runtime_checks,
                &self.for_step_runtime_checks,
            ),
            import_records: self.import_records.clone(),
            used_imports: self.used_imports.clone(),
        }
    }

    pub(crate) fn validate(&self) {
        for (expr_id, fact) in self.lambda_escapes() {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for ((lambda_id, binding_id), fact) in self.lambda_captures() {
            debug_assert_eq!(*lambda_id, fact.lambda_id);
            debug_assert_eq!(*binding_id, fact.binding_id);
        }
        for span in self.for_step_runtime_checks().values() {
            debug_assert!(span.span.start <= span.span.end);
        }
        for (binding_id, fact) in self.binding_promotions() {
            debug_assert_eq!(*binding_id, fact.binding_id);
        }
    }
}

fn is_system_import(import: &ImportRecord) -> bool {
    let ModuleScope::Package(module) = &import.id.module else {
        return false;
    };
    module
        .package_context()
        .is_some_and(|package| package == &PackageId::core() || package == &PackageId::std())
}

fn unused_import_event(import: &ImportRecord) -> LintEvent {
    LintEvent {
        id: LintId::UnusedImport,
        span: import.span,
        message: format!(
            "unused import from '{}'",
            render_import_module(&import.target_module)
        ),
        label: "unused import".to_string(),
        notes: vec![],
        help: Some("remove this import".to_string()),
        tags: vec![DiagnosticTag::Unnecessary],
    }
}

fn render_import_module(module: &ModuleScope) -> String {
    match module {
        ModuleScope::Root => "<root>".to_string(),
        ModuleScope::Named(path) => path.segments().join("."),
        ModuleScope::Package(module) => module
            .module_path()
            .map(|path| path.segments().join("."))
            .or_else(|| {
                module
                    .source_file()
                    .map(|file| source_module_name(file.path()))
            })
            .unwrap_or_else(|| "<root>".to_string()),
    }
}

fn source_module_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("<source>")
        .to_string()
}
