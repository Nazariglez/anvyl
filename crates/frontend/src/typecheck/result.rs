use std::path::Path;

use super::{
    CaptureCellRequirementMap, CompileWarning, ImportId, ImportRecord, IterRuntimeCheckKind,
    IterRuntimeCheckMap, LambdaCaptureMap, LambdaEscapeMap, ModuleScope, NominalKey,
    SemanticDeclarations, SemanticFactMaps, TypeError, contract_surface::ContractSurfaceSchemas,
    decls::DeclarationIndex, infer::SourceExprTypes, semantic_use::map_delta,
};
use crate::{
    ast::{ExprId, Visibility},
    diagnostic::DiagnosticTag,
    externs::catalog::ExternCatalog,
    lint::{LintEvent, LintId},
    resolve::PackageId,
};

#[derive(Clone)]
pub(crate) struct SemanticProgram {
    pub(crate) facts: SemanticFactMaps,
    pub(crate) declaration_facts: SemanticDeclarations,
    pub(crate) contract_surfaces: ContractSurfaceSchemas,
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
    pub(crate) public_facts: TypecheckFacts,
    pub(crate) source_types: SourceExprTypes,
    pub(crate) program: SemanticProgram,
}

#[derive(Debug)]
pub(crate) struct TypecheckFailure {
    pub(crate) errors: Vec<TypeError>,
    pub(crate) warnings: Vec<CompileWarning>,
    pub(crate) lint_events: Vec<LintEvent>,
    pub(crate) diagnostic_context: Box<TypeDiagnosticContext>,
}

impl TypecheckFailure {
    pub(crate) fn errors(errors: Vec<TypeError>) -> Self {
        Self {
            errors,
            warnings: vec![],
            lint_events: vec![],
            diagnostic_context: Box::new(TypeDiagnosticContext::from_core_option(None)),
        }
    }
}

#[cfg(test)]
pub struct TypecheckOutput {
    errors: Vec<TypeError>,
    warnings: Vec<CompileWarning>,
    lint_events: Vec<LintEvent>,
    diagnostic_context: TypeDiagnosticContext,
    facts: Option<TypecheckFacts>,
}

#[cfg(test)]
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
    pub(super) capture_cell_requirements: CaptureCellRequirementMap,
    pub(super) iter_runtime_checks: IterRuntimeCheckMap,
    pub(super) import_records: Vec<ImportRecord>,
    pub(super) used_imports: std::collections::HashSet<ImportId>,
}

impl SemanticCheckOutput {
    pub(crate) fn validated_public_facts(&self) -> &TypecheckFacts {
        validate_semantic_facts(self);
        &self.public_facts
    }
}

impl TypecheckFacts {
    pub(crate) fn from_semantic(output: SemanticCheckOutput) -> Self {
        validate_semantic_facts(&output);
        output.public_facts
    }

    pub(crate) fn lambda_escapes(&self) -> &LambdaEscapeMap {
        &self.lambda_escapes
    }

    pub(crate) fn lambda_captures(&self) -> &LambdaCaptureMap {
        &self.lambda_captures
    }

    pub(crate) fn capture_cell_requirements(&self) -> &CaptureCellRequirementMap {
        &self.capture_cell_requirements
    }

    pub(crate) fn iter_runtime_check(&self, expr_id: ExprId) -> Option<IterRuntimeCheckKind> {
        self.iter_runtime_checks.get(&expr_id).map(|fact| fact.kind)
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
            capture_cell_requirements: map_delta(
                &old.capture_cell_requirements,
                &self.capture_cell_requirements,
            ),
            iter_runtime_checks: map_delta(&old.iter_runtime_checks, &self.iter_runtime_checks),
            import_records: self.import_records.clone(),
            used_imports: self.used_imports.clone(),
        }
    }

    #[cfg(test)]
    pub(crate) fn validate(&self) {
        for (expr_id, fact) in &self.lambda_escapes {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for ((lambda_id, binding_id), fact) in &self.lambda_captures {
            debug_assert_eq!(*lambda_id, fact.lambda_id);
            debug_assert_eq!(*binding_id, fact.binding_id);
        }
        for (expr_id, fact) in &self.iter_runtime_checks {
            debug_assert_eq!(*expr_id, fact.expr);
        }
        for (binding_id, fact) in &self.capture_cell_requirements {
            debug_assert_eq!(*binding_id, fact.binding_id);
        }
    }
}

fn validate_semantic_facts(output: &SemanticCheckOutput) {
    output.program.facts.validate_finished();
    output
        .program
        .declaration_facts
        .validate_bodies(&output.program.facts);
    output.program.declaration_facts.validate();
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
