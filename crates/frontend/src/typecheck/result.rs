use std::path::Path;

use super::{
    BindingPromotionMap, CompileWarning, ImportId, ImportRecord, LambdaCaptureMap, LambdaEscapeMap,
    ModuleScope, semantic_use::map_delta,
};
use crate::{
    ast::Visibility,
    diagnostic::DiagnosticTag,
    lint::{LintEvent, LintId},
    resolve::PackageId,
};

pub struct TypecheckResult {
    warnings: Vec<CompileWarning>,
    lint_events: Vec<LintEvent>,
    facts: TypecheckFacts,
}

impl TypecheckResult {
    pub(crate) fn new(
        warnings: Vec<CompileWarning>,
        lint_events: Vec<LintEvent>,
        facts: TypecheckFacts,
    ) -> Self {
        facts.validate();
        Self {
            warnings,
            lint_events,
            facts,
        }
    }

    pub(crate) fn into_parts(self) -> (Vec<CompileWarning>, Vec<LintEvent>, TypecheckFacts) {
        (self.warnings, self.lint_events, self.facts)
    }
}

#[derive(Clone, Default)]
pub struct TypecheckFacts {
    pub(super) lambda_escapes: LambdaEscapeMap,
    pub(super) lambda_captures: LambdaCaptureMap,
    pub(super) binding_promotions: BindingPromotionMap,
    pub(super) import_records: Vec<ImportRecord>,
    pub(super) used_imports: std::collections::HashSet<ImportId>,
}

impl TypecheckFacts {
    pub fn lambda_escapes(&self) -> &LambdaEscapeMap {
        &self.lambda_escapes
    }

    pub fn lambda_captures(&self) -> &LambdaCaptureMap {
        &self.lambda_captures
    }

    pub fn binding_promotions(&self) -> &BindingPromotionMap {
        &self.binding_promotions
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
            import_records: self.import_records.clone(),
            used_imports: self.used_imports.clone(),
        }
    }

    fn validate(&self) {
        for (expr_id, fact) in self.lambda_escapes() {
            debug_assert_eq!(*expr_id, fact.expr_id);
        }
        for ((lambda_id, binding_id), fact) in self.lambda_captures() {
            debug_assert_eq!(*lambda_id, fact.lambda_id);
            debug_assert_eq!(*binding_id, fact.binding_id);
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
        label: None,
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
