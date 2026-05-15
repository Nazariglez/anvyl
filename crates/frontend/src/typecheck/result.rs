use super::{
    BindingPromotionMap, CompileWarning, ImportId, ImportRecord, LambdaCaptureMap, LambdaEscapeMap,
    map_delta,
};
use crate::lint::LintEvent;

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

    pub(crate) fn import_records(&self) -> &[ImportRecord] {
        &self.import_records
    }

    pub(crate) fn used_imports(&self) -> &std::collections::HashSet<ImportId> {
        &self.used_imports
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
