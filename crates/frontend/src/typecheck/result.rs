use super::TypeWarning;

pub(crate) struct TypecheckResult {
    pub(crate) warnings: Vec<TypeWarning>,
}

impl TypecheckResult {
    pub(crate) fn warnings(&self) -> &[TypeWarning] {
        &self.warnings
    }
}
