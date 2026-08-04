pub(crate) mod catalog;
mod identity;
mod providers;
mod raw;
mod shape;
mod source;

use std::collections::HashSet;

pub(crate) use identity::validate_raw_identities;
pub(crate) use providers::ingest_providers;
pub(crate) use raw::{
    ExternInputError, ExternProvenance, RawExternDecl, RawExternFunctionKey, RawExternIdentityKey,
    RawExternMemberKey, RawExternModule, RawExternScope, RawExternSite, RawExternTypeKey,
    RawExterns, UnsupportedSourceKind, UnsupportedSourceParamReason,
};
pub use raw::{ExternInputs, PackageExternInputs};
pub(crate) use shape::validate_raw_shapes;
pub(crate) use source::collect_source_externs;

use crate::{resolve::ModulePath as ResolveModulePath, typecheck::ModuleScope};

pub(crate) fn extern_module_path(path: &anvyx_externs::ModulePath) -> ResolveModulePath {
    ResolveModulePath::from_extern_path(path).expect("raw extern module paths are validated")
}

pub(crate) fn extern_module_scope(path: &anvyx_externs::ModulePath) -> ModuleScope {
    ModuleScope::Named(extern_module_path(path))
}

pub(crate) fn raw_module_scope(scope: &RawExternScope) -> ModuleScope {
    match scope {
        RawExternScope::Module(module) => ModuleScope::from_module_id(module),
    }
}

pub(crate) fn prepare_raw_externs(
    mut provider_raw: RawExterns,
    root: &crate::ast::Program,
    resolved: &crate::resolve::ResolveResult,
) -> Result<RawExterns, Vec<ExternInputError>> {
    let source_raw = collect_source_externs(root, resolved)?;
    provider_raw.append(source_raw);
    validate_raw_shapes(&provider_raw)?;
    validate_raw_identities(&provider_raw)?;
    Ok(provider_raw)
}

pub(crate) fn raw_extern_module_ids(raw: &RawExterns) -> HashSet<crate::resolve::ModuleId> {
    raw.groups
        .iter()
        .flat_map(|group| &group.modules)
        .map(|module| match &module.scope {
            RawExternScope::Module(module) => module.clone(),
        })
        .collect()
}
