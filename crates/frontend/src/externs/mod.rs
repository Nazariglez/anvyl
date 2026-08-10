pub(crate) mod catalog;
mod raw;
mod source;

use std::collections::HashSet;

pub(crate) use raw::{
    ExternInputError, RawExternDecl, RawExternIdentityKey, RawExternModule, RawExternSite,
    RawExterns, UnsupportedSourceKind, UnsupportedSourceParamReason,
};
pub(crate) use source::collect_source_externs;

use crate::{
    resolve::{ModuleId, ModulePath as ResolveModulePath, PackageId},
    typecheck::ModuleScope,
};

pub(crate) fn extern_module_path(path: &anvyx_externs::ModulePath) -> ResolveModulePath {
    ResolveModulePath::from_extern_path(path).expect("raw extern module paths are validated")
}

pub(crate) fn provider_module_scope(
    package: &anvyx_externs::ProviderPackageKey,
    path: &anvyx_externs::ModulePath,
) -> ModuleScope {
    ModuleScope::from_module_id(&ModuleId::provider(
        PackageId::new(package.0.clone()),
        extern_module_path(path),
    ))
}

pub(crate) fn raw_module_scope(module: &ModuleId) -> ModuleScope {
    ModuleScope::from_module_id(module)
}

pub(crate) fn prepare_raw_externs(
    root: &crate::ast::Program,
    resolved: &crate::resolve::ResolveResult,
) -> Result<RawExterns, Vec<ExternInputError>> {
    collect_source_externs(root, resolved)
}

pub(crate) fn provider_module_ids(catalog: &anvyx_externs::ProviderCatalog) -> HashSet<ModuleId> {
    catalog
        .modules()
        .map(|(package, _, module)| {
            ModuleId::provider(
                PackageId::new(package.0.clone()),
                extern_module_path(&module.path),
            )
        })
        .collect()
}
