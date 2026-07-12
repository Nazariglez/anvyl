use std::collections::{HashMap, hash_map::Entry};

use anvyx_externs::{
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternInitDescriptor, ExternMethodDescriptor,
    ExternModuleDescriptor, ExternOperatorDescriptor, ExternStaticDescriptor, ExternTypeDescriptor,
    ModulePath, ProviderDescriptor, ProviderId, validate,
};

use super::raw::*;
use crate::resolve::{ModuleId, ModulePath as ResolveModulePath, PackageId};

pub(crate) fn ingest_providers(inputs: ExternInputs) -> Result<RawExterns, Vec<ExternInputError>> {
    let mut groups = vec![];
    let mut errors = vec![];
    let mut modules = HashMap::<(PackageId, ModulePath), ProviderId>::new();

    for package in inputs.packages {
        for provider in package.providers {
            if let Err(provider_errors) = validate(&provider) {
                errors.extend(provider_errors.into_iter().map(|error| {
                    ExternInputError::InvalidProviderDescriptor {
                        package: package.package.clone(),
                        provider: provider.provider.clone(),
                        error,
                    }
                }));
                continue;
            }

            if let Some(error) = check_provider_modules(&mut modules, &package.package, &provider) {
                errors.push(error);
            } else {
                groups.push(normalize_provider(&package.package, provider));
            }
        }
    }

    if errors.is_empty() {
        Ok(RawExterns { groups })
    } else {
        Err(errors)
    }
}

fn check_provider_modules(
    modules: &mut HashMap<(PackageId, ModulePath), ProviderId>,
    package: &PackageId,
    provider: &ProviderDescriptor,
) -> Option<ExternInputError> {
    for module in &provider.modules {
        let key = (package.clone(), module.path.clone());
        match modules.entry(key) {
            Entry::Vacant(entry) => {
                entry.insert(provider.provider.clone());
            }
            Entry::Occupied(entry) => {
                return Some(ExternInputError::DuplicateProviderModule {
                    package: package.clone(),
                    module: module.path.clone(),
                    first: entry.get().clone(),
                    duplicate: provider.provider.clone(),
                });
            }
        }
    }
    None
}

fn normalize_provider(package: &PackageId, provider: ProviderDescriptor) -> RawExternGroup {
    RawExternGroup {
        provenance: ExternProvenance::Provider {
            package: package.clone(),
            provider: provider.provider,
        },
        modules: provider
            .modules
            .into_iter()
            .map(|module| normalize_module(package, module))
            .collect(),
    }
}

fn normalize_module(package: &PackageId, module: ExternModuleDescriptor) -> RawExternModule {
    let path = ResolveModulePath::new(module.path.segments.clone())
        .expect("provider module paths are validated before normalization");
    RawExternModule {
        scope: RawExternScope::Module(ModuleId::provider(package.clone(), path)),
        types: module.types.into_iter().map(normalize_type).collect(),
        functions: module
            .functions
            .into_iter()
            .map(normalize_function)
            .collect(),
    }
}

pub(super) fn normalize_type(ty: ExternTypeDescriptor) -> RawExternType {
    RawExternType {
        name: ty.name,
        doc: ty.doc,
        exported: true,
        rep: ty.rep,
        layout: ty.layout,
        materialization: ty.materialization,
        owns_heap_edges: ty.owns_heap_edges,
        fields: ty.fields.into_iter().map(normalize_field).collect(),
        variants: ty.variants,
        init: ty.init.map(normalize_init),
        methods: ty.methods.into_iter().map(normalize_method).collect(),
        statics: ty.statics.into_iter().map(normalize_static).collect(),
        operators: ty.operators.into_iter().map(normalize_operator).collect(),
        site: RawExternSite::default(),
    }
}

fn normalize_function(decl: ExternFunctionDescriptor) -> RawExternFunction {
    RawExternFunction {
        decl,
        exported: true,
        site: RawExternSite::default(),
    }
}

fn normalize_field(decl: ExternFieldDescriptor) -> RawExternField {
    RawExternField {
        decl,
        site: RawExternSite::default(),
    }
}

fn normalize_init(decl: ExternInitDescriptor) -> RawExternInit {
    RawExternInit {
        decl,
        site: RawExternSite::default(),
    }
}

fn normalize_method(decl: ExternMethodDescriptor) -> RawExternMethod {
    RawExternMethod {
        decl,
        site: RawExternSite::default(),
    }
}

fn normalize_static(decl: ExternStaticDescriptor) -> RawExternStatic {
    RawExternStatic {
        decl,
        site: RawExternSite::default(),
    }
}

fn normalize_operator(decl: ExternOperatorDescriptor) -> RawExternOperator {
    RawExternOperator {
        decl,
        site: RawExternSite::default(),
    }
}
