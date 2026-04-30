use anvyx_externs::{
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternInitDescriptor, ExternMethodDescriptor,
    ExternModuleDescriptor, ExternOperatorDescriptor, ExternStaticDescriptor, ExternTypeDescriptor,
    ProviderDescriptor, validate,
};

use super::raw::*;

pub(crate) fn ingest_providers(inputs: ExternInputs) -> Result<RawExterns, Vec<ExternInputError>> {
    let mut groups = vec![];
    let mut errors = vec![];

    for provider in inputs.providers {
        if let Err(provider_errors) = validate(&provider) {
            errors.extend(provider_errors.into_iter().map(|error| {
                ExternInputError::InvalidProviderDescriptor {
                    provider: provider.provider.clone(),
                    error,
                }
            }));
            continue;
        }

        groups.push(normalize_provider(provider));
    }

    if errors.is_empty() {
        Ok(RawExterns { groups })
    } else {
        Err(errors)
    }
}

fn normalize_provider(provider: ProviderDescriptor) -> RawExternGroup {
    RawExternGroup {
        provenance: ExternProvenance::Provider {
            provider: provider.provider,
        },
        modules: provider.modules.into_iter().map(normalize_module).collect(),
    }
}

fn normalize_module(module: ExternModuleDescriptor) -> RawExternModule {
    RawExternModule {
        scope: RawExternScope::Named(module.path),
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
        rep: ty.rep,
        fields: ty.fields.into_iter().map(normalize_field).collect(),
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
