use anvyx_externs::{
    ExternDescriptorError, ExternModuleDescriptor, ModulePath, ProviderDescriptor, validate,
    validate_module_contents,
};

use super::raw::*;

pub(crate) fn validate_raw_shapes(raw: &RawExterns) -> Result<(), Vec<ExternInputError>> {
    let mut errors = vec![];

    for group in &raw.groups {
        for module in &group.modules {
            let descriptor = module_descriptor(module);
            let validation = match &group.provenance {
                ExternProvenance::Provider { provider, .. } => validate(&ProviderDescriptor {
                    provider: provider.clone(),
                    modules: vec![descriptor],
                }),
                ExternProvenance::Source { .. } => validate_module_contents(&descriptor),
            };
            let Err(module_errors) = validation else {
                continue;
            };
            let decl = RawExternDecl {
                provenance: group.provenance.clone(),
                site: module_site(module),
            };
            errors.extend(
                module_errors
                    .into_iter()
                    .filter(|error| !is_duplicate_error(error))
                    .map(|error| ExternInputError::InvalidRawDescriptor {
                        decl: decl.clone(),
                        scope: module.scope.clone(),
                        error,
                    }),
            );
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn module_descriptor(module: &RawExternModule) -> ExternModuleDescriptor {
    ExternModuleDescriptor {
        path: descriptor_path(&module.scope),
        types: module.types.iter().map(type_descriptor).collect(),
        functions: module
            .functions
            .iter()
            .map(|function| function.decl.clone())
            .collect(),
    }
}

fn descriptor_path(scope: &RawExternScope) -> ModulePath {
    match scope {
        RawExternScope::Module(module) => ModulePath {
            segments: module
                .module_path()
                .map_or_else(Vec::new, |path| path.segments().to_vec()),
        },
    }
}

fn type_descriptor(ty: &RawExternType) -> anvyx_externs::ExternTypeDescriptor {
    anvyx_externs::ExternTypeDescriptor {
        name: ty.name.clone(),
        doc: ty.doc.clone(),
        rep: ty.rep,
        fields: ty.fields.iter().map(|field| field.decl.clone()).collect(),
        variants: ty.variants.clone(),
        init: ty.init.as_ref().map(|init| init.decl.clone()),
        methods: ty
            .methods
            .iter()
            .map(|method| method.decl.clone())
            .collect(),
        statics: ty
            .statics
            .iter()
            .map(|static_method| static_method.decl.clone())
            .collect(),
        operators: ty
            .operators
            .iter()
            .map(|operator| operator.decl.clone())
            .collect(),
    }
}

fn module_site(module: &RawExternModule) -> RawExternSite {
    module
        .functions
        .iter()
        .map(|function| function.site)
        .chain(module.types.iter().map(|ty| ty.site))
        .find(|site| site.span.is_some())
        .unwrap_or_default()
}

fn is_duplicate_error(error: &ExternDescriptorError) -> bool {
    matches!(
        error,
        ExternDescriptorError::DuplicateModule(_)
            | ExternDescriptorError::DuplicateType { .. }
            | ExternDescriptorError::DuplicateFunction { .. }
            | ExternDescriptorError::DuplicateField { .. }
            | ExternDescriptorError::DuplicateMethod { .. }
            | ExternDescriptorError::DuplicateStatic { .. }
            | ExternDescriptorError::DuplicateOperator { .. }
    )
}
