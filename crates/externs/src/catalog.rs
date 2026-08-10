use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use serde::{Deserialize, Serialize};

use crate::{
    ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternDescriptorError,
    ExternMaterialization, ExternMemberSelector, ExternModuleDescriptor, ExternTypeDescriptor,
    ExternTypeKey, ModulePath, ProviderDescriptor, ProviderId, RustExternAbi, RustExternBinding,
    RustMaterializerBinding, RustPath, RustSupportError, RustTypeBinding,
    native::valid_rust_identifier, validate, validate_rust_module_parts,
};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ProviderPackageKey(pub String);

impl std::fmt::Display for ProviderPackageKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawProviderPackage {
    pub exports: Vec<RawProviderExport>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RawProviderExport {
    Descriptor(ProviderDescriptor),
    Rust(RawRustProviderExport),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawRustProviderExport {
    pub provider: ProviderId,
    pub modules: Vec<RawRustModuleExport>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawRustModuleExport {
    pub descriptor: ExternModuleDescriptor,
    pub types: Vec<RawRustTypeBinding>,
    pub bindings: Vec<RawRustExternBinding>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawRustTypeBinding {
    pub name: String,
    pub path: RawRustTypePath,
    pub owns_heap_edges: bool,
    pub materializer: Option<RawRustMaterializerBinding>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawRustTypePath {
    pub symbol: RawRustSymbolPath,
    pub args: RawRustTypeArgs,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RawRustTypeArgs {
    None,
    ContextLifetime,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawRustSymbolPath(pub Vec<String>);

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawRustMaterializerBinding {
    pub mode: ExternMaterialization,
    pub rust_type: RawRustTypePath,
    pub path: RawRustSymbolPath,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RawRustExternBinding {
    pub target: RawModuleBindingTarget,
    pub operation: ExternBindingOp,
    pub path: RawRustSymbolPath,
    pub abi: RustExternAbi,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RawModuleBindingTarget {
    Function(String),
    Member {
        owner: String,
        selector: ExternMemberSelector,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProviderCatalogError {
    EmptyPackage {
        package: ProviderPackageKey,
    },
    DuplicatePackage(ProviderPackageKey),
    InvalidDescriptor {
        package: ProviderPackageKey,
        provider: ProviderId,
        errors: Vec<ExternDescriptorError>,
    },
    InvalidSupport {
        package: ProviderPackageKey,
        error: RustSupportError,
    },
    DuplicateProvider {
        package: ProviderPackageKey,
        provider: ProviderId,
    },
    DuplicateModule {
        package: ProviderPackageKey,
        module: ModulePath,
    },
    MissingCrateAlias {
        package: ProviderPackageKey,
        provider: ProviderId,
    },
    InvalidCrateAlias {
        package: ProviderPackageKey,
        alias: String,
    },
    MissingProvider,
    DescriptorOnly,
    MissingType,
    MissingBinding,
}

impl std::fmt::Display for ProviderCatalogError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyPackage { package } => write!(
                f,
                "native provider package `{package}` exposed no provider descriptors"
            ),
            Self::DuplicatePackage(package) => write!(f, "duplicate provider package `{package}`"),
            Self::InvalidDescriptor {
                package,
                provider,
                errors,
            } => write!(
                f,
                "invalid provider `{}` in package `{package}`: {errors:?}",
                provider.name
            ),
            Self::InvalidSupport { package, error } => write!(
                f,
                "invalid native provider support in package `{package}`: {error}"
            ),
            Self::DuplicateProvider { package, provider } => write!(
                f,
                "duplicate provider `{}` in package `{package}`",
                provider.name
            ),
            Self::DuplicateModule { package, module } => write!(
                f,
                "duplicate provider module `{module}` in package `{package}`"
            ),
            Self::MissingCrateAlias { package, provider } => write!(
                f,
                "Rust provider `{}` in package `{package}` has no crate alias",
                provider.name
            ),
            Self::InvalidCrateAlias { package, alias } => write!(
                f,
                "Rust provider package `{package}` has invalid crate alias `{alias}`"
            ),
            Self::MissingProvider => f.write_str("native provider is missing"),
            Self::DescriptorOnly => f.write_str("native provider has no Rust support"),
            Self::MissingType => f.write_str("native provider type support is missing"),
            Self::MissingBinding => f.write_str("native provider binding support is missing"),
        }
    }
}

impl std::error::Error for ProviderCatalogError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TypeLocation {
    module: usize,
    index: usize,
    descriptor_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BindingLocation {
    module: usize,
    index: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct CatalogPackage {
    key: ProviderPackageKey,
    providers: Vec<CatalogProvider>,
    provider_index: HashMap<ProviderId, usize>,
}

#[derive(Debug, PartialEq, Eq)]
struct CatalogProvider {
    descriptor: ProviderDescriptor,
    support: Option<Vec<CatalogRustModule>>,
    types: HashMap<ExternTypeKey, TypeLocation>,
    bindings: HashMap<ExternBindingKey, BindingLocation>,
}

#[derive(Debug, PartialEq, Eq)]
struct CatalogRustModule {
    types: Vec<RustTypeBinding>,
    bindings: Vec<RustExternBinding>,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ProviderCatalog {
    packages: Vec<CatalogPackage>,
    package_index: HashMap<ProviderPackageKey, usize>,
}

impl ProviderCatalog {
    pub fn try_new(
        inputs: Vec<(ProviderPackageKey, RawProviderPackage, Option<String>)>,
    ) -> Result<Self, ProviderCatalogError> {
        let mut package_index = HashMap::with_capacity(inputs.len());
        for (index, (key, _, _)) in inputs.iter().enumerate() {
            if package_index.insert(key.clone(), index).is_some() {
                return Err(ProviderCatalogError::DuplicatePackage(key.clone()));
            }
        }
        let packages = inputs
            .into_iter()
            .map(|(key, package, crate_alias)| {
                validate_package(key, package, crate_alias.as_deref())
            })
            .collect::<Result<_, _>>()?;
        Ok(Self {
            packages,
            package_index,
        })
    }

    pub fn binding(
        &self,
        package: &ProviderPackageKey,
        provider: &ProviderId,
        key: &ExternBindingKey,
    ) -> Result<(&RustExternBinding, &ExternModuleDescriptor), ProviderCatalogError> {
        let package = self
            .package_index
            .get(package)
            .map(|index| &self.packages[*index])
            .ok_or(ProviderCatalogError::MissingProvider)?;
        let provider = package
            .provider_index
            .get(provider)
            .map(|index| &package.providers[*index])
            .ok_or(ProviderCatalogError::MissingProvider)?;
        let location = provider
            .bindings
            .get(key)
            .ok_or(if provider.support.is_some() {
                ProviderCatalogError::MissingBinding
            } else {
                ProviderCatalogError::DescriptorOnly
            })?;
        let module = provider
            .support
            .as_ref()
            .expect("binding index requires Rust provider")
            .get(location.module)
            .expect("binding index requires matching module");
        Ok((
            &module.bindings[location.index],
            &provider.descriptor.modules[location.module],
        ))
    }

    pub fn native_type_parts(
        &self,
        package: &ProviderPackageKey,
        provider: &ProviderId,
        key: &ExternTypeKey,
    ) -> Result<
        (
            &RustTypeBinding,
            &ExternModuleDescriptor,
            &ExternTypeDescriptor,
        ),
        ProviderCatalogError,
    > {
        let package = self
            .package_index
            .get(package)
            .map(|index| &self.packages[*index])
            .ok_or(ProviderCatalogError::MissingProvider)?;
        let provider = package
            .provider_index
            .get(provider)
            .map(|index| &package.providers[*index])
            .ok_or(ProviderCatalogError::MissingProvider)?;
        let location = provider
            .types
            .get(key)
            .ok_or(if provider.support.is_some() {
                ProviderCatalogError::MissingType
            } else {
                ProviderCatalogError::DescriptorOnly
            })?;
        let module = &provider.descriptor.modules[location.module];
        Ok((
            &provider
                .support
                .as_ref()
                .expect("type index requires Rust provider")[location.module]
                .types[location.index],
            module,
            &module.types[location.descriptor_index],
        ))
    }

    pub fn modules(
        &self,
    ) -> impl Iterator<Item = (&ProviderPackageKey, &ProviderId, &ExternModuleDescriptor)> {
        self.packages.iter().flat_map(|package| {
            package.providers.iter().flat_map(move |provider| {
                provider
                    .descriptor
                    .modules
                    .iter()
                    .map(move |module| (&package.key, &provider.descriptor.provider, module))
            })
        })
    }
}

fn validate_package(
    key: ProviderPackageKey,
    package: RawProviderPackage,
    crate_alias: Option<&str>,
) -> Result<CatalogPackage, ProviderCatalogError> {
    if package.exports.is_empty() {
        return Err(ProviderCatalogError::EmptyPackage { package: key });
    }

    let rust_provider = package.exports.iter().find_map(|export| match export {
        RawProviderExport::Rust(raw) => Some(&raw.provider),
        RawProviderExport::Descriptor(_) => None,
    });
    let crate_alias = rust_provider
        .map(|provider| {
            let alias = crate_alias.ok_or_else(|| ProviderCatalogError::MissingCrateAlias {
                package: key.clone(),
                provider: provider.clone(),
            })?;
            if !valid_rust_identifier(alias) {
                return Err(ProviderCatalogError::InvalidCrateAlias {
                    package: key.clone(),
                    alias: alias.to_string(),
                });
            }
            Ok(Rc::<str>::from(alias))
        })
        .transpose()?;

    let mut providers = vec![];
    let mut provider_index = HashMap::new();
    let mut modules = HashSet::new();
    for export in package.exports {
        let (descriptor, support) = match export {
            RawProviderExport::Descriptor(descriptor) => (descriptor, None),
            RawProviderExport::Rust(raw) => {
                let alias = crate_alias
                    .as_ref()
                    .expect("Rust package requires validated crate alias");
                let RawRustProviderExport { provider, modules } = raw;
                let mut descriptor_modules = Vec::with_capacity(modules.len());
                let mut support = Vec::with_capacity(modules.len());
                for raw in modules {
                    let (descriptor, native) = catalog_rust_module(raw, alias);
                    descriptor_modules.push(descriptor);
                    support.push(native);
                }
                (
                    ProviderDescriptor {
                        provider,
                        modules: descriptor_modules,
                    },
                    Some(support),
                )
            }
        };
        let index = providers.len();
        if provider_index
            .insert(descriptor.provider.clone(), index)
            .is_some()
        {
            return Err(ProviderCatalogError::DuplicateProvider {
                package: key.clone(),
                provider: descriptor.provider,
            });
        }
        for module in &descriptor.modules {
            if !modules.insert(module.path.clone()) {
                return Err(ProviderCatalogError::DuplicateModule {
                    package: key.clone(),
                    module: module.path.clone(),
                });
            }
        }
        providers.push(CatalogProvider {
            descriptor,
            support,
            types: HashMap::new(),
            bindings: HashMap::new(),
        });
    }

    for provider in &mut providers {
        let Some(support) = &provider.support else {
            continue;
        };
        for (module_index, native) in support.iter().enumerate() {
            for (index, ty) in native.types.iter().enumerate() {
                let Some(descriptor_index) = provider.descriptor.modules[module_index]
                    .types
                    .iter()
                    .position(|descriptor| descriptor.name == ty.key.name)
                else {
                    continue;
                };
                if provider
                    .types
                    .insert(
                        ty.key.clone(),
                        TypeLocation {
                            module: module_index,
                            index,
                            descriptor_index,
                        },
                    )
                    .is_some()
                {
                    return Err(ProviderCatalogError::InvalidSupport {
                        package: key.clone(),
                        error: RustSupportError::duplicate_type(
                            &provider.descriptor.provider.name,
                            &ty.key,
                        ),
                    });
                }
            }
            for (index, binding) in native.bindings.iter().enumerate() {
                if provider
                    .bindings
                    .insert(
                        binding.key.clone(),
                        BindingLocation {
                            module: module_index,
                            index,
                        },
                    )
                    .is_some()
                {
                    return Err(ProviderCatalogError::InvalidSupport {
                        package: key.clone(),
                        error: RustSupportError::duplicate_binding(
                            &provider.descriptor.provider.name,
                            &binding.key,
                        ),
                    });
                }
            }
        }
    }

    let descriptors = providers
        .iter()
        .map(|provider| &provider.descriptor)
        .collect::<Vec<_>>();
    for descriptor in &descriptors {
        validate(descriptor).map_err(|errors| ProviderCatalogError::InvalidDescriptor {
            package: key.clone(),
            provider: descriptor.provider.clone(),
            errors,
        })?;
    }
    let all_types = providers
        .iter()
        .filter_map(|provider| provider.support.as_ref())
        .flatten()
        .flat_map(|module| &module.types)
        .collect::<Vec<_>>();
    for provider in &providers {
        let Some(support) = &provider.support else {
            continue;
        };
        for (module, native) in provider.descriptor.modules.iter().zip(support) {
            validate_rust_module_parts(
                &provider.descriptor,
                &module.path,
                &native.types,
                &native.bindings,
                &descriptors,
                &all_types,
            )
            .map_err(|error| ProviderCatalogError::InvalidSupport {
                package: key.clone(),
                error,
            })?;
        }
    }

    Ok(CatalogPackage {
        key,
        providers,
        provider_index,
    })
}

fn catalog_rust_module(
    raw: RawRustModuleExport,
    crate_alias: &Rc<str>,
) -> (ExternModuleDescriptor, CatalogRustModule) {
    let RawRustModuleExport {
        descriptor,
        types,
        bindings,
    } = raw;
    let module = descriptor.path.clone();
    let types = types
        .into_iter()
        .map(|ty| RustTypeBinding {
            key: ExternTypeKey {
                module: module.clone(),
                name: ty.name,
            },
            path: rust_path(ty.path, crate_alias),
            owns_heap_edges: ty.owns_heap_edges,
            materializer: ty.materializer.map(|materializer| RustMaterializerBinding {
                mode: materializer.mode,
                rust_type: rust_path(materializer.rust_type, crate_alias),
                path: rust_symbol_path(materializer.path, crate_alias),
            }),
        })
        .collect();
    let bindings = bindings
        .into_iter()
        .map(|binding| {
            let key = match binding.target {
                RawModuleBindingTarget::Function(name) => ExternBindingKey {
                    target: ExternBindingTarget::Function(crate::ExternFunctionKey {
                        module: module.clone(),
                        name,
                    }),
                    operation: binding.operation,
                },
                RawModuleBindingTarget::Member { owner, selector } => ExternBindingKey {
                    target: ExternBindingTarget::Member(crate::ExternMemberKey {
                        owner: ExternTypeKey {
                            module: module.clone(),
                            name: owner,
                        },
                        selector,
                    }),
                    operation: binding.operation,
                },
            };
            RustExternBinding {
                key,
                path: rust_symbol_path(binding.path, crate_alias),
                abi: binding.abi,
            }
        })
        .collect();
    (descriptor, CatalogRustModule { types, bindings })
}

fn rust_path(path: RawRustTypePath, crate_alias: &Rc<str>) -> RustPath {
    let RawRustTypePath { symbol, args } = path;
    let mut segments = symbol.0;
    if matches!(args, RawRustTypeArgs::ContextLifetime)
        && let Some(last) = segments.last_mut()
    {
        last.push_str("<'cx>");
    }
    RustPath {
        crate_name: crate_alias.clone(),
        segments,
    }
}

fn rust_symbol_path(path: RawRustSymbolPath, crate_alias: &Rc<str>) -> RustPath {
    RustPath {
        crate_name: crate_alias.clone(),
        segments: path.0,
    }
}
