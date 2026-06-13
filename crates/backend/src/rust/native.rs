use anvyx_frontend::air;
use anvyx_runtime::{RustAbiSupport, RustExternBinding, RustProviderSupport, RustTypeBinding};

use super::rir::{self, NativeParamAbi};

pub(super) struct ResolvedExtern<'a> {
    pub binding: &'a RustExternBinding,
    pub params: Vec<NativeParamAbi>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ResolveExternError {
    UnsupportedExtern,
    UnsupportedRustAbi,
}

pub(super) fn resolve_extern<'a>(
    providers: &'a [RustProviderSupport],
    decl: &air::ExternDecl,
) -> Result<ResolvedExtern<'a>, ResolveExternError> {
    let binding = decl
        .binding
        .as_ref()
        .ok_or(ResolveExternError::UnsupportedExtern)?;
    let binding =
        extern_binding(providers, binding).ok_or(ResolveExternError::UnsupportedExtern)?;
    if binding.abi.support != RustAbiSupport::Direct
        || binding.abi.params.len() != decl.call_params().count()
    {
        return Err(ResolveExternError::UnsupportedRustAbi);
    }
    let params = binding
        .abi
        .params
        .iter()
        .map(rir::rust_param_abi)
        .collect::<Option<Vec<_>>>()
        .ok_or(ResolveExternError::UnsupportedRustAbi)?;
    Ok(ResolvedExtern { binding, params })
}

fn extern_binding<'a>(
    providers: &'a [RustProviderSupport],
    binding: &air::ExternBindingDecl,
) -> Option<&'a RustExternBinding> {
    provider(providers, binding.package.as_str(), &binding.provider).and_then(|provider| {
        provider
            .modules
            .iter()
            .flat_map(|module| &module.bindings)
            .find(|native| native.key == binding.key)
    })
}

pub(super) fn type_binding<'a>(
    providers: &'a [RustProviderSupport],
    binding: &air::ExternTypeBindingDecl,
) -> Option<&'a RustTypeBinding> {
    provider(providers, binding.package.as_str(), &binding.provider).and_then(|provider| {
        provider
            .modules
            .iter()
            .flat_map(|module| &module.types)
            .find(|native| native.key == binding.key)
    })
}

fn provider<'a>(
    providers: &'a [RustProviderSupport],
    package: &str,
    provider: &anvyx_runtime::ProviderId,
) -> Option<&'a RustProviderSupport> {
    providers
        .iter()
        .find(|native| native.package == package && native.provider == *provider)
}
