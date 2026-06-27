use anvyx_frontend::air;
use anvyx_runtime::{
    ExternTypeExpr, RustExternBinding, RustParamAbi, RustProviderSupport, RustReturnAbi,
    RustTypeBinding,
};

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
    if !rir::rust_extern_abi_supported(&binding.abi)
        || binding.abi.params.len() != decl.call_params().count()
        || !rust_abi_matches_air(&binding.abi.params, &binding.abi.ret, &decl.abi)
    {
        return Err(ResolveExternError::UnsupportedRustAbi);
    }
    let params = binding.abi.params.iter().map(rir::rust_param_abi).collect();
    Ok(ResolvedExtern { binding, params })
}

pub(super) fn rust_abi_matches_air(
    params: &[RustParamAbi],
    ret: &RustReturnAbi,
    abi: &air::ExternAbi,
) -> bool {
    params.len() == abi.params.len()
        && params
            .iter()
            .zip(&abi.params)
            .all(|(param, expected)| param_abi_matches(param, expected))
        && return_abi_matches(ret, &abi.ret)
}

fn param_abi_matches(param: &RustParamAbi, expected: &ExternTypeExpr) -> bool {
    match param {
        RustParamAbi::Value(ty)
        | RustParamAbi::OwnedNamed(ty)
        | RustParamAbi::Borrow(ty)
        | RustParamAbi::MutBorrow(ty)
        | RustParamAbi::MutPlace(ty) => expected != &ExternTypeExpr::Void && ty == expected,
        RustParamAbi::InitField(inner) => param_abi_matches(inner, expected),
        RustParamAbi::Option(inner) => {
            matches!(expected, ExternTypeExpr::Option(expected) if param_abi_matches(inner, expected))
        }
        RustParamAbi::Result(ok, err) => {
            matches!(expected, ExternTypeExpr::Result(expected_ok, expected_err) if param_abi_matches(ok, expected_ok) && param_abi_matches(err, expected_err))
        }
        RustParamAbi::Slice(inner) => {
            matches!(expected, ExternTypeExpr::Slice(expected) if param_abi_matches(inner, expected))
        }
        RustParamAbi::ScopedLambda(callback) | RustParamAbi::EscapingLambda(callback) => {
            matches!(expected, ExternTypeExpr::Callback(expected) if callback_shape_matches(callback, expected))
        }
    }
}

fn callback_shape_matches(
    found: &anvyx_runtime::ExternCallbackSignature,
    expected: &anvyx_runtime::ExternCallbackSignature,
) -> bool {
    found.params == expected.params && found.ret == expected.ret && found.policy == expected.policy
}

fn return_abi_matches(ret: &RustReturnAbi, expected: &ExternTypeExpr) -> bool {
    match ret {
        RustReturnAbi::Void => expected == &ExternTypeExpr::Void,
        RustReturnAbi::Value(ty) | RustReturnAbi::OwnedNamed(ty) => {
            expected != &ExternTypeExpr::Void && ty == expected
        }
        RustReturnAbi::Option(inner) => {
            matches!(expected, ExternTypeExpr::Option(expected) if return_abi_matches(inner, expected))
        }
        RustReturnAbi::Result(ok, err) => {
            matches!(expected, ExternTypeExpr::Result(expected_ok, expected_err) if return_abi_matches(ok, expected_ok) && return_abi_matches(err, expected_err))
        }
    }
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
