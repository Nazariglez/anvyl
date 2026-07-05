use anvyx_frontend::air;
use anvyx_runtime::{
    CallbackEscape, ExternParam, ExternTypeExpr, ParamFlow, RustExternBinding, RustParamAbi,
    RustProviderSupport, RustReturnAbi, RustTypeBinding,
};

use super::{native_call, rir};

pub(super) struct ResolvedExtern<'a> {
    pub binding: &'a RustExternBinding,
    pub params: Vec<native_call::NativeParamAbi>,
    pub callback_receiver: Option<usize>,
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
    let callback_receiver = callback_receiver_index(decl);
    if !rir::rust_extern_abi_supported_with_receiver(&binding.abi, callback_receiver)
        || binding.abi.params.len() != decl.call_params().count()
        || !rust_abi_matches_air(&binding.abi.params, &binding.abi.ret, &decl.abi)
    {
        return Err(ResolveExternError::UnsupportedRustAbi);
    }
    let params = binding
        .abi
        .params
        .iter()
        .map(native_call::classify_param)
        .collect();
    Ok(ResolvedExtern {
        binding,
        params,
        callback_receiver,
    })
}

fn callback_receiver_index(decl: &air::ExternDecl) -> Option<usize> {
    match decl.member {
        air::ExternMember::FieldGetter { .. }
        | air::ExternMember::FieldSetter { .. }
        | air::ExternMember::Method { .. }
        | air::ExternMember::UnaryOperator { .. }
        | air::ExternMember::BinaryOperator { .. } => Some(0),
        air::ExternMember::FreeFunction
        | air::ExternMember::StaticMethod { .. }
        | air::ExternMember::Init { .. } => None,
    }
}

pub(super) fn rust_abi_matches_air(
    params: &[RustParamAbi],
    ret: &RustReturnAbi,
    abi: &air::ExternAbi,
) -> bool {
    params.len() == abi.params.len()
        && params.iter().zip(&abi.params).all(|(param, expected)| {
            param.matches_extern_param(&air_extern_param(param, expected.clone()))
        })
        && ret.matches_extern_type(&abi.ret)
}

fn air_extern_param(abi: &RustParamAbi, ty: ExternTypeExpr) -> ExternParam {
    let flow = match abi {
        RustParamAbi::Borrow(_) => ParamFlow::Borrow,
        RustParamAbi::MutBorrow(_) | RustParamAbi::MutPlace(_) => ParamFlow::MutBorrow,
        _ => ParamFlow::Value,
    };
    let escape = match &ty {
        ExternTypeExpr::Callback(callback) => callback.policy.escape,
        _ => CallbackEscape::NonEscaping,
    };
    ExternParam {
        name: None,
        ty,
        flow,
        escape,
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
