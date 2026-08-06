use anvyx_frontend::air;
use anvyx_runtime::{
    CallbackEscape, ExternParam, ExternTypeExpr, ParamFlow, RustExternAbi, RustExternBinding,
    RustParamAbi, RustProviderSupport, RustReturnAbi,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ResolveExternError {
    MissingBinding,
    MissingConfiguredSupport,
    MissingExport,
    UnsupportedAbi,
}

pub(super) fn resolve_extern<'a>(
    providers: &'a [RustProviderSupport],
    program: &air::Program,
    decl: &air::ExternDecl,
) -> Result<(&'a RustExternBinding, Option<usize>), ResolveExternError> {
    let binding = decl
        .binding
        .as_ref()
        .ok_or(ResolveExternError::MissingBinding)?;
    let support = provider(providers, binding.package.as_str(), &binding.provider)
        .ok_or(ResolveExternError::MissingConfiguredSupport)?;
    let binding = extern_binding(support, binding).ok_or(ResolveExternError::MissingExport)?;
    let callback_receiver = callback_receiver_index(decl);
    if !rust_extern_abi_supported_with_receiver(&binding.abi, callback_receiver)
        || binding.abi.params.len() != decl.call_params().count()
        || !rust_abi_matches_air(&binding.abi.params, &binding.abi.ret, &decl.abi)
        || !hidden_ctx_borrows_ok(program, decl, binding)
    {
        return Err(ResolveExternError::UnsupportedAbi);
    }
    Ok((binding, callback_receiver))
}

pub(super) fn rust_extern_abi_supported_with_receiver(
    abi: &RustExternAbi,
    receiver: Option<usize>,
) -> bool {
    match abi.support {
        anvyx_runtime::RustAbiSupport::Direct => abi.backend_supported(),
        anvyx_runtime::RustAbiSupport::NeedsWrapperConversion => {
            abi.supported_callback_wrapper_with_receiver(receiver)
        }
        anvyx_runtime::RustAbiSupport::Unsupported => false,
    }
}

fn hidden_ctx_borrows_ok(
    program: &air::Program,
    decl: &air::ExternDecl,
    binding: &RustExternBinding,
) -> bool {
    binding.abi.ctx != anvyx_runtime::RustWrapperCtx::HiddenRuntime
        || decl
            .call_params()
            .zip(&binding.abi.params)
            .all(|(param, abi)| {
                !matches!(abi, RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_))
                    || !matches!(
                        program.type_arena.data(param.ty),
                        air::TypeData::Extern(id)
                            if program.extern_type(*id).rep == air::ExternRep::Shared
                    )
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
    support: &'a RustProviderSupport,
    binding: &air::ExternBindingDecl,
) -> Option<&'a RustExternBinding> {
    support
        .modules
        .iter()
        .flat_map(|module| &module.bindings)
        .find(|native| native.key == binding.key)
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
