use anvyx_externs::{
    CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackParam, ExternCallbackSignature,
    ExternTypeExpr, SCOPED_LAMBDA_MAX_ARITY,
};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    GenericArgument, Ident, PathArguments, ReturnType, Signature, Token, Type, TypePath, bracketed,
    parse::{ParseStream, Parser},
    spanned::Spanned,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CleanParam {
    pub name: String,
    pub ty: CleanType,
    pub flow: CleanFlow,
    pub abi: CleanParamAbi,
    pub conversion: BoundaryConversion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CleanReturn {
    pub ty: CleanType,
    pub abi: CleanReturnAbi,
    pub fallible: bool,
    pub conversion: BoundaryConversion,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CleanType {
    Void,
    Bool,
    Int,
    Float,
    String,
    Callback(CleanCallback),
    Option(Box<CleanType>),
    List(Box<CleanType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CleanCallback {
    pub params: Vec<CleanType>,
    pub ret: Box<CleanType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CleanFlow {
    Value,
    Borrow,
    MutBorrow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CleanParamAbi {
    Value(CleanType),
    Borrow(CleanType),
    MutPlace(CleanType),
    ScopedLambda(CleanCallback),
    Option(Box<CleanParamAbi>),
    List(Box<CleanParamAbi>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CleanReturnAbi {
    Void,
    Value(CleanType),
    Option(Box<CleanReturnAbi>),
    List(Box<CleanReturnAbi>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundaryConversion {
    Direct,
    NeedsWrapper,
    Unsupported,
}

pub fn validate_callable_signature(
    sig: &Signature,
    macro_name: &str,
    noun: &str,
    allow_ctx_lifetime: bool,
) -> syn::Result<()> {
    if invalid_generics(sig, allow_ctx_lifetime) {
        return Err(syn::Error::new_spanned(
            &sig.generics,
            format!("{macro_name} does not support generic {noun}s"),
        ));
    }
    if sig.asyncness.is_some() {
        return Err(syn::Error::new_spanned(
            sig.asyncness,
            format!("{macro_name} does not support async {noun}s"),
        ));
    }
    if sig.unsafety.is_some() {
        return Err(syn::Error::new_spanned(
            sig.unsafety,
            format!("{macro_name} does not support unsafe {noun}s"),
        ));
    }
    if sig.constness.is_some() {
        return Err(syn::Error::new_spanned(
            sig.constness,
            format!("{macro_name} does not support const {noun}s"),
        ));
    }
    if sig.variadic.is_some() {
        return Err(syn::Error::new_spanned(
            &sig.variadic,
            format!("{macro_name} does not support variadic {noun}s"),
        ));
    }
    Ok(())
}

fn invalid_generics(sig: &Signature, allow_ctx_lifetime: bool) -> bool {
    sig.generics.where_clause.is_some()
        || sig.generics.params.iter().any(|param| {
            !allow_ctx_lifetime
                || !matches!(param, syn::GenericParam::Lifetime(lifetime) if lifetime.lifetime.ident == "cx")
        })
}

pub fn validate_ctx_param(param: &syn::PatType, macro_name: &str) -> syn::Result<()> {
    if !matches!(param.pat.as_ref(), syn::Pat::Ident(ident) if ident.ident == "ctx") {
        return Err(syn::Error::new_spanned(
            &param.pat,
            format!("{macro_name} ctx parameter must be named `ctx`"),
        ));
    }
    if !is_ctx_type(&param.ty) {
        return Err(syn::Error::new_spanned(
            &param.ty,
            format!("{macro_name} ctx parameter must be `&mut anvyx_runtime::Ctx`"),
        ));
    }
    Ok(())
}

pub fn validate_mut_place_ctx(
    sig: &Signature,
    ctx: &syn::PatType,
    params: &[CleanParam],
    macro_name: &str,
) -> syn::Result<()> {
    if !params
        .iter()
        .any(|param| param.flow == CleanFlow::MutBorrow)
    {
        return Ok(());
    }
    if !sig.generics.params.iter().any(|param| {
        matches!(param, syn::GenericParam::Lifetime(lifetime) if lifetime.lifetime.ident == "cx")
    }) {
        return Err(syn::Error::new_spanned(
            &sig.generics,
            format!("{macro_name} MutPlace parameters require a `'cx` lifetime"),
        ));
    }
    if !ctx_has_explicit_brand(&ctx.ty) {
        return Err(syn::Error::new_spanned(
            &ctx.ty,
            format!(
                "{macro_name} MutPlace parameters require ctx type `&mut anvyx_runtime::Ctx<'cx, '_>`"
            ),
        ));
    }
    Ok(())
}

fn is_ctx_type(ty: &Type) -> bool {
    ctx_args(ty).is_some_and(valid_ctx_args)
}

fn ctx_has_explicit_brand(ty: &Type) -> bool {
    let Some(PathArguments::AngleBracketed(args)) = ctx_args(ty) else {
        return false;
    };
    if args.args.len() != 2 {
        return false;
    }
    let mut args = args.args.iter();
    matches!(args.next(), Some(GenericArgument::Lifetime(lifetime)) if lifetime.ident == "cx")
        && matches!(args.next(), Some(arg) if valid_inferred_lifetime(arg))
}

fn ctx_args(ty: &Type) -> Option<&PathArguments> {
    let Type::Reference(reference) = ty else {
        return None;
    };
    if reference.lifetime.is_some() || reference.mutability.is_none() {
        return None;
    }
    let Type::Path(path) = reference.elem.as_ref() else {
        return None;
    };
    if path.qself.is_some() {
        return None;
    }
    let mut segments = path.path.segments.iter();
    let first = segments.next();
    let second = segments.next();
    if segments.next().is_some() {
        return None;
    }
    match (first, second) {
        (Some(ctx), None) if ctx.ident == "Ctx" => Some(&ctx.arguments),
        (Some(runtime), Some(ctx))
            if runtime.ident == "anvyx_runtime"
                && matches!(runtime.arguments, PathArguments::None)
                && ctx.ident == "Ctx" =>
        {
            Some(&ctx.arguments)
        }
        _ => None,
    }
}

fn valid_ctx_args(args: &PathArguments) -> bool {
    match args {
        PathArguments::None => true,
        PathArguments::AngleBracketed(args) if args.args.len() == 2 => {
            let mut args = args.args.iter();
            valid_ctx_brand(args.next().expect("checked len"))
                && valid_inferred_lifetime(args.next().expect("checked len"))
        }
        _ => false,
    }
}

fn valid_ctx_brand(arg: &GenericArgument) -> bool {
    valid_inferred_lifetime(arg)
        || matches!(arg, GenericArgument::Lifetime(lifetime) if lifetime.ident == "cx")
}

fn valid_inferred_lifetime(arg: &GenericArgument) -> bool {
    matches!(arg, GenericArgument::Lifetime(lifetime) if lifetime.ident == "_")
}

pub fn classify_param(pat_ty: &syn::PatType, has_ctx: bool) -> syn::Result<CleanParam> {
    let syn::Pat::Ident(ident) = pat_ty.pat.as_ref() else {
        return Err(syn::Error::new_spanned(
            &pat_ty.pat,
            "#[function] parameters must be identifiers",
        ));
    };
    let (ty, flow) = classify_type(&pat_ty.ty, Position::Param)?;
    if flow == CleanFlow::MutBorrow && !has_ctx {
        return Err(syn::Error::new_spanned(
            &pat_ty.ty,
            "MutPlace parameters require #[function(ctx)]",
        ));
    }
    if flow == CleanFlow::MutBorrow && !mut_place_macro_payload_supported(&ty) {
        return Err(syn::Error::new_spanned(
            &pat_ty.ty,
            "#[function(ctx)] MutPlace parameters only support bool, i64, f64, and Option of those payloads",
        ));
    }
    if ty == CleanType::Void {
        return Err(syn::Error::new_spanned(
            &pat_ty.ty,
            "#[function] parameters cannot be void",
        ));
    }
    let abi = param_abi(&ty, flow);
    let conversion = param_conversion(&ty, flow);
    Ok(CleanParam {
        name: ident.ident.to_string(),
        ty,
        flow,
        abi,
        conversion,
    })
}

pub fn classify_return(output: &ReturnType) -> syn::Result<CleanReturn> {
    match output {
        ReturnType::Default => Ok(CleanReturn {
            ty: CleanType::Void,
            abi: CleanReturnAbi::Void,
            fallible: false,
            conversion: BoundaryConversion::Direct,
        }),
        ReturnType::Type(_, ty) => classify_return_type(ty),
    }
}

fn classify_return_type(ty: &Type) -> syn::Result<CleanReturn> {
    if let Some((ok, err)) = result_args(ty)? {
        if !is_runtime_error(err) {
            return Err(syn::Error::new_spanned(
                err,
                "#[function] only supports Result<T, RuntimeError>",
            ));
        }
        let (ty, _) = classify_type(ok, Position::Return)?;
        let abi = return_abi(&ty);
        let conversion = return_conversion_for_type(&ty);
        return Ok(CleanReturn {
            ty,
            abi,
            fallible: true,
            conversion,
        });
    }
    let (ty, _) = classify_type(ty, Position::Return)?;
    let abi = return_abi(&ty);
    let conversion = return_conversion_for_type(&ty);
    Ok(CleanReturn {
        ty,
        abi,
        fallible: false,
        conversion,
    })
}

pub fn classify_field_type(source: &Type) -> syn::Result<CleanType> {
    let (ty, flow) = classify_type(source, Position::WrapperElement)?;
    if flow != CleanFlow::Value {
        return Err(syn::Error::new_spanned(
            source,
            "#[anvyx(field)] fields cannot be borrowed",
        ));
    }
    reject_wrapper_element(source, &ty, flow)?;
    Ok(ty)
}

fn classify_type(ty: &Type, position: Position) -> syn::Result<(CleanType, CleanFlow)> {
    match ty {
        Type::Tuple(tuple) if tuple.elems.is_empty() => Ok((CleanType::Void, CleanFlow::Value)),
        Type::Reference(reference) => {
            if position != Position::Param {
                return Err(syn::Error::new_spanned(
                    reference,
                    "#[function] returns cannot be borrowed",
                ));
            }
            if reference.mutability.is_some() {
                return Err(syn::Error::new_spanned(
                    reference,
                    "#[function] does not support direct mutable references; use #[function(ctx)] with MutPlace<'_, 'cx, T>",
                ));
            }
            if matches!(reference.elem.as_ref(), Type::Path(path) if path_is(path, &["str"])) {
                return Ok((CleanType::String, CleanFlow::Borrow));
            }
            Err(syn::Error::new_spanned(
                reference,
                "#[function] only supports &str references in the MVP",
            ))
        }
        Type::Path(path) => classify_path(path, position),
        _ => Err(syn::Error::new_spanned(
            ty,
            "unsupported #[function] boundary type",
        )),
    }
}

fn classify_path(path: &TypePath, position: Position) -> syn::Result<(CleanType, CleanFlow)> {
    if path.qself.is_some() {
        return Err(syn::Error::new_spanned(
            path,
            "qualified boundary types are not supported",
        ));
    }
    if let Some(callback) = scoped_lambda_type(path)? {
        if position != Position::Param {
            return Err(syn::Error::new_spanned(
                path,
                "ScopedLambda is only supported in parameter position",
            ));
        }
        return Ok((CleanType::Callback(callback), CleanFlow::Value));
    }
    if let Some(inner) = mut_place_type_arg(path)? {
        if position != Position::Param {
            return Err(syn::Error::new_spanned(
                path,
                "MutPlace is only supported in parameter position",
            ));
        }
        let (ty, flow) = classify_type(inner, Position::WrapperElement)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok((ty, CleanFlow::MutBorrow));
    }
    if path_is(path, &["bool"]) {
        return Ok((CleanType::Bool, CleanFlow::Value));
    }
    if path_is(path, &["i64"]) {
        return Ok((CleanType::Int, CleanFlow::Value));
    }
    if path_is(path, &["f64"]) {
        return Ok((CleanType::Float, CleanFlow::Value));
    }
    if path_is(path, &["String"]) || path_is(path, &["std", "string", "String"]) {
        return Ok((CleanType::String, CleanFlow::Value));
    }
    for rejected in [
        "f32", "usize", "isize", "i8", "i16", "i32", "u8", "u16", "u32", "u64",
    ] {
        if path_is(path, &[rejected]) {
            return Err(syn::Error::new_spanned(
                path,
                format!("#[function] does not support `{rejected}`; use i64 or f64"),
            ));
        }
    }
    if let Some(inner) = one_type_arg(path, &[&["Option"], &["std", "option", "Option"]])? {
        let (ty, flow) = classify_type(inner, Position::WrapperElement)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok((CleanType::Option(Box::new(ty)), CleanFlow::Value));
    }
    if let Some(inner) = one_type_arg(path, &[&["Vec"], &["std", "vec", "Vec"]])? {
        let (ty, flow) = classify_type(inner, Position::WrapperElement)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok((CleanType::List(Box::new(ty)), CleanFlow::Value));
    }
    Err(syn::Error::new_spanned(
        path,
        "unsupported #[function] boundary type",
    ))
}

pub fn type_with_override(
    inferred: &CleanType,
    override_ty: Option<&str>,
    span: proc_macro2::Span,
    mismatch: impl Into<String>,
) -> syn::Result<CleanType> {
    let Some(override_ty) = override_ty else {
        return Ok(inferred.clone());
    };
    let override_ty = parse_type_expr(override_ty)?;
    if override_ty == *inferred {
        Ok(override_ty)
    } else {
        Err(syn::Error::new(span, mismatch.into()))
    }
}

pub fn type_tokens_with_override(
    inferred: &CleanType,
    override_ty: Option<&str>,
    span: proc_macro2::Span,
    mismatch: impl Into<String>,
) -> syn::Result<TokenStream> {
    type_with_override(inferred, override_ty, span, mismatch).map(|ty| type_expr_tokens(&ty))
}

pub fn type_expr_tokens(ty: &CleanType) -> TokenStream {
    match ty {
        CleanType::Void => quote! { anvyx_runtime::ExternTypeExpr::Void },
        CleanType::Bool => quote! { anvyx_runtime::ExternTypeExpr::Bool },
        CleanType::Int => quote! { anvyx_runtime::ExternTypeExpr::Int },
        CleanType::Float => quote! { anvyx_runtime::ExternTypeExpr::Float },
        CleanType::String => quote! { anvyx_runtime::ExternTypeExpr::String },
        CleanType::Callback(callback) => callback_type_tokens(callback),
        CleanType::Option(inner) => {
            let inner = type_expr_tokens(inner);
            quote! { anvyx_runtime::ExternTypeExpr::Option(Box::new(#inner)) }
        }
        CleanType::List(inner) => {
            let inner = type_expr_tokens(inner);
            quote! { anvyx_runtime::ExternTypeExpr::List(Box::new(#inner)) }
        }
    }
}

fn callback_type_tokens(callback: &CleanCallback) -> TokenStream {
    let signature = callback_signature_tokens(callback);
    quote! { anvyx_runtime::ExternTypeExpr::Callback(#signature) }
}

fn callback_signature_tokens(callback: &CleanCallback) -> TokenStream {
    let params = callback.params.iter().map(|ty| {
        let ty = type_expr_tokens(ty);
        quote! {
            anvyx_runtime::ExternCallbackParam {
                ty: #ty,
                escape: anvyx_runtime::CallbackEscape::NonEscaping,
            }
        }
    });
    let ret = type_expr_tokens(&callback.ret);
    quote! {
        anvyx_runtime::ExternCallbackSignature {
            params: vec![#(#params),*],
            ret: Box::new(#ret),
            policy: anvyx_runtime::CallbackPolicy {
                escape: anvyx_runtime::CallbackEscape::NonEscaping,
                thread: anvyx_runtime::CallbackThread::SameThread,
            },
        }
    }
}

pub fn flow_tokens(flow: CleanFlow) -> TokenStream {
    match flow {
        CleanFlow::Value => quote! { anvyx_runtime::ParamFlow::Value },
        CleanFlow::Borrow => quote! { anvyx_runtime::ParamFlow::Borrow },
        CleanFlow::MutBorrow => quote! { anvyx_runtime::ParamFlow::MutBorrow },
    }
}

pub fn param_abi_tokens(abi: &CleanParamAbi) -> TokenStream {
    match abi {
        CleanParamAbi::Value(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustParamAbi::Value(#ty) }
        }
        CleanParamAbi::Borrow(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustParamAbi::Borrow(#ty) }
        }
        CleanParamAbi::MutPlace(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustParamAbi::MutPlace(#ty) }
        }
        CleanParamAbi::ScopedLambda(callback) => {
            let callback = callback_signature_tokens(callback);
            quote! { anvyx_runtime::RustParamAbi::ScopedLambda(#callback) }
        }
        CleanParamAbi::Option(inner) => {
            let inner = param_abi_tokens(inner);
            quote! { anvyx_runtime::RustParamAbi::Option(Box::new(#inner)) }
        }
        CleanParamAbi::List(inner) => {
            let inner = param_abi_tokens(inner);
            quote! { anvyx_runtime::RustParamAbi::List(Box::new(#inner)) }
        }
    }
}

pub fn return_abi_tokens(abi: &CleanReturnAbi) -> TokenStream {
    match abi {
        CleanReturnAbi::Void => quote! { anvyx_runtime::RustReturnAbi::Void },
        CleanReturnAbi::Value(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustReturnAbi::Value(#ty) }
        }
        CleanReturnAbi::Option(inner) => {
            let inner = return_abi_tokens(inner);
            quote! { anvyx_runtime::RustReturnAbi::Option(Box::new(#inner)) }
        }
        CleanReturnAbi::List(inner) => {
            let inner = return_abi_tokens(inner);
            quote! { anvyx_runtime::RustReturnAbi::List(Box::new(#inner)) }
        }
    }
}

fn param_abi(ty: &CleanType, flow: CleanFlow) -> CleanParamAbi {
    match flow {
        CleanFlow::MutBorrow => CleanParamAbi::MutPlace(ty.clone()),
        CleanFlow::Borrow => CleanParamAbi::Borrow(ty.clone()),
        CleanFlow::Value => match ty {
            CleanType::Option(inner) => {
                CleanParamAbi::Option(Box::new(param_abi(inner, CleanFlow::Value)))
            }
            CleanType::Callback(callback) => CleanParamAbi::ScopedLambda(callback.clone()),
            CleanType::List(inner) => {
                CleanParamAbi::List(Box::new(param_abi(inner, CleanFlow::Value)))
            }
            _ => CleanParamAbi::Value(ty.clone()),
        },
    }
}

fn return_abi(ty: &CleanType) -> CleanReturnAbi {
    match ty {
        CleanType::Void => CleanReturnAbi::Void,
        CleanType::Option(inner) => CleanReturnAbi::Option(Box::new(return_abi(inner))),
        CleanType::List(inner) => CleanReturnAbi::List(Box::new(return_abi(inner))),
        CleanType::Callback(_) => unreachable!("callbacks are rejected in return position"),
        _ => CleanReturnAbi::Value(ty.clone()),
    }
}

pub fn conversion_tokens(conversion: BoundaryConversion) -> TokenStream {
    match conversion {
        BoundaryConversion::Direct => quote! { anvyx_runtime::RustAbiSupport::Direct },
        BoundaryConversion::NeedsWrapper => {
            quote! { anvyx_runtime::RustAbiSupport::NeedsWrapperConversion }
        }
        BoundaryConversion::Unsupported => quote! { anvyx_runtime::RustAbiSupport::Unsupported },
    }
}

pub fn has_scoped_lambda(params: &[CleanParam]) -> bool {
    params
        .iter()
        .any(|param| matches!(param.ty, CleanType::Callback(_)))
}

pub fn scoped_lambda_has_visible_borrow(params: &[CleanParam]) -> bool {
    has_scoped_lambda(params)
        && params.iter().any(|param| {
            matches!(
                param.abi,
                CleanParamAbi::Borrow(_) | CleanParamAbi::MutPlace(_)
            )
        })
}

pub fn merge_conversions(
    items: impl IntoIterator<Item = BoundaryConversion>,
) -> BoundaryConversion {
    items
        .into_iter()
        .fold(BoundaryConversion::Direct, |current, item| {
            match (current, item) {
                (BoundaryConversion::Unsupported, _) | (_, BoundaryConversion::Unsupported) => {
                    BoundaryConversion::Unsupported
                }
                (BoundaryConversion::NeedsWrapper, _) | (_, BoundaryConversion::NeedsWrapper) => {
                    BoundaryConversion::NeedsWrapper
                }
                (BoundaryConversion::Direct, BoundaryConversion::Direct) => {
                    BoundaryConversion::Direct
                }
            }
        })
}

fn mut_place_macro_payload_supported(ty: &CleanType) -> bool {
    match ty {
        CleanType::Bool | CleanType::Int | CleanType::Float => true,
        CleanType::Option(inner) => mut_place_macro_payload_supported(inner),
        CleanType::Void | CleanType::String | CleanType::Callback(_) | CleanType::List(_) => false,
    }
}

fn param_conversion(ty: &CleanType, flow: CleanFlow) -> BoundaryConversion {
    if flow == CleanFlow::MutBorrow {
        return BoundaryConversion::Direct;
    }
    match ty {
        CleanType::Callback(_) => BoundaryConversion::NeedsWrapper,
        CleanType::Option(_) | CleanType::List(_) => BoundaryConversion::Unsupported,
        _ => BoundaryConversion::Direct,
    }
}

fn return_conversion_for_type(ty: &CleanType) -> BoundaryConversion {
    match ty {
        CleanType::Option(inner) if return_option_inner_supported(inner) => {
            BoundaryConversion::Direct
        }
        CleanType::Callback(_) | CleanType::Option(_) | CleanType::List(_) => {
            BoundaryConversion::Unsupported
        }
        _ => BoundaryConversion::Direct,
    }
}

fn return_option_inner_supported(ty: &CleanType) -> bool {
    matches!(
        ty,
        CleanType::Bool | CleanType::Int | CleanType::Float | CleanType::String
    )
}

fn reject_wrapper_element(ty: &Type, classified: &CleanType, flow: CleanFlow) -> syn::Result<()> {
    if *classified == CleanType::Void {
        return Err(syn::Error::new_spanned(
            ty,
            "wrapper element types cannot be void",
        ));
    }
    if flow != CleanFlow::Value {
        return Err(syn::Error::new_spanned(
            ty,
            "wrapper element types cannot be borrowed",
        ));
    }
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Position {
    Param,
    Return,
    WrapperElement,
}

fn path_is(path: &TypePath, segments: &[&str]) -> bool {
    path.path.segments.len() == segments.len()
        && path
            .path
            .segments
            .iter()
            .zip(segments)
            .all(|(actual, expected)| actual.ident == expected)
}

fn scoped_lambda_type(path: &TypePath) -> syn::Result<Option<CleanCallback>> {
    if !path_is(path, &["ScopedLambda"]) && !path_is(path, &["anvyx_runtime", "ScopedLambda"]) {
        return Ok(None);
    }
    let segment = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        return Err(syn::Error::new_spanned(
            segment,
            "ScopedLambda parameters must be `ScopedLambda<'_, '_, Args, Ret>`",
        ));
    };
    if args.args.len() != 4 {
        return Err(syn::Error::new_spanned(
            args,
            "ScopedLambda parameters must be `ScopedLambda<'_, '_, Args, Ret>`",
        ));
    }
    let mut args = args.args.iter();
    for arg in [
        args.next().expect("checked len"),
        args.next().expect("checked len"),
    ] {
        if !valid_inferred_lifetime(arg) {
            return Err(syn::Error::new_spanned(
                arg,
                "ScopedLambda lifetimes must be `'_`",
            ));
        }
    }
    let GenericArgument::Type(args_ty) = args.next().expect("checked len") else {
        return Err(syn::Error::new_spanned(
            segment,
            "ScopedLambda args must be a tuple type",
        ));
    };
    let params = scoped_lambda_params(args_ty)?;
    let GenericArgument::Type(ret_ty) = args.next().expect("checked len") else {
        return Err(syn::Error::new_spanned(
            segment,
            "ScopedLambda return must be a type",
        ));
    };
    let (ret, flow) = classify_type(ret_ty, Position::WrapperElement)?;
    if flow != CleanFlow::Value || !callback_return_supported(&ret) {
        return Err(syn::Error::new_spanned(
            ret_ty,
            "unsupported ScopedLambda return type",
        ));
    }
    Ok(Some(CleanCallback {
        params,
        ret: Box::new(ret),
    }))
}

fn scoped_lambda_params(ty: &Type) -> syn::Result<Vec<CleanType>> {
    let Type::Tuple(tuple) = ty else {
        return Err(syn::Error::new_spanned(
            ty,
            "ScopedLambda args must be a tuple type",
        ));
    };
    if tuple.elems.len() > SCOPED_LAMBDA_MAX_ARITY {
        return Err(callback_arity_error(ty.span()));
    }
    tuple
        .elems
        .iter()
        .map(|arg| {
            let (ty, flow) = classify_type(arg, Position::WrapperElement)?;
            reject_wrapper_element(arg, &ty, flow)?;
            if !callback_param_supported(&ty) {
                return Err(syn::Error::new_spanned(
                    arg,
                    "unsupported ScopedLambda parameter type",
                ));
            }
            Ok(ty)
        })
        .collect()
}

fn callback_arity_error(span: proc_macro2::Span) -> syn::Error {
    syn::Error::new(
        span,
        format!("ScopedLambda supports at most {SCOPED_LAMBDA_MAX_ARITY} parameters"),
    )
}

fn callback_param_supported(ty: &CleanType) -> bool {
    extern_type_expr(ty).scoped_lambda_param_supported()
}

fn callback_return_supported(ty: &CleanType) -> bool {
    extern_type_expr(ty).scoped_lambda_return_supported()
}

fn extern_callback_signature(callback: &CleanCallback) -> ExternCallbackSignature {
    ExternCallbackSignature {
        params: callback
            .params
            .iter()
            .map(|ty| ExternCallbackParam {
                ty: extern_type_expr(ty),
                escape: CallbackEscape::NonEscaping,
            })
            .collect(),
        ret: Box::new(extern_type_expr(&callback.ret)),
        policy: CallbackPolicy {
            escape: CallbackEscape::NonEscaping,
            thread: CallbackThread::SameThread,
        },
    }
}

fn extern_type_expr(ty: &CleanType) -> ExternTypeExpr {
    match ty {
        CleanType::Void => ExternTypeExpr::Void,
        CleanType::Bool => ExternTypeExpr::Bool,
        CleanType::Int => ExternTypeExpr::Int,
        CleanType::Float => ExternTypeExpr::Float,
        CleanType::String => ExternTypeExpr::String,
        CleanType::Callback(callback) => {
            ExternTypeExpr::Callback(extern_callback_signature(callback))
        }
        CleanType::Option(inner) => ExternTypeExpr::Option(Box::new(extern_type_expr(inner))),
        CleanType::List(inner) => ExternTypeExpr::List(Box::new(extern_type_expr(inner))),
    }
}

pub fn parse_descriptor_type_expr(
    input: ParseStream,
    allow_callback: bool,
) -> syn::Result<CleanType> {
    let mut ty = if input.peek(Token![fn]) {
        if !allow_callback {
            return Err(syn::Error::new(
                input.span(),
                "callbacks are only supported in top-level parameter position",
            ));
        }
        parse_descriptor_callback(input)?
    } else if input.peek(syn::token::Bracket) {
        let content;
        bracketed!(content in input);
        CleanType::List(Box::new(parse_descriptor_type_expr(&content, false)?))
    } else if input.peek(syn::token::Paren) {
        let content;
        syn::parenthesized!(content in input);
        if !content.is_empty() {
            return Err(syn::Error::new(
                content.span(),
                "unsupported descriptor type",
            ));
        }
        CleanType::Void
    } else {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "void" => CleanType::Void,
            "bool" => CleanType::Bool,
            "int" => CleanType::Int,
            "float" => CleanType::Float,
            "string" => CleanType::String,
            _ => {
                return Err(syn::Error::new_spanned(
                    ident,
                    "unsupported descriptor type",
                ));
            }
        }
    };
    if input.peek(Token![?]) {
        input.parse::<Token![?]>()?;
        if matches!(ty, CleanType::Callback(_)) {
            return Err(syn::Error::new(
                input.span(),
                "callbacks cannot be optional",
            ));
        }
        ty = CleanType::Option(Box::new(ty));
    }
    Ok(ty)
}

fn parse_descriptor_callback(input: ParseStream) -> syn::Result<CleanType> {
    input.parse::<Token![fn]>()?;
    let content;
    syn::parenthesized!(content in input);
    let mut params = vec![];
    while !content.is_empty() {
        let ty = parse_descriptor_type_expr(&content, false)?;
        if !callback_param_supported(&ty) {
            return Err(syn::Error::new(
                content.span(),
                "unsupported callback parameter type",
            ));
        }
        params.push(ty);
        if content.is_empty() {
            break;
        }
        content.parse::<Token![,]>()?;
    }
    if params.len() > SCOPED_LAMBDA_MAX_ARITY {
        return Err(callback_arity_error(input.span()));
    }
    let ret = if input.peek(Token![->]) {
        input.parse::<Token![->]>()?;
        parse_descriptor_type_expr(input, false)?
    } else {
        CleanType::Void
    };
    if !callback_return_supported(&ret) {
        return Err(syn::Error::new(
            input.span(),
            "unsupported callback return type",
        ));
    }
    Ok(CleanType::Callback(CleanCallback {
        params,
        ret: Box::new(ret),
    }))
}

fn mut_place_type_arg(path: &TypePath) -> syn::Result<Option<&Type>> {
    if !path_is(path, &["MutPlace"]) && !path_is(path, &["anvyx_runtime", "MutPlace"]) {
        return Ok(None);
    }
    let segment = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        return Err(syn::Error::new_spanned(
            segment,
            "MutPlace requires lifetimes and a type argument",
        ));
    };
    if args.args.len() != 3 {
        return Err(syn::Error::new_spanned(
            args,
            "MutPlace parameters must be `MutPlace<'_, 'cx, T>`",
        ));
    }
    let mut args = args.args.iter();
    let region = args.next().expect("checked len");
    if !valid_inferred_lifetime(region) {
        return Err(syn::Error::new_spanned(
            region,
            "MutPlace first lifetime must be `'_`",
        ));
    }
    let cx = args.next().expect("checked len");
    if !matches!(cx, GenericArgument::Lifetime(lifetime) if lifetime.ident == "cx") {
        return Err(syn::Error::new_spanned(
            cx,
            "MutPlace second lifetime must be `'cx`",
        ));
    }
    match args.next().expect("checked len") {
        GenericArgument::Type(ty) => Ok(Some(ty)),
        arg => Err(syn::Error::new_spanned(
            arg,
            "MutPlace requires a type argument",
        )),
    }
}

fn one_type_arg<'a>(path: &'a TypePath, names: &[&[&str]]) -> syn::Result<Option<&'a Type>> {
    if !names.iter().any(|name| path_is(path, name)) {
        return Ok(None);
    }
    let name = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(args) = &name.arguments else {
        return Err(syn::Error::new_spanned(
            name,
            "wrapper type requires one type argument",
        ));
    };
    if args.args.len() != 1 {
        return Err(syn::Error::new_spanned(
            args,
            "wrapper type requires one type argument",
        ));
    }
    match args.args.first().expect("checked len") {
        GenericArgument::Type(ty) => Ok(Some(ty)),
        arg => Err(syn::Error::new_spanned(
            arg,
            "wrapper type requires a type argument",
        )),
    }
}

fn result_args(ty: &Type) -> syn::Result<Option<(&Type, &Type)>> {
    let Type::Path(path) = ty else {
        return Ok(None);
    };
    if !path_is(path, &["Result"]) && !path_is(path, &["std", "result", "Result"]) {
        return Ok(None);
    }
    let segment = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        return Err(syn::Error::new_spanned(
            segment,
            "Result requires two type arguments",
        ));
    };
    if args.args.len() != 2 {
        return Err(syn::Error::new_spanned(
            args,
            "Result requires two type arguments",
        ));
    }
    let mut args = args.args.iter();
    let GenericArgument::Type(ok) = args.next().expect("checked len") else {
        return Err(syn::Error::new_spanned(
            segment,
            "Result requires type arguments",
        ));
    };
    let GenericArgument::Type(err) = args.next().expect("checked len") else {
        return Err(syn::Error::new_spanned(
            segment,
            "Result requires type arguments",
        ));
    };
    Ok(Some((ok, err)))
}

pub fn parse_type_expr(text: &str) -> syn::Result<CleanType> {
    let text = text.trim();
    if text.starts_with("fn") {
        return parse_descriptor_type_text(text, true);
    }
    if let Ok(ty) = parse_descriptor_type_text(text, false) {
        return Ok(ty);
    }
    if let Some(inner) = text
        .strip_prefix("Option<")
        .and_then(|rest| rest.strip_suffix('>'))
    {
        return Ok(CleanType::Option(Box::new(parse_wrapper_override(inner)?)));
    }
    if let Some(inner) = text.strip_suffix('?') {
        return Ok(CleanType::Option(Box::new(parse_wrapper_override(inner)?)));
    }
    if let Some(inner) = text
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
    {
        return Ok(CleanType::List(Box::new(parse_wrapper_override(inner)?)));
    }
    Err(syn::Error::new(
        proc_macro2::Span::call_site(),
        format!("unsupported extern type override `{text}`"),
    ))
}

fn parse_wrapper_override(text: &str) -> syn::Result<CleanType> {
    let ty = parse_type_expr(text)?;
    if matches!(ty, CleanType::Callback(_)) {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "callbacks are only supported in top-level parameter position",
        ));
    }
    Ok(ty)
}

fn parse_descriptor_type_text(text: &str, allow_callback: bool) -> syn::Result<CleanType> {
    (|input: ParseStream| {
        let ty = parse_descriptor_type_expr(input, allow_callback)?;
        if !input.is_empty() {
            return Err(syn::Error::new(
                input.span(),
                format!("unsupported extern type override `{text}`"),
            ));
        }
        Ok(ty)
    })
    .parse_str(text)
}

fn is_runtime_error(ty: &Type) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };
    path_is(path, &["RuntimeError"]) || path_is(path, &["anvyx_runtime", "RuntimeError"])
}

#[cfg(test)]
mod tests {
    use quote::quote;
    use syn::{ItemFn, parse_quote};

    use super::*;

    fn first_param(tokens: TokenStream) -> syn::Result<CleanParam> {
        first_param_with_ctx(tokens, false)
    }

    fn first_param_with_ctx(tokens: TokenStream, has_ctx: bool) -> syn::Result<CleanParam> {
        let func: ItemFn = syn::parse2(quote! { fn f(#tokens) {} }).unwrap();
        let Some(syn::FnArg::Typed(param)) = func.sig.inputs.first() else {
            unreachable!();
        };
        classify_param(param, has_ctx)
    }

    fn ret(output: ReturnType) -> syn::Result<CleanReturn> {
        classify_return(&output)
    }

    #[test]
    fn maps_primitive_params() {
        assert_eq!(first_param(quote! { x: bool }).unwrap().ty, CleanType::Bool);
        assert_eq!(first_param(quote! { x: i64 }).unwrap().ty, CleanType::Int);
        assert_eq!(first_param(quote! { x: f64 }).unwrap().ty, CleanType::Float);
        assert_eq!(
            first_param(quote! { x: String }).unwrap().ty,
            CleanType::String
        );
    }

    #[test]
    fn maps_borrow_string_param() {
        let param = first_param(quote! { x: &str }).unwrap();

        assert_eq!(param.ty, CleanType::String);
        assert_eq!(param.flow, CleanFlow::Borrow);
        assert_eq!(param.abi, CleanParamAbi::Borrow(CleanType::String));
    }

    #[test]
    fn maps_mut_place_param() {
        let param = first_param_with_ctx(
            quote! { value: anvyx_runtime::MutPlace<'_, 'cx, i64> },
            true,
        )
        .unwrap();

        assert_eq!(param.ty, CleanType::Int);
        assert_eq!(param.flow, CleanFlow::MutBorrow);
        assert_eq!(param.abi, CleanParamAbi::MutPlace(CleanType::Int));
        assert_eq!(param.conversion, BoundaryConversion::Direct);
    }

    #[test]
    fn rejects_mut_place_without_ctx_or_runtime_payload() {
        assert!(first_param(quote! { value: MutPlace<'_, 'cx, i64> }).is_err());
        assert!(
            first_param_with_ctx(quote! { value: MutPlace<'_, 'cx, String> }, true)
                .unwrap_err()
                .to_string()
                .contains("only support")
        );
        assert!(
            first_param_with_ctx(quote! { value: MutPlace<'_, 'cx, Vec<i64>> }, true)
                .unwrap_err()
                .to_string()
                .contains("only support")
        );
        assert_eq!(
            first_param_with_ctx(quote! { value: MutPlace<'_, 'cx, Option<i64>> }, true)
                .unwrap()
                .ty,
            CleanType::Option(Box::new(CleanType::Int))
        );
    }

    #[test]
    fn maps_wrappers() {
        assert_eq!(
            first_param(quote! { x: Option<i64> }).unwrap().ty,
            CleanType::Option(Box::new(CleanType::Int))
        );
        assert_eq!(
            first_param(quote! { x: Vec<String> }).unwrap().ty,
            CleanType::List(Box::new(CleanType::String))
        );
    }

    #[test]
    fn maps_returns_and_fallible_result() {
        assert_eq!(ret(ReturnType::Default).unwrap().ty, CleanType::Void);
        assert_eq!(
            ret(parse_quote! { -> f64 }).unwrap().abi,
            CleanReturnAbi::Value(CleanType::Float)
        );
        let result = ret(parse_quote! { -> Result<i64, RuntimeError> }).unwrap();
        assert!(result.fallible);
        assert_eq!(result.ty, CleanType::Int);
    }

    #[test]
    fn rejects_string_scoped_lambda_elements() {
        assert!(
            first_param(quote! { f: ScopedLambda<'_, '_, (String,), ()> })
                .unwrap_err()
                .to_string()
                .contains("unsupported ScopedLambda parameter type")
        );
        assert!(
            first_param(quote! { f: ScopedLambda<'_, '_, (i64,), String> })
                .unwrap_err()
                .to_string()
                .contains("unsupported ScopedLambda return type")
        );
        assert!(
            parse_type_expr("fn(string) -> void")
                .unwrap_err()
                .to_string()
                .contains("unsupported callback parameter type")
        );
    }

    #[test]
    fn parses_callback_override_with_unit_return() {
        assert_eq!(
            parse_type_expr("fn(int) -> ()").unwrap(),
            CleanType::Callback(CleanCallback {
                params: vec![CleanType::Int],
                ret: Box::new(CleanType::Void),
            })
        );
    }

    #[test]
    fn rejects_scoped_lambda_above_max_arity() {
        let args = quote! { (i64, i64, i64, i64, i64, i64, i64, i64, i64) };
        assert!(
            first_param(quote! { f: ScopedLambda<'_, '_, #args, ()> })
                .unwrap_err()
                .to_string()
                .contains("at most 8")
        );
        assert!(
            parse_type_expr("fn(int, int, int, int, int, int, int, int, int)")
                .unwrap_err()
                .to_string()
                .contains("at most 8")
        );
    }

    #[test]
    fn rejects_nested_callback_overrides() {
        assert!(parse_type_expr("Option<fn(int)>").is_err());
        assert!(parse_type_expr("[fn(int)]").is_err());
    }

    #[test]
    fn rejects_unsupported_types() {
        assert!(first_param(quote! { x: f32 }).is_err());
        assert!(first_param(quote! { x: usize }).is_err());
        assert!(first_param(quote! { x: &i64 }).is_err());
        assert!(first_param(quote! { x: () }).is_err());
        assert!(first_param(quote! { x: Option<()> }).is_err());
        assert!(first_param(quote! { x: Option<&str> }).is_err());
        assert!(first_param(quote! { x: my_crate::Option<i64> }).is_err());
        assert!(ret(parse_quote! { -> &str }).is_err());
        assert!(ret(parse_quote! { -> Result<i64, String> }).is_err());
        assert!(ret(parse_quote! { -> foo::Result<i64, RuntimeError> }).is_err());
    }
}
