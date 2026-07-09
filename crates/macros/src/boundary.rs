use anvyx_externs::{
    AbiPosition, CALLBACK_WRAPPER_MAX_ARITY, CallbackEscape, CallbackPolicy, CallbackThread,
    ExternCallbackParam, ExternCallbackSignature, ExternTypeExpr,
};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use syn::{
    GenericArgument, Ident, PathArguments, ReturnType, Signature, Token, Type, TypePath, bracketed,
    parse::{ParseStream, Parser},
    spanned::Spanned,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundaryParam {
    pub name: String,
    pub ty: BoundaryType,
    pub flow: ParamFlow,
    pub abi: ParamAbi,
    pub conversion: BoundaryConversion,
    pub init_presence: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundaryReturn {
    pub ty: BoundaryType,
    pub abi: ReturnAbi,
    pub fallible: bool,
    pub conversion: BoundaryConversion,
}

#[derive(Clone, Copy)]
pub struct OwnerReturn<'a> {
    pub rust_owner: &'a Ident,
    pub export_name: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BoundaryType {
    Void,
    Unit,
    Bool,
    Int,
    Float,
    String,
    Char,
    Named(String),
    Callback(BoundaryCallback),
    Option(Box<BoundaryType>),
    Result(Box<BoundaryType>, Box<BoundaryType>),
    Tuple(Vec<BoundaryType>),
    Array { elem: Box<BoundaryType>, len: u64 },
    List(Box<BoundaryType>),
    Map(Box<BoundaryType>, Box<BoundaryType>),
    Slice(Box<BoundaryType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BoundaryCallback {
    pub params: Vec<BoundaryType>,
    pub ret: Box<BoundaryType>,
    pub escape: CallbackEscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamFlow {
    Value,
    Borrow,
    MutBorrow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParamAbi {
    Value(BoundaryType),
    OwnedNamed(BoundaryType),
    Borrow(BoundaryType),
    MutPlace(BoundaryType),
    ScopedLambda(BoundaryCallback),
    EscapingLambda(BoundaryCallback),
    AnvCallback(BoundaryCallback),
    InitField(Box<ParamAbi>),
    Option(Box<ParamAbi>),
    Result(Box<ParamAbi>, Box<ParamAbi>),
    Slice(Box<ParamAbi>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReturnAbi {
    Void,
    Value(BoundaryType),
    OwnedNamed(BoundaryType),
    Option(Box<ReturnAbi>),
    Result(Box<ReturnAbi>, Box<ReturnAbi>),
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
    params: &[BoundaryParam],
    macro_name: &str,
) -> syn::Result<()> {
    if !params
        .iter()
        .any(|param| param.flow == ParamFlow::MutBorrow)
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

pub fn classify_param(pat_ty: &syn::PatType, has_ctx: bool) -> syn::Result<BoundaryParam> {
    let syn::Pat::Ident(ident) = pat_ty.pat.as_ref() else {
        return Err(syn::Error::new_spanned(
            &pat_ty.pat,
            "#[function] parameters must be identifiers",
        ));
    };
    let (ty, flow) = classify_type(&pat_ty.ty, Position::Param)?;
    if flow == ParamFlow::MutBorrow && !has_ctx {
        return Err(syn::Error::new_spanned(
            &pat_ty.ty,
            "MutPlace parameters require #[function(ctx)]",
        ));
    }
    if flow == ParamFlow::MutBorrow && !mut_place_macro_payload_supported(&ty) {
        return Err(syn::Error::new_spanned(
            &pat_ty.ty,
            "#[function(ctx)] MutPlace parameters only support bool, i64, f64, char, and Option of those payloads",
        ));
    }
    if ty == BoundaryType::Void {
        return Err(syn::Error::new_spanned(
            &pat_ty.ty,
            "#[function] parameters cannot be void",
        ));
    }
    let abi = param_abi_for_source(&pat_ty.ty, &ty, flow)?;
    let conversion = param_conversion(&ty, flow);
    Ok(BoundaryParam {
        name: ident.ident.to_string(),
        ty,
        flow,
        abi,
        conversion,
        init_presence: false,
    })
}

pub fn classify_init_param(pat_ty: &syn::PatType) -> syn::Result<BoundaryParam> {
    let syn::Pat::Ident(ident) = pat_ty.pat.as_ref() else {
        return Err(syn::Error::new_spanned(
            &pat_ty.pat,
            "#[methods] init parameters must be identifiers",
        ));
    };
    let Some(inner) = init_field_type_arg(&pat_ty.ty)? else {
        return classify_param(pat_ty, false);
    };
    let (ty, flow) = classify_type(inner, Position::Param)?;
    if flow != ParamFlow::Value {
        return Err(syn::Error::new_spanned(
            inner,
            "AnvInitField<T> only supports value payloads",
        ));
    }
    if ty == BoundaryType::Void {
        return Err(syn::Error::new_spanned(
            inner,
            "AnvInitField<T> payload cannot be void",
        ));
    }
    let abi = param_abi_for_source(inner, &ty, flow)?;
    let conversion = param_conversion(&ty, flow);
    Ok(BoundaryParam {
        name: ident.ident.to_string(),
        ty,
        flow,
        abi: ParamAbi::InitField(Box::new(abi)),
        conversion,
        init_presence: true,
    })
}

pub fn classify_return(output: &ReturnType) -> syn::Result<BoundaryReturn> {
    classify_provider_return(output)
}

pub fn classify_provider_return(output: &ReturnType) -> syn::Result<BoundaryReturn> {
    classify_provider_return_inner(output, None)
}

pub fn classify_provider_return_for_owner(
    output: &ReturnType,
    owner: OwnerReturn<'_>,
) -> syn::Result<BoundaryReturn> {
    classify_provider_return_inner(output, Some(owner))
}

fn classify_provider_return_inner(
    output: &ReturnType,
    owner: Option<OwnerReturn<'_>>,
) -> syn::Result<BoundaryReturn> {
    match output {
        ReturnType::Default => Ok(BoundaryReturn {
            ty: BoundaryType::Void,
            abi: ReturnAbi::Void,
            fallible: false,
            conversion: BoundaryConversion::Direct,
        }),
        ReturnType::Type(_, ty) => classify_return_type(ty, owner),
    }
}

fn classify_hidden_return_payload_inner(
    source: &Type,
    owner: Option<OwnerReturn<'_>>,
) -> syn::Result<BoundaryReturn> {
    let (ty, _) = classify_type_inner(source, Position::Return, owner)?;
    let abi = return_abi_for_source(source, &ty)?;
    let conversion = return_conversion_for_type(&ty);
    Ok(BoundaryReturn {
        ty,
        abi,
        fallible: true,
        conversion,
    })
}

fn classify_return_type(
    source: &Type,
    owner: Option<OwnerReturn<'_>>,
) -> syn::Result<BoundaryReturn> {
    if let Some(ok) = runtime_result_type_arg(source)? {
        return classify_hidden_return_payload_inner(ok, owner);
    }
    if let Some((ok_ty, err_ty)) = result_args(source)? {
        if is_runtime_error(err_ty) {
            return Err(syn::Error::new_spanned(
                err_ty,
                "Result<T, RuntimeError> is a hidden runtime failure shape; use RuntimeResult<T>",
            ));
        }
        let (ok, ok_flow) = classify_type_inner(ok_ty, Position::WrapperElement, owner)?;
        reject_wrapper_element(source, &ok, ok_flow)?;
        let (err, err_flow) = classify_type_inner(err_ty, Position::WrapperElement, owner)?;
        reject_wrapper_element(source, &err, err_flow)?;
        let abi = ReturnAbi::Result(
            Box::new(return_abi_for_source(ok_ty, &ok)?),
            Box::new(return_abi_for_source(err_ty, &err)?),
        );
        let ty = BoundaryType::Result(Box::new(ok), Box::new(err));
        return Ok(BoundaryReturn {
            abi,
            conversion: return_conversion_for_type(&ty),
            ty,
            fallible: false,
        });
    }
    let (ty, _) = classify_type_inner(source, Position::Return, owner)?;
    let abi = return_abi_for_source(source, &ty)?;
    let conversion = return_conversion_for_type(&ty);
    Ok(BoundaryReturn {
        ty,
        abi,
        fallible: false,
        conversion,
    })
}

pub fn classify_field_type(source: &Type) -> syn::Result<BoundaryType> {
    let (ty, flow) = classify_type(source, Position::WrapperElement)?;
    if flow != ParamFlow::Value {
        return Err(syn::Error::new_spanned(
            source,
            "#[anvyx(field)] fields cannot be borrowed",
        ));
    }
    reject_wrapper_element(source, &ty, flow)?;
    Ok(ty)
}

pub(crate) fn classify_type(
    ty: &Type,
    position: Position,
) -> syn::Result<(BoundaryType, ParamFlow)> {
    classify_type_inner(ty, position, None)
}

fn classify_type_inner(
    ty: &Type,
    position: Position,
    owner: Option<OwnerReturn<'_>>,
) -> syn::Result<(BoundaryType, ParamFlow)> {
    match ty {
        Type::Tuple(tuple) if tuple.elems.is_empty() => Ok((BoundaryType::Unit, ParamFlow::Value)),
        Type::Tuple(tuple) => {
            let fields = tuple
                .elems
                .iter()
                .map(|field| {
                    let (ty, flow) = classify_type_inner(field, Position::WrapperElement, owner)?;
                    reject_wrapper_element(field, &ty, flow)?;
                    Ok(ty)
                })
                .collect::<syn::Result<Vec<_>>>()?;
            Ok((BoundaryType::Tuple(fields), ParamFlow::Value))
        }
        Type::Array(array) => {
            let syn::Expr::Lit(len) = &array.len else {
                return Err(syn::Error::new_spanned(
                    &array.len,
                    "array ABI lengths must be integer literals",
                ));
            };
            let syn::Lit::Int(len) = &len.lit else {
                return Err(syn::Error::new_spanned(
                    &array.len,
                    "array ABI lengths must be integer literals",
                ));
            };
            let (elem, flow) = classify_type_inner(&array.elem, Position::WrapperElement, owner)?;
            reject_wrapper_element(&array.elem, &elem, flow)?;
            Ok((
                BoundaryType::Array {
                    elem: Box::new(elem),
                    len: len.base10_parse()?,
                },
                ParamFlow::Value,
            ))
        }
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
                return Ok((BoundaryType::String, ParamFlow::Borrow));
            }
            Err(syn::Error::new_spanned(
                reference,
                "#[function] only supports &str references in the MVP",
            ))
        }
        Type::Path(path) => classify_path(path, position, owner),
        _ => Err(syn::Error::new_spanned(
            ty,
            "unsupported #[function] boundary type",
        )),
    }
}

fn classify_path(
    path: &TypePath,
    position: Position,
    owner: Option<OwnerReturn<'_>>,
) -> syn::Result<(BoundaryType, ParamFlow)> {
    if path.qself.is_some() {
        return Err(syn::Error::new_spanned(
            path,
            "qualified boundary types are not supported",
        ));
    }
    if runtime_result_type_arg_from_path(path)?.is_some() {
        return Err(syn::Error::new_spanned(
            path,
            "RuntimeResult<T> is only supported as a provider return type",
        ));
    }
    if one_type_arg(
        path,
        &[&["AnvInitField"], &["anvyx_runtime", "AnvInitField"]],
    )?
    .is_some()
    {
        return Err(syn::Error::new_spanned(
            path,
            "AnvInitField<T> is only supported in #[anvyx(init)] parameters",
        ));
    }
    if let Some(callback) = callback_wrapper_type(path)? {
        if position != Position::Param {
            return Err(syn::Error::new_spanned(
                path,
                "callback wrappers are only supported in parameter position",
            ));
        }
        return Ok((BoundaryType::Callback(callback), ParamFlow::Value));
    }
    if let Some(inner) = mut_place_type_arg(path)? {
        if position != Position::Param {
            return Err(syn::Error::new_spanned(
                path,
                "MutPlace is only supported in parameter position",
            ));
        }
        let (ty, flow) = classify_type_inner(inner, Position::WrapperElement, owner)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok((ty, ParamFlow::MutBorrow));
    }
    if let Some(owner) = owner_name(path, owner) {
        return Ok((BoundaryType::Named(owner), ParamFlow::Value));
    }
    if path_is(path, &["bool"]) {
        return Ok((BoundaryType::Bool, ParamFlow::Value));
    }
    if path_is(path, &["i64"]) {
        return Ok((BoundaryType::Int, ParamFlow::Value));
    }
    if path_is(path, &["f64"]) {
        return Ok((BoundaryType::Float, ParamFlow::Value));
    }
    if path_is(path, &["char"]) {
        return Ok((BoundaryType::Char, ParamFlow::Value));
    }
    if path_is(path, &["String"]) || path_is(path, &["std", "string", "String"]) {
        return Err(syn::Error::new_spanned(
            path,
            "String is not a provider boundary carrier; use AnvString for owned strings or &str for borrowed parameters",
        ));
    }
    if path_is(path, &["AnvString"]) || path_is(path, &["anvyx_runtime", "AnvString"]) {
        return Ok((BoundaryType::String, ParamFlow::Value));
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
        let (ty, flow) = classify_type_inner(inner, Position::WrapperElement, owner)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok((BoundaryType::Option(Box::new(ty)), ParamFlow::Value));
    }
    if let Some((ok_ty, err_ty)) = result_type_args(path)? {
        if is_runtime_error(err_ty) {
            return Err(syn::Error::new_spanned(
                err_ty,
                "Result<T, RuntimeError> is a hidden runtime failure shape; use RuntimeResult<T> only as a return type",
            ));
        }
        let (ok, ok_flow) = classify_type_inner(ok_ty, Position::WrapperElement, owner)?;
        reject_wrapper_element(ok_ty, &ok, ok_flow)?;
        let (err, err_flow) = classify_type_inner(err_ty, Position::WrapperElement, owner)?;
        reject_wrapper_element(err_ty, &err, err_flow)?;
        return Ok((
            BoundaryType::Result(Box::new(ok), Box::new(err)),
            ParamFlow::Value,
        ));
    }
    if one_type_arg(path, &[&["Vec"], &["std", "vec", "Vec"]])?.is_some() {
        return Err(syn::Error::new_spanned(
            path,
            "Vec<T> is not a provider boundary carrier; use AnvList<'cx, T> or explicit runtime copy helpers",
        ));
    }
    if map_type_args(
        path,
        &[
            &["HashMap"],
            &["BTreeMap"],
            &["std", "collections", "HashMap"],
            &["std", "collections", "BTreeMap"],
        ],
    )?
    .is_some()
    {
        return Err(syn::Error::new_spanned(
            path,
            "std maps are not provider boundary carriers; use AnvMap<'cx, K, V> or explicit runtime copy helpers",
        ));
    }
    if let Some(args) =
        runtime_cx_type_args(path, &[&["AnvList"], &["anvyx_runtime", "AnvList"]], 1)?
    {
        let inner = args[0];
        let (ty, flow) = classify_type_inner(inner, Position::WrapperElement, owner)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok((BoundaryType::List(Box::new(ty)), ParamFlow::Value));
    }
    if let Some(args) = runtime_cx_type_args(path, &[&["AnvMap"], &["anvyx_runtime", "AnvMap"]], 2)?
    {
        let (key, key_flow) = classify_type_inner(args[0], Position::WrapperElement, owner)?;
        reject_wrapper_element(args[0], &key, key_flow)?;
        let (value, value_flow) = classify_type_inner(args[1], Position::WrapperElement, owner)?;
        reject_wrapper_element(args[1], &value, value_flow)?;
        return Ok((
            BoundaryType::Map(Box::new(key), Box::new(value)),
            ParamFlow::Value,
        ));
    }
    if let Some(args) =
        runtime_cx_type_args(path, &[&["AnvSlice"], &["anvyx_runtime", "AnvSlice"]], 1)?
    {
        if position != Position::Param {
            return Err(syn::Error::new_spanned(
                path,
                "AnvSlice is only supported in parameter position",
            ));
        }
        let inner = args[0];
        let (ty, flow) = classify_type_inner(inner, Position::WrapperElement, owner)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok((BoundaryType::Slice(Box::new(ty)), ParamFlow::Value));
    }
    if let Some(resource) = anv_ref_type_arg_for_owner(path, owner)? {
        return Ok((BoundaryType::Named(resource), ParamFlow::Value));
    }
    if path.path.segments.len() == 1 {
        let segment = path.path.segments.first().expect("checked len");
        if segment.ident == "Self" {
            return Err(syn::Error::new_spanned(
                path,
                "Self is not supported in #[function] boundaries",
            ));
        }
        if reserved_named_boundary_type(&segment.ident) {
            return Err(syn::Error::new_spanned(
                path,
                "unsupported #[function] boundary type",
            ));
        }
        if matches!(segment.arguments, PathArguments::None) {
            return Ok((
                BoundaryType::Named(segment.ident.to_string()),
                ParamFlow::Value,
            ));
        }
        return Err(syn::Error::new_spanned(
            path,
            "generic named boundary types are not supported",
        ));
    }
    Err(syn::Error::new_spanned(
        path,
        "unsupported #[function] boundary type",
    ))
}

pub fn type_with_override(
    inferred: &BoundaryType,
    override_ty: Option<&str>,
    span: proc_macro2::Span,
    mismatch: impl Into<String>,
) -> syn::Result<BoundaryType> {
    let Some(override_ty) = override_ty else {
        return Ok(inferred.clone());
    };
    let override_ty = parse_type_expr(override_ty)?;
    if override_ty == *inferred
        || matches!(
            (inferred, &override_ty),
            (BoundaryType::Named(_), BoundaryType::Named(_))
        )
    {
        Ok(override_ty)
    } else {
        Err(syn::Error::new(span, mismatch.into()))
    }
}

pub fn type_expr_tokens(ty: &BoundaryType) -> TokenStream {
    type_expr_tokens_at(ty, &quote! { anvyx_runtime })
}

pub fn named_type_expr_tokens(name: &str) -> TokenStream {
    extern_type_expr_tokens(&named_type_expr(name), &quote! { anvyx_runtime })
}

pub fn externs_type_expr_tokens(ty: &BoundaryType) -> TokenStream {
    type_expr_tokens_at(ty, &quote! { anvyx_externs })
}

fn type_expr_tokens_at(ty: &BoundaryType, root: &TokenStream) -> TokenStream {
    extern_type_expr_tokens(&extern_type_expr(ty), root)
}

fn extern_type_expr_tokens(ty: &ExternTypeExpr, root: &TokenStream) -> TokenStream {
    match ty {
        ExternTypeExpr::Void => quote! { #root::ExternTypeExpr::Void },
        ExternTypeExpr::Unit => quote! { #root::ExternTypeExpr::Unit },
        ExternTypeExpr::Bool => quote! { #root::ExternTypeExpr::Bool },
        ExternTypeExpr::Int => quote! { #root::ExternTypeExpr::Int },
        ExternTypeExpr::Float => quote! { #root::ExternTypeExpr::Float },
        ExternTypeExpr::String => quote! { #root::ExternTypeExpr::String },
        ExternTypeExpr::Char => quote! { #root::ExternTypeExpr::Char },
        ExternTypeExpr::Any => quote! { #root::ExternTypeExpr::Any },
        ExternTypeExpr::Named { module, name, args } => {
            let module = if let Some(module) = module {
                let segments = module.segments.iter();
                quote! { Some(#root::ModulePath { segments: vec![#(#segments.to_string()),*] }) }
            } else {
                quote! { None }
            };
            let args = args.iter().map(|arg| extern_type_expr_tokens(arg, root));
            quote! {
                #root::ExternTypeExpr::Named {
                    module: #module,
                    name: #name.to_string(),
                    args: vec![#(#args),*],
                }
            }
        }
        ExternTypeExpr::Callback(callback) => {
            let signature = callback_signature_tokens_at(callback, root);
            quote! { #root::ExternTypeExpr::Callback(#signature) }
        }
        ExternTypeExpr::Option(inner) => {
            let inner = extern_type_expr_tokens(inner, root);
            quote! { #root::ExternTypeExpr::Option(Box::new(#inner)) }
        }
        ExternTypeExpr::Result(ok, err) => {
            let ok = extern_type_expr_tokens(ok, root);
            let err = extern_type_expr_tokens(err, root);
            quote! { #root::ExternTypeExpr::Result(Box::new(#ok), Box::new(#err)) }
        }
        ExternTypeExpr::Tuple(fields) => {
            let fields = fields
                .iter()
                .map(|field| extern_type_expr_tokens(field, root));
            quote! { #root::ExternTypeExpr::Tuple(vec![#(#fields),*]) }
        }
        ExternTypeExpr::Array { elem, len } => {
            let elem = extern_type_expr_tokens(elem, root);
            quote! { #root::ExternTypeExpr::Array { elem: Box::new(#elem), len: #len } }
        }
        ExternTypeExpr::List(inner) => {
            let inner = extern_type_expr_tokens(inner, root);
            quote! { #root::ExternTypeExpr::List(Box::new(#inner)) }
        }
        ExternTypeExpr::Map(key, value) => {
            let key = extern_type_expr_tokens(key, root);
            let value = extern_type_expr_tokens(value, root);
            quote! { #root::ExternTypeExpr::Map(Box::new(#key), Box::new(#value)) }
        }
        ExternTypeExpr::Slice(inner) => {
            let inner = extern_type_expr_tokens(inner, root);
            quote! { #root::ExternTypeExpr::Slice(Box::new(#inner)) }
        }
    }
}

fn callback_signature_tokens(callback: &ExternCallbackSignature) -> TokenStream {
    callback_signature_tokens_at(callback, &quote! { anvyx_runtime })
}

fn callback_signature_tokens_at(
    callback: &ExternCallbackSignature,
    root: &TokenStream,
) -> TokenStream {
    let params = callback.params.iter().map(|param| {
        let ty = extern_type_expr_tokens(&param.ty, root);
        let escape = callback_escape_tokens_at(param.escape, root);
        quote! {
            #root::ExternCallbackParam {
                ty: #ty,
                escape: #escape,
            }
        }
    });
    let ret = extern_type_expr_tokens(&callback.ret, root);
    let escape = callback_escape_tokens_at(callback.policy.escape, root);
    quote! {
        #root::ExternCallbackSignature {
            params: vec![#(#params),*],
            ret: Box::new(#ret),
            policy: #root::CallbackPolicy {
                escape: #escape,
                thread: #root::CallbackThread::SameThread,
            },
        }
    }
}

fn callback_escape_tokens(escape: CallbackEscape) -> TokenStream {
    callback_escape_tokens_at(escape, &quote! { anvyx_runtime })
}

fn callback_escape_tokens_at(escape: CallbackEscape, root: &TokenStream) -> TokenStream {
    match escape {
        CallbackEscape::NonEscaping => quote! { #root::CallbackEscape::NonEscaping },
        CallbackEscape::Escaping => quote! { #root::CallbackEscape::Escaping },
    }
}

pub fn flow_tokens(flow: ParamFlow) -> TokenStream {
    match flow {
        ParamFlow::Value => quote! { anvyx_runtime::ParamFlow::Value },
        ParamFlow::Borrow => quote! { anvyx_runtime::ParamFlow::Borrow },
        ParamFlow::MutBorrow => quote! { anvyx_runtime::ParamFlow::MutBorrow },
    }
}

pub fn param_escape_tokens(param: &BoundaryParam) -> TokenStream {
    match &param.ty {
        BoundaryType::Callback(callback) => callback_escape_tokens(callback.escape),
        _ => callback_escape_tokens(CallbackEscape::NonEscaping),
    }
}

pub fn param_abi_tokens(abi: &ParamAbi) -> TokenStream {
    match abi {
        ParamAbi::Value(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustParamAbi::Value(#ty) }
        }
        ParamAbi::OwnedNamed(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustParamAbi::OwnedNamed(#ty) }
        }
        ParamAbi::Borrow(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustParamAbi::Borrow(#ty) }
        }
        ParamAbi::MutPlace(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustParamAbi::MutPlace(#ty) }
        }
        ParamAbi::ScopedLambda(callback) => {
            let callback = callback_signature_tokens(&extern_callback_signature(callback));
            quote! { anvyx_runtime::RustParamAbi::ScopedLambda(#callback) }
        }
        ParamAbi::EscapingLambda(callback) => {
            let callback = callback_signature_tokens(&extern_callback_signature(callback));
            quote! { anvyx_runtime::RustParamAbi::EscapingLambda(#callback) }
        }
        ParamAbi::AnvCallback(callback) => {
            let callback = callback_signature_tokens(&extern_callback_signature(callback));
            quote! { anvyx_runtime::RustParamAbi::AnvCallback(#callback) }
        }
        ParamAbi::InitField(inner) => {
            let inner = param_abi_tokens(inner);
            quote! { anvyx_runtime::RustParamAbi::InitField(Box::new(#inner)) }
        }
        ParamAbi::Option(inner) => {
            let inner = param_abi_tokens(inner);
            quote! { anvyx_runtime::RustParamAbi::Option(Box::new(#inner)) }
        }
        ParamAbi::Result(ok, err) => {
            let ok = param_abi_tokens(ok);
            let err = param_abi_tokens(err);
            quote! { anvyx_runtime::RustParamAbi::Result(Box::new(#ok), Box::new(#err)) }
        }
        ParamAbi::Slice(inner) => {
            let inner = param_abi_tokens(inner);
            quote! { anvyx_runtime::RustParamAbi::Slice(Box::new(#inner)) }
        }
    }
}

pub fn return_abi_tokens(abi: &ReturnAbi) -> TokenStream {
    match abi {
        ReturnAbi::Void => quote! { anvyx_runtime::RustReturnAbi::Void },
        ReturnAbi::Value(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustReturnAbi::Value(#ty) }
        }
        ReturnAbi::OwnedNamed(ty) => {
            let ty = type_expr_tokens(ty);
            quote! { anvyx_runtime::RustReturnAbi::OwnedNamed(#ty) }
        }
        ReturnAbi::Option(inner) => {
            let inner = return_abi_tokens(inner);
            quote! { anvyx_runtime::RustReturnAbi::Option(Box::new(#inner)) }
        }
        ReturnAbi::Result(ok, err) => {
            let ok = return_abi_tokens(ok);
            let err = return_abi_tokens(err);
            quote! { anvyx_runtime::RustReturnAbi::Result(Box::new(#ok), Box::new(#err)) }
        }
    }
}

pub fn param_abi(ty: &BoundaryType, flow: ParamFlow) -> ParamAbi {
    match flow {
        ParamFlow::MutBorrow => ParamAbi::MutPlace(ty.clone()),
        ParamFlow::Borrow => ParamAbi::Borrow(ty.clone()),
        ParamFlow::Value => match ty {
            BoundaryType::Option(inner) => {
                ParamAbi::Option(Box::new(param_abi(inner, ParamFlow::Value)))
            }
            BoundaryType::Result(ok, err) => ParamAbi::Result(
                Box::new(param_abi(ok, ParamFlow::Value)),
                Box::new(param_abi(err, ParamFlow::Value)),
            ),
            BoundaryType::Callback(callback) => match callback.escape {
                CallbackEscape::NonEscaping => ParamAbi::ScopedLambda(callback.clone()),
                CallbackEscape::Escaping => ParamAbi::EscapingLambda(callback.clone()),
            },
            BoundaryType::Slice(inner) => {
                ParamAbi::Slice(Box::new(param_abi(inner, ParamFlow::Value)))
            }
            _ => ParamAbi::Value(ty.clone()),
        },
    }
}

fn param_abi_for_source(
    source: &Type,
    ty: &BoundaryType,
    flow: ParamFlow,
) -> syn::Result<ParamAbi> {
    if flow != ParamFlow::Value {
        return Ok(param_abi(ty, flow));
    }
    match ty {
        BoundaryType::Callback(callback) if is_anv_callback_source(source) => {
            Ok(ParamAbi::AnvCallback(callback.clone()))
        }
        BoundaryType::Named(_) if is_anv_ref_source(source)? => Ok(ParamAbi::Value(ty.clone())),
        BoundaryType::Named(_) => Ok(ParamAbi::OwnedNamed(ty.clone())),
        BoundaryType::Option(inner) => match option_type_arg(source)? {
            Some(source_inner) => Ok(ParamAbi::Option(Box::new(param_abi_for_source(
                source_inner,
                inner,
                ParamFlow::Value,
            )?))),
            None => Ok(param_abi(ty, flow)),
        },
        BoundaryType::Result(ok, err) => match result_args(source)? {
            Some((source_ok, source_err)) => Ok(ParamAbi::Result(
                Box::new(param_abi_for_source(source_ok, ok, ParamFlow::Value)?),
                Box::new(param_abi_for_source(source_err, err, ParamFlow::Value)?),
            )),
            None => Ok(param_abi(ty, flow)),
        },
        BoundaryType::Slice(inner) => match slice_type_arg(source)? {
            Some(source_inner) => Ok(ParamAbi::Slice(Box::new(param_abi_for_source(
                source_inner,
                inner,
                ParamFlow::Value,
            )?))),
            None => Ok(param_abi(ty, flow)),
        },
        _ => Ok(param_abi(ty, flow)),
    }
}

pub fn param_abi_for_override(original: &ParamAbi, ty: &BoundaryType, flow: ParamFlow) -> ParamAbi {
    match (original, ty) {
        (ParamAbi::InitField(inner), _) => {
            ParamAbi::InitField(Box::new(param_abi_for_override(inner, ty, flow)))
        }
        _ if flow != ParamFlow::Value => param_abi(ty, flow),
        (ParamAbi::OwnedNamed(_), BoundaryType::Named(_)) => ParamAbi::OwnedNamed(ty.clone()),
        (ParamAbi::Value(_), BoundaryType::Named(_)) => ParamAbi::Value(ty.clone()),
        (ParamAbi::AnvCallback(_), BoundaryType::Callback(callback)) => {
            ParamAbi::AnvCallback(callback.clone())
        }
        (ParamAbi::Option(inner), BoundaryType::Option(ty)) => ParamAbi::Option(Box::new(
            param_abi_for_override(inner, ty, ParamFlow::Value),
        )),
        (ParamAbi::Result(ok, err), BoundaryType::Result(ok_ty, err_ty)) => ParamAbi::Result(
            Box::new(param_abi_for_override(ok, ok_ty, ParamFlow::Value)),
            Box::new(param_abi_for_override(err, err_ty, ParamFlow::Value)),
        ),
        (ParamAbi::Slice(inner), BoundaryType::Slice(ty)) => ParamAbi::Slice(Box::new(
            param_abi_for_override(inner, ty, ParamFlow::Value),
        )),
        _ => param_abi(ty, flow),
    }
}

pub fn return_abi(ty: &BoundaryType) -> ReturnAbi {
    match ty {
        BoundaryType::Void => ReturnAbi::Void,
        BoundaryType::Option(inner) => ReturnAbi::Option(Box::new(return_abi(inner))),
        BoundaryType::Result(ok, err) => {
            ReturnAbi::Result(Box::new(return_abi(ok)), Box::new(return_abi(err)))
        }
        BoundaryType::Callback(_) | BoundaryType::Slice(_) => {
            unreachable!("callbacks and slices are rejected in return position")
        }
        _ => ReturnAbi::Value(ty.clone()),
    }
}

fn return_abi_for_source(source: &Type, ty: &BoundaryType) -> syn::Result<ReturnAbi> {
    match ty {
        BoundaryType::Named(_) if is_anv_ref_source(source)? => Ok(ReturnAbi::Value(ty.clone())),
        BoundaryType::Named(_) => Ok(ReturnAbi::OwnedNamed(ty.clone())),
        BoundaryType::Option(inner) => {
            let Some(source_inner) = option_type_arg(source)? else {
                return Ok(return_abi(ty));
            };
            Ok(ReturnAbi::Option(Box::new(return_abi_for_source(
                source_inner,
                inner,
            )?)))
        }
        BoundaryType::Result(ok, err) => {
            let Some((source_ok, source_err)) = result_args(source)? else {
                return Ok(return_abi(ty));
            };
            Ok(ReturnAbi::Result(
                Box::new(return_abi_for_source(source_ok, ok)?),
                Box::new(return_abi_for_source(source_err, err)?),
            ))
        }
        _ => Ok(return_abi(ty)),
    }
}

pub fn return_abi_for_override(original: &ReturnAbi, ty: &BoundaryType) -> ReturnAbi {
    match (original, ty) {
        (ReturnAbi::OwnedNamed(_), BoundaryType::Named(_)) => ReturnAbi::OwnedNamed(ty.clone()),
        (ReturnAbi::Option(_), BoundaryType::Option(_))
        | (ReturnAbi::Result(_, _), BoundaryType::Result(_, _)) => original.clone(),
        _ => return_abi(ty),
    }
}

fn owner_name(path: &TypePath, owner: Option<OwnerReturn<'_>>) -> Option<String> {
    let owner = owner?;
    if path.qself.is_some() || path.path.segments.len() != 1 {
        return None;
    }
    let segment = path.path.segments.first().expect("checked len");
    if !matches!(segment.arguments, PathArguments::None) {
        return None;
    }
    if segment.ident == "Self" || segment.ident == *owner.rust_owner {
        Some(owner.export_name.to_string())
    } else {
        None
    }
}

fn is_anv_callback_source(source: &Type) -> bool {
    let Type::Path(path) = source else {
        return false;
    };
    path_is(path, &["AnvCallback"]) || path_is(path, &["anvyx_runtime", "AnvCallback"])
}

fn is_anv_ref_source(source: &Type) -> syn::Result<bool> {
    let Type::Path(path) = source else {
        return Ok(false);
    };
    Ok(anv_ref_type_arg(path)?.is_some())
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

pub fn has_callback_wrapper(params: &[BoundaryParam]) -> bool {
    params
        .iter()
        .any(|param| matches!(param.ty, BoundaryType::Callback(_)))
}

pub fn callback_wrapper_requires_ctxless(params: &[BoundaryParam]) -> bool {
    params.iter().any(|param| {
        matches!(
            param.abi,
            ParamAbi::ScopedLambda(_) | ParamAbi::EscapingLambda(_)
        )
    })
}

pub fn validate_callback_wrapper_precheck(
    error_target: &impl ToTokens,
    params: &[BoundaryParam],
    has_receiver: bool,
) -> syn::Result<bool> {
    let has_callback = has_callback_wrapper(params);
    if !has_callback {
        return Ok(false);
    }
    if has_receiver
        && params
            .iter()
            .any(|param| matches!(param.abi, ParamAbi::ScopedLambda(_)))
    {
        return Err(syn::Error::new_spanned(
            error_target,
            "scoped callback wrapper parameters cannot be combined with method receivers",
        ));
    }
    if params.iter().any(|param| param.flow != ParamFlow::Value) {
        return Err(syn::Error::new_spanned(
            error_target,
            "callback wrapper parameters cannot be combined with borrowed or mutable-place provider parameters",
        ));
    }
    Ok(true)
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

pub fn signature_conversion(params: &[BoundaryParam], ret: &BoundaryReturn) -> BoundaryConversion {
    merge_conversions(
        params
            .iter()
            .map(|param| param.conversion)
            .chain(std::iter::once(ret.conversion)),
    )
}

fn mut_place_macro_payload_supported(ty: &BoundaryType) -> bool {
    match ty {
        BoundaryType::Bool | BoundaryType::Int | BoundaryType::Float | BoundaryType::Char => true,
        BoundaryType::Option(inner) => mut_place_macro_payload_supported(inner),
        BoundaryType::Void
        | BoundaryType::Unit
        | BoundaryType::String
        | BoundaryType::Named(_)
        | BoundaryType::Callback(_)
        | BoundaryType::Result(_, _)
        | BoundaryType::Tuple(_)
        | BoundaryType::Array { .. }
        | BoundaryType::List(_)
        | BoundaryType::Map(_, _)
        | BoundaryType::Slice(_) => false,
    }
}

fn param_conversion(ty: &BoundaryType, flow: ParamFlow) -> BoundaryConversion {
    if flow == ParamFlow::MutBorrow {
        return BoundaryConversion::Direct;
    }
    match ty {
        BoundaryType::Callback(_) => BoundaryConversion::NeedsWrapper,
        _ if abi_supported_at(ty, AbiPosition::ParamValue) => BoundaryConversion::Direct,
        _ => BoundaryConversion::Unsupported,
    }
}

pub fn return_conversion_for_type(ty: &BoundaryType) -> BoundaryConversion {
    match ty {
        BoundaryType::Callback(_) | BoundaryType::Slice(_) => BoundaryConversion::Unsupported,
        _ if abi_supported_at(ty, AbiPosition::Return) => BoundaryConversion::Direct,
        _ => BoundaryConversion::Unsupported,
    }
}

fn abi_supported_at(ty: &BoundaryType, position: AbiPosition) -> bool {
    extern_type_expr(ty).classify_abi(position).is_ok()
}

pub(crate) fn reject_wrapper_element(
    ty: &Type,
    classified: &BoundaryType,
    flow: ParamFlow,
) -> syn::Result<()> {
    if matches!(classified, BoundaryType::Void | BoundaryType::Slice(_)) {
        return Err(syn::Error::new_spanned(
            ty,
            "unsupported wrapper element type",
        ));
    }
    if flow != ParamFlow::Value {
        return Err(syn::Error::new_spanned(
            ty,
            "wrapper element types cannot be borrowed",
        ));
    }
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum Position {
    Param,
    Return,
    WrapperElement,
}

fn reserved_named_boundary_type(ident: &Ident) -> bool {
    matches!(
        ident.to_string().as_str(),
        "str"
            | "char"
            | "f32"
            | "usize"
            | "isize"
            | "i8"
            | "i16"
            | "i32"
            | "i128"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "Option"
            | "Vec"
            | "AnvList"
            | "AnvMap"
            | "AnvSlice"
            | "Result"
            | "String"
            | "ScopedLambda"
            | "EscapingLambda"
            | "AnvCallback"
            | "MutPlace"
            | "Ctx"
            | "RuntimeError"
            | "RuntimeResult"
            | "HashMap"
            | "BTreeMap"
    )
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

fn callback_wrapper_type(path: &TypePath) -> syn::Result<Option<BoundaryCallback>> {
    let escape =
        if path_is(path, &["ScopedLambda"]) || path_is(path, &["anvyx_runtime", "ScopedLambda"]) {
            CallbackEscape::NonEscaping
        } else if path_is(path, &["EscapingLambda"])
            || path_is(path, &["anvyx_runtime", "EscapingLambda"])
            || path_is(path, &["AnvCallback"])
            || path_is(path, &["anvyx_runtime", "AnvCallback"])
        {
            CallbackEscape::Escaping
        } else {
            return Ok(None);
        };

    let segment = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(generic_args) = &segment.arguments else {
        return Err(syn::Error::new_spanned(
            segment,
            callback_wrapper_signature_error(escape),
        ));
    };
    let mut args = generic_args.args.iter();
    let (args_ty, ret_ty) = match escape {
        CallbackEscape::NonEscaping => {
            if generic_args.args.len() != 4 {
                return Err(syn::Error::new_spanned(
                    generic_args,
                    callback_wrapper_signature_error(escape),
                ));
            }
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
            (
                args.next().expect("checked len"),
                args.next().expect("checked len"),
            )
        }
        CallbackEscape::Escaping
            if path_is(path, &["AnvCallback"])
                || path_is(path, &["anvyx_runtime", "AnvCallback"]) =>
        {
            if generic_args.args.len() != 3 {
                return Err(syn::Error::new_spanned(
                    generic_args,
                    "AnvCallback parameters must be `AnvCallback<'cx, Args, Ret>`",
                ));
            }
            let cx = args.next().expect("checked len");
            if !matches!(cx, GenericArgument::Lifetime(lifetime) if lifetime.ident == "cx") {
                return Err(syn::Error::new_spanned(
                    cx,
                    "AnvCallback lifetime must be `'cx`",
                ));
            }
            (
                args.next().expect("checked len"),
                args.next().expect("checked len"),
            )
        }
        CallbackEscape::Escaping => {
            if generic_args.args.len() != 2 {
                return Err(syn::Error::new_spanned(
                    generic_args,
                    callback_wrapper_signature_error(escape),
                ));
            }
            (
                args.next().expect("checked len"),
                args.next().expect("checked len"),
            )
        }
    };
    let GenericArgument::Type(args_ty) = args_ty else {
        return Err(syn::Error::new_spanned(
            segment,
            "callback wrapper args must be a tuple type",
        ));
    };
    let params = callback_wrapper_params(args_ty, escape)?;
    let GenericArgument::Type(ret_ty) = ret_ty else {
        return Err(syn::Error::new_spanned(
            segment,
            "callback wrapper return must be a type",
        ));
    };
    let (mut ret, flow) = classify_type(ret_ty, Position::WrapperElement)?;
    if ret == BoundaryType::Unit {
        ret = BoundaryType::Void;
    }
    if flow != ParamFlow::Value || !callback_return_supported(&ret) {
        return Err(syn::Error::new_spanned(
            ret_ty,
            "unsupported callback wrapper return type",
        ));
    }
    Ok(Some(BoundaryCallback {
        params,
        ret: Box::new(ret),
        escape,
    }))
}

fn callback_wrapper_signature_error(escape: CallbackEscape) -> &'static str {
    match escape {
        CallbackEscape::NonEscaping => {
            "ScopedLambda parameters must be `ScopedLambda<'_, '_, Args, Ret>`"
        }
        CallbackEscape::Escaping => "EscapingLambda parameters must be `EscapingLambda<Args, Ret>`",
    }
}

fn callback_wrapper_params(ty: &Type, escape: CallbackEscape) -> syn::Result<Vec<BoundaryType>> {
    let label = callback_wrapper_label(escape);
    let Type::Tuple(tuple) = ty else {
        return Err(syn::Error::new_spanned(
            ty,
            format!("{label} args must be a tuple type"),
        ));
    };
    if tuple.elems.len() > CALLBACK_WRAPPER_MAX_ARITY {
        return Err(callback_arity_error(ty.span(), label));
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
                    format!("unsupported {label} parameter type"),
                ));
            }
            Ok(ty)
        })
        .collect()
}

fn callback_arity_error(span: proc_macro2::Span, label: &'static str) -> syn::Error {
    syn::Error::new(
        span,
        format!("{label} supports at most {CALLBACK_WRAPPER_MAX_ARITY} parameters"),
    )
}

fn callback_wrapper_label(escape: CallbackEscape) -> &'static str {
    match escape {
        CallbackEscape::NonEscaping => "ScopedLambda",
        CallbackEscape::Escaping => "EscapingLambda",
    }
}

fn callback_param_supported(ty: &BoundaryType) -> bool {
    extern_type_expr(ty).callback_wrapper_param_supported()
}

fn callback_return_supported(ty: &BoundaryType) -> bool {
    extern_type_expr(ty).callback_wrapper_return_supported()
}

fn extern_callback_signature(callback: &BoundaryCallback) -> ExternCallbackSignature {
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
            escape: callback.escape,
            thread: CallbackThread::SameThread,
        },
    }
}

fn named_type_expr(name: &str) -> ExternTypeExpr {
    ExternTypeExpr::Named {
        module: None,
        name: name.to_string(),
        args: vec![],
    }
}

fn extern_type_expr(ty: &BoundaryType) -> ExternTypeExpr {
    match ty {
        BoundaryType::Void => ExternTypeExpr::Void,
        BoundaryType::Unit => ExternTypeExpr::Unit,
        BoundaryType::Bool => ExternTypeExpr::Bool,
        BoundaryType::Int => ExternTypeExpr::Int,
        BoundaryType::Float => ExternTypeExpr::Float,
        BoundaryType::String => ExternTypeExpr::String,
        BoundaryType::Char => ExternTypeExpr::Char,
        BoundaryType::Named(name) => named_type_expr(name),
        BoundaryType::Callback(callback) => {
            ExternTypeExpr::Callback(extern_callback_signature(callback))
        }
        BoundaryType::Option(inner) => ExternTypeExpr::Option(Box::new(extern_type_expr(inner))),
        BoundaryType::Result(ok, err) => ExternTypeExpr::Result(
            Box::new(extern_type_expr(ok)),
            Box::new(extern_type_expr(err)),
        ),
        BoundaryType::Tuple(fields) => {
            ExternTypeExpr::Tuple(fields.iter().map(extern_type_expr).collect())
        }
        BoundaryType::Array { elem, len } => ExternTypeExpr::Array {
            elem: Box::new(extern_type_expr(elem)),
            len: *len,
        },
        BoundaryType::List(inner) => ExternTypeExpr::List(Box::new(extern_type_expr(inner))),
        BoundaryType::Map(key, value) => ExternTypeExpr::Map(
            Box::new(extern_type_expr(key)),
            Box::new(extern_type_expr(value)),
        ),
        BoundaryType::Slice(inner) => ExternTypeExpr::Slice(Box::new(extern_type_expr(inner))),
    }
}

pub fn parse_descriptor_type_expr(
    input: ParseStream,
    allow_callback: bool,
) -> syn::Result<BoundaryType> {
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
        let inner = parse_descriptor_type_expr(&content, false)?;
        if !content.is_empty() {
            return Err(syn::Error::new(
                content.span(),
                "unsupported descriptor type",
            ));
        }
        BoundaryType::List(Box::new(inner))
    } else if input.peek(syn::token::Paren) {
        let content;
        syn::parenthesized!(content in input);
        if !content.is_empty() {
            return Err(syn::Error::new(
                content.span(),
                "unsupported descriptor type",
            ));
        }
        BoundaryType::Unit
    } else {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "void" => BoundaryType::Void,
            "bool" => BoundaryType::Bool,
            "int" => BoundaryType::Int,
            "float" => BoundaryType::Float,
            "string" => BoundaryType::String,
            "char" => BoundaryType::Char,
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
        if matches!(ty, BoundaryType::Callback(_)) {
            return Err(syn::Error::new(
                input.span(),
                "callbacks cannot be optional",
            ));
        }
        ty = BoundaryType::Option(Box::new(ty));
    }
    Ok(ty)
}

fn parse_descriptor_callback(input: ParseStream) -> syn::Result<BoundaryType> {
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
    if params.len() > CALLBACK_WRAPPER_MAX_ARITY {
        return Err(callback_arity_error(input.span(), "callback"));
    }
    let ret = if input.peek(Token![->]) {
        input.parse::<Token![->]>()?;
        parse_descriptor_type_expr(input, false)?
    } else {
        BoundaryType::Void
    };
    let ret = if ret == BoundaryType::Unit {
        BoundaryType::Void
    } else {
        ret
    };
    if !callback_return_supported(&ret) {
        return Err(syn::Error::new(
            input.span(),
            "unsupported callback return type",
        ));
    }
    Ok(BoundaryType::Callback(BoundaryCallback {
        params,
        ret: Box::new(ret),
        escape: CallbackEscape::NonEscaping,
    }))
}

fn anv_ref_type_arg_for_owner(
    path: &TypePath,
    owner: Option<OwnerReturn<'_>>,
) -> syn::Result<Option<String>> {
    let Some(name) = anv_ref_type_arg(path)? else {
        return Ok(None);
    };
    if let Some(owner) = owner
        && (name == "Self" || owner.rust_owner == name.as_str())
    {
        return Ok(Some(owner.export_name.to_string()));
    }
    Ok(Some(name))
}

fn anv_ref_type_arg(path: &TypePath) -> syn::Result<Option<String>> {
    if !path_is(path, &["AnvRef"]) && !path_is(path, &["anvyx_runtime", "AnvRef"]) {
        return Ok(None);
    }
    let segment = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        return Err(syn::Error::new_spanned(
            segment,
            "AnvRef requires a `'cx` lifetime and a type argument",
        ));
    };
    if args.args.len() != 2 {
        return Err(syn::Error::new_spanned(
            args,
            "AnvRef parameters must be `AnvRef<'cx, T>`",
        ));
    }
    let mut args = args.args.iter();
    let cx = args.next().expect("checked len");
    if !matches!(cx, GenericArgument::Lifetime(lifetime) if lifetime.ident == "cx") {
        return Err(syn::Error::new_spanned(cx, "AnvRef lifetime must be `'cx`"));
    }
    let GenericArgument::Type(Type::Path(ty)) = args.next().expect("checked len") else {
        return Err(syn::Error::new_spanned(
            segment,
            "AnvRef requires a concrete type argument",
        ));
    };
    if ty.qself.is_none() && ty.path.segments.len() == 1 {
        let segment = &ty.path.segments[0];
        if matches!(segment.arguments, PathArguments::None)
            || lifetime_only_args(&segment.arguments)
        {
            return Ok(Some(segment.ident.to_string()));
        }
    }
    Err(syn::Error::new_spanned(
        ty,
        "AnvRef resource type must be a concrete non-generic type name",
    ))
}

fn lifetime_only_args(args: &PathArguments) -> bool {
    let PathArguments::AngleBracketed(args) = args else {
        return false;
    };
    !args.args.is_empty()
        && args
            .args
            .iter()
            .all(|arg| matches!(arg, GenericArgument::Lifetime(lifetime) if lifetime.ident == "cx"))
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

fn init_field_type_arg(ty: &Type) -> syn::Result<Option<&Type>> {
    let Type::Path(path) = ty else {
        return Ok(None);
    };
    one_type_arg(
        path,
        &[&["AnvInitField"], &["anvyx_runtime", "AnvInitField"]],
    )
}

fn map_type_args<'a>(
    path: &'a TypePath,
    names: &[&[&str]],
) -> syn::Result<Option<(&'a Type, &'a Type)>> {
    if !names.iter().any(|name| path_is(path, name)) {
        return Ok(None);
    }
    let name = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(args) = &name.arguments else {
        return Err(syn::Error::new_spanned(
            name,
            "map wrapper type requires two type arguments",
        ));
    };
    if args.args.len() != 2 {
        return Err(syn::Error::new_spanned(
            args,
            "map wrapper type requires two type arguments",
        ));
    }
    let mut args = args.args.iter();
    let GenericArgument::Type(key) = args.next().expect("checked len") else {
        return Err(syn::Error::new_spanned(name, "map key must be a type"));
    };
    let GenericArgument::Type(value) = args.next().expect("checked len") else {
        return Err(syn::Error::new_spanned(name, "map value must be a type"));
    };
    Ok(Some((key, value)))
}

fn option_type_arg(ty: &Type) -> syn::Result<Option<&Type>> {
    let Type::Path(path) = ty else {
        return Ok(None);
    };
    one_type_arg(path, &[&["Option"], &["std", "option", "Option"]])
}

fn slice_type_arg(ty: &Type) -> syn::Result<Option<&Type>> {
    let Type::Path(path) = ty else {
        return Ok(None);
    };
    let Some(args) =
        runtime_cx_type_args(path, &[&["AnvSlice"], &["anvyx_runtime", "AnvSlice"]], 1)?
    else {
        return Ok(None);
    };
    Ok(Some(args[0]))
}

pub(crate) fn runtime_result_type_arg(ty: &Type) -> syn::Result<Option<&Type>> {
    let Type::Path(path) = ty else {
        return Ok(None);
    };
    runtime_result_type_arg_from_path(path)
}

fn runtime_result_type_arg_from_path(path: &TypePath) -> syn::Result<Option<&Type>> {
    one_type_arg(
        path,
        &[&["RuntimeResult"], &["anvyx_runtime", "RuntimeResult"]],
    )
}

pub(crate) fn result_args(ty: &Type) -> syn::Result<Option<(&Type, &Type)>> {
    let Type::Path(path) = ty else {
        return Ok(None);
    };
    result_type_args(path)
}

fn result_type_args(path: &TypePath) -> syn::Result<Option<(&Type, &Type)>> {
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

fn runtime_cx_type_args<'a>(
    path: &'a TypePath,
    names: &[&[&str]],
    type_count: usize,
) -> syn::Result<Option<Vec<&'a Type>>> {
    if !names.iter().any(|name| path_is(path, name)) {
        return Ok(None);
    }
    let segment = path.path.segments.last().expect("matched path has segment");
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        return Err(syn::Error::new_spanned(
            segment,
            "runtime ABI type requires a `'cx` lifetime and type argument(s)",
        ));
    };
    if args.args.len() != type_count + 1 {
        return Err(syn::Error::new_spanned(
            args,
            format!("runtime ABI type requires a `'cx` lifetime and {type_count} type argument(s)"),
        ));
    }
    let mut args = args.args.iter();
    let cx = args.next().expect("checked len");
    if !matches!(cx, GenericArgument::Lifetime(lifetime) if lifetime.ident == "cx") {
        return Err(syn::Error::new_spanned(
            cx,
            "runtime ABI lifetime must be `'cx`",
        ));
    }
    args.map(|arg| match arg {
        GenericArgument::Type(ty) => Ok(ty),
        _ => Err(syn::Error::new_spanned(
            arg,
            "runtime ABI argument must be a type",
        )),
    })
    .collect::<syn::Result<Vec<_>>>()
    .map(Some)
}

pub fn parse_type_expr(text: &str) -> syn::Result<BoundaryType> {
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
        return Ok(BoundaryType::Option(Box::new(parse_wrapper_override(
            inner,
        )?)));
    }
    if let Some(inner) = text.strip_suffix('?') {
        return Ok(BoundaryType::Option(Box::new(parse_wrapper_override(
            inner,
        )?)));
    }
    if let Some(inner) = text
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
    {
        return Ok(BoundaryType::List(Box::new(parse_wrapper_override(inner)?)));
    }
    if let Ok(ident) = syn::parse_str::<Ident>(text)
        && !reserved_named_boundary_type(&ident)
    {
        return Ok(BoundaryType::Named(ident.to_string()));
    }
    Err(syn::Error::new(
        proc_macro2::Span::call_site(),
        format!("unsupported extern type override `{text}`"),
    ))
}

fn parse_wrapper_override(text: &str) -> syn::Result<BoundaryType> {
    let ty = parse_type_expr(text)?;
    if matches!(ty, BoundaryType::Callback(_)) {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "callbacks are only supported in top-level parameter position",
        ));
    }
    Ok(ty)
}

fn parse_descriptor_type_text(text: &str, allow_callback: bool) -> syn::Result<BoundaryType> {
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

pub(crate) fn is_runtime_error(ty: &Type) -> bool {
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

    fn first_param(tokens: TokenStream) -> syn::Result<BoundaryParam> {
        first_param_with_ctx(tokens, false)
    }

    fn first_param_with_ctx(tokens: TokenStream, has_ctx: bool) -> syn::Result<BoundaryParam> {
        let func: ItemFn = syn::parse2(quote! { fn f(#tokens) {} }).unwrap();
        std::hint::black_box(tokens);
        let Some(syn::FnArg::Typed(param)) = func.sig.inputs.first() else {
            unreachable!();
        };
        classify_param(param, has_ctx)
    }

    fn ret(output: ReturnType) -> syn::Result<BoundaryReturn> {
        let ret = classify_return(&output);
        std::hint::black_box(output);
        ret
    }

    #[test]
    fn rejects_mut_place_without_ctx_or_runtime_payload() {
        assert!(first_param(quote! { value: MutPlace<'_, 'cx, i64> }).is_err());
        assert!(
            first_param_with_ctx(quote! { value: MutPlace<'_, 'cx, String> }, true)
                .unwrap_err()
                .to_string()
                .contains("String is not a provider boundary carrier")
        );
        assert!(
            first_param_with_ctx(quote! { value: MutPlace<'_, 'cx, Vec<i64>> }, true)
                .unwrap_err()
                .to_string()
                .contains("Vec<T> is not a provider boundary carrier")
        );
        assert_eq!(
            first_param_with_ctx(quote! { value: MutPlace<'_, 'cx, Option<i64>> }, true)
                .unwrap()
                .ty,
            BoundaryType::Option(Box::new(BoundaryType::Int))
        );
    }

    #[test]
    fn maps_wrappers() {
        assert!(
            first_param(quote! { x: Vec<AnvString> })
                .unwrap_err()
                .to_string()
                .contains("Vec<T> is not a provider boundary carrier")
        );
        assert!(
            ret(parse_quote! { -> Vec<i64> })
                .unwrap_err()
                .to_string()
                .contains("Vec<T> is not a provider boundary carrier")
        );
    }

    #[test]
    fn rejects_invalid_anv_ref_resource_types() {
        assert!(
            first_param(quote! { x: anvyx_runtime::AnvRef<'_, Counter> })
                .unwrap_err()
                .to_string()
                .contains("'cx")
        );
        assert!(
            first_param(quote! { x: anvyx_runtime::AnvRef<'cx, Box<Counter>> })
                .unwrap_err()
                .to_string()
                .contains("non-generic")
        );
    }

    #[test]
    fn rejects_string_callback_wrapper_elements() {
        assert!(
            first_param(quote! { f: ScopedLambda<'_, '_, (String,), ()> })
                .unwrap_err()
                .to_string()
                .contains("String is not a provider boundary carrier")
        );
        assert!(
            first_param(quote! { f: ScopedLambda<'_, '_, (i64,), String> })
                .unwrap_err()
                .to_string()
                .contains("String is not a provider boundary carrier")
        );
        assert!(
            first_param(quote! { f: EscapingLambda<(String,), ()> })
                .unwrap_err()
                .to_string()
                .contains("String is not a provider boundary carrier")
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
            BoundaryType::Callback(BoundaryCallback {
                params: vec![BoundaryType::Int],
                ret: Box::new(BoundaryType::Void),
                escape: CallbackEscape::NonEscaping,
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
    fn classifies_supported_and_rejects_unsupported_types() {
        assert!(first_param(quote! { x: f32 }).is_err());
        assert!(first_param(quote! { x: usize }).is_err());
        assert!(first_param(quote! { x: i128 }).is_err());
        assert!(first_param(quote! { x: u128 }).is_err());
        assert_eq!(
            first_param(quote! { x: char }).unwrap().ty,
            BoundaryType::Char
        );
        assert_eq!(
            ret(parse_quote! { -> char }).unwrap().ty,
            BoundaryType::Char
        );
        assert_eq!(
            first_param(quote! { f: ScopedLambda<'_, '_, (char,), char> })
                .unwrap()
                .ty,
            BoundaryType::Callback(BoundaryCallback {
                params: vec![BoundaryType::Char],
                ret: Box::new(BoundaryType::Char),
                escape: CallbackEscape::NonEscaping,
            })
        );
        assert_eq!(
            parse_type_expr("fn(char) -> char").unwrap(),
            BoundaryType::Callback(BoundaryCallback {
                params: vec![BoundaryType::Char],
                ret: Box::new(BoundaryType::Char),
                escape: CallbackEscape::NonEscaping,
            })
        );
        assert!(first_param(quote! { x: str }).is_err());
        assert!(first_param(quote! { x: &i64 }).is_err());
        assert_eq!(
            first_param(quote! { x: () }).unwrap().ty,
            BoundaryType::Unit
        );
        assert_eq!(
            first_param(quote! { x: Option<()> }).unwrap().ty,
            BoundaryType::Option(Box::new(BoundaryType::Unit))
        );
        assert!(first_param(quote! { x: Option<&str> }).is_err());
        assert!(first_param(quote! { x: my_crate::Option<i64> }).is_err());
        assert!(first_param(quote! { x: my_crate::WindowConfig }).is_err());
        assert!(first_param(quote! { x: WindowConfig<i64> }).is_err());
        assert!(first_param(quote! { x: Vec<WindowConfig> }).is_err());
        assert!(first_param(quote! { x: Option<WindowConfig> }).is_ok());
        assert!(ret(parse_quote! { -> &str }).is_err());
        assert!(ret(parse_quote! { -> Result<i64, RuntimeError> }).is_err());
        assert!(ret(parse_quote! { -> foo::Result<i64, RuntimeError> }).is_err());
    }
}
