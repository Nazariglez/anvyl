use proc_macro2::TokenStream;
use quote::quote;
use syn::{FnArg, GenericArgument, PathArguments, ReturnType, Signature, Type, TypePath};

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
    Option(Box<CleanType>),
    List(Box<CleanType>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CleanFlow {
    Value,
    Borrow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CleanParamAbi {
    Value(CleanType),
    Borrow(CleanType),
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
) -> syn::Result<()> {
    if !sig.generics.params.is_empty() || sig.generics.where_clause.is_some() {
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

pub fn classify_param(arg: &FnArg) -> syn::Result<CleanParam> {
    let FnArg::Typed(pat_ty) = arg else {
        return Err(syn::Error::new_spanned(
            arg,
            "#[function] does not support self receivers",
        ));
    };
    let syn::Pat::Ident(ident) = pat_ty.pat.as_ref() else {
        return Err(syn::Error::new_spanned(
            &pat_ty.pat,
            "#[function] parameters must be identifiers",
        ));
    };
    let (ty, flow) = classify_type(&pat_ty.ty, Position::Param)?;
    if ty == CleanType::Void {
        return Err(syn::Error::new_spanned(
            &pat_ty.ty,
            "#[function] parameters cannot be void",
        ));
    }
    let abi = param_abi(&ty, flow);
    let conversion = param_conversion_for_type(&ty);
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
                    "#[function] does not support mutable references yet",
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
        Type::Path(path) => classify_path(path).map(|ty| (ty, CleanFlow::Value)),
        _ => Err(syn::Error::new_spanned(
            ty,
            "unsupported #[function] boundary type",
        )),
    }
}

fn classify_path(path: &TypePath) -> syn::Result<CleanType> {
    if path.qself.is_some() {
        return Err(syn::Error::new_spanned(
            path,
            "qualified boundary types are not supported",
        ));
    }
    if path_is(path, &["bool"]) {
        return Ok(CleanType::Bool);
    }
    if path_is(path, &["i64"]) {
        return Ok(CleanType::Int);
    }
    if path_is(path, &["f64"]) {
        return Ok(CleanType::Float);
    }
    if path_is(path, &["String"]) || path_is(path, &["std", "string", "String"]) {
        return Ok(CleanType::String);
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
        return Ok(CleanType::Option(Box::new(ty)));
    }
    if let Some(inner) = one_type_arg(path, &[&["Vec"], &["std", "vec", "Vec"]])? {
        let (ty, flow) = classify_type(inner, Position::WrapperElement)?;
        reject_wrapper_element(inner, &ty, flow)?;
        return Ok(CleanType::List(Box::new(ty)));
    }
    Err(syn::Error::new_spanned(
        path,
        "unsupported #[function] boundary type",
    ))
}

pub fn type_expr_tokens(ty: &CleanType) -> TokenStream {
    match ty {
        CleanType::Void => quote! { anvyx_runtime::ExternTypeExpr::Void },
        CleanType::Bool => quote! { anvyx_runtime::ExternTypeExpr::Bool },
        CleanType::Int => quote! { anvyx_runtime::ExternTypeExpr::Int },
        CleanType::Float => quote! { anvyx_runtime::ExternTypeExpr::Float },
        CleanType::String => quote! { anvyx_runtime::ExternTypeExpr::String },
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

pub fn flow_tokens(flow: CleanFlow) -> TokenStream {
    match flow {
        CleanFlow::Value => quote! { anvyx_runtime::ParamFlow::Value },
        CleanFlow::Borrow => quote! { anvyx_runtime::ParamFlow::Borrow },
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
    match ty {
        CleanType::Option(inner) => {
            CleanParamAbi::Option(Box::new(param_abi(inner, CleanFlow::Value)))
        }
        CleanType::List(inner) => CleanParamAbi::List(Box::new(param_abi(inner, CleanFlow::Value))),
        _ => match flow {
            CleanFlow::Value => CleanParamAbi::Value(ty.clone()),
            CleanFlow::Borrow => CleanParamAbi::Borrow(ty.clone()),
        },
    }
}

fn return_abi(ty: &CleanType) -> CleanReturnAbi {
    match ty {
        CleanType::Void => CleanReturnAbi::Void,
        CleanType::Option(inner) => CleanReturnAbi::Option(Box::new(return_abi(inner))),
        CleanType::List(inner) => CleanReturnAbi::List(Box::new(return_abi(inner))),
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

fn param_conversion_for_type(ty: &CleanType) -> BoundaryConversion {
    match ty {
        CleanType::Option(_) | CleanType::List(_) => BoundaryConversion::Unsupported,
        _ => BoundaryConversion::Direct,
    }
}

fn return_conversion_for_type(ty: &CleanType) -> BoundaryConversion {
    match ty {
        CleanType::Option(inner) if return_option_inner_supported(inner) => {
            BoundaryConversion::Direct
        }
        CleanType::Option(_) | CleanType::List(_) => BoundaryConversion::Unsupported,
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
    match text {
        "void" | "()" => return Ok(CleanType::Void),
        "bool" => return Ok(CleanType::Bool),
        "int" => return Ok(CleanType::Int),
        "float" => return Ok(CleanType::Float),
        "string" => return Ok(CleanType::String),
        _ => {}
    }
    if let Some(inner) = text
        .strip_prefix("Option<")
        .and_then(|rest| rest.strip_suffix('>'))
    {
        return Ok(CleanType::Option(Box::new(parse_type_expr(inner)?)));
    }
    if let Some(inner) = text.strip_suffix('?') {
        return Ok(CleanType::Option(Box::new(parse_type_expr(inner)?)));
    }
    if let Some(inner) = text
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
    {
        return Ok(CleanType::List(Box::new(parse_type_expr(inner)?)));
    }
    Err(syn::Error::new(
        proc_macro2::Span::call_site(),
        format!("unsupported extern type override `{text}`"),
    ))
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
        let func: ItemFn = syn::parse2(quote! { fn f(#tokens) {} }).unwrap();
        classify_param(func.sig.inputs.first().unwrap())
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
