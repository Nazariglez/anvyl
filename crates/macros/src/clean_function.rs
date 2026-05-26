use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    ItemFn, LitStr, Token,
    parse::{Parse, ParseStream},
};

use crate::clean_type_map::{
    classify_param, classify_return, conversion_tokens, flow_tokens, merge_conversions,
    param_abi_tokens, parse_type_expr, return_abi_tokens, type_expr_tokens,
    validate_callable_signature,
};

struct FunctionArgs {
    name: Option<String>,
    ret: Option<String>,
    params: HashMap<String, String>,
}

impl Parse for FunctionArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut ret = None;
        let mut params = None;

        while !input.is_empty() {
            let key: syn::Ident = input.parse()?;
            match key.to_string().as_str() {
                "name" => {
                    let _eq: Token![=] = input.parse()?;
                    set_once(&mut name, &key, input.parse::<LitStr>()?.value())?;
                }
                "ret" => {
                    let _eq: Token![=] = input.parse()?;
                    set_once(&mut ret, &key, input.parse::<LitStr>()?.value())?;
                }
                "params" => set_once(&mut params, &key, parse_param_overrides(input)?)?,
                _ => {
                    return Err(syn::Error::new(
                        key.span(),
                        "expected `name`, `ret`, or `params`",
                    ));
                }
            }
            if !input.is_empty() {
                let _comma: Token![,] = input.parse()?;
            }
        }

        Ok(Self {
            name,
            ret,
            params: params.unwrap_or_default(),
        })
    }
}

fn parse_param_overrides(input: ParseStream) -> syn::Result<HashMap<String, String>> {
    let content;
    syn::parenthesized!(content in input);
    let mut params = HashMap::new();
    while !content.is_empty() {
        let param: syn::Ident = content.parse()?;
        let _eq: Token![=] = content.parse()?;
        let lit: LitStr = content.parse()?;
        if params.insert(param.to_string(), lit.value()).is_some() {
            return Err(syn::Error::new_spanned(
                param,
                "duplicate parameter override",
            ));
        }
        if !content.is_empty() {
            let _comma: Token![,] = content.parse()?;
        }
    }
    Ok(params)
}

fn set_once<T>(slot: &mut Option<T>, key: &syn::Ident, value: T) -> syn::Result<()> {
    if slot.is_some() {
        return Err(syn::Error::new_spanned(
            key,
            format!("duplicate #[function] key `{key}`"),
        ));
    }
    *slot = Some(value);
    Ok(())
}

pub fn expand(attr: TokenStream, item: TokenStream) -> TokenStream {
    let fallback = item.clone();
    crate::util::expand_or_error(&fallback, expand_inner(attr, item))
}

fn expand_inner(attr: TokenStream, item: TokenStream) -> syn::Result<TokenStream> {
    let args: FunctionArgs = syn::parse2(attr)?;
    let func: ItemFn = syn::parse2(item)?;
    validate_callable_signature(&func.sig, "#[function]", "function")?;

    let ident = &func.sig.ident;
    let export_name = args.name.clone().unwrap_or_else(|| ident.to_string());
    let companion = crate::naming::fn_companion_ident(ident);
    let native_mod = crate::naming::native_export_module_ident(ident);
    let wrapper = format_ident!("{}", export_name);

    let params = func
        .sig
        .inputs
        .iter()
        .map(classify_param)
        .collect::<syn::Result<Vec<_>>>()?;
    validate_overrides(&args, &params)?;
    let ret = classify_return(&func.sig.output)?;
    let ret_ty = match args.ret.as_deref() {
        Some(override_ty) => type_expr_tokens(&parse_type_expr(override_ty)?),
        None => type_expr_tokens(&ret.ty),
    };

    let descriptor_params = params
        .iter()
        .map(|param| {
            let name = &param.name;
            let ty = match args.params.get(name) {
                Some(override_ty) => parse_type_expr(override_ty).map(|ty| type_expr_tokens(&ty)),
                None => Ok(type_expr_tokens(&param.ty)),
            }?;
            let flow = flow_tokens(param.flow);
            Ok::<_, syn::Error>(quote! {
                anvyx_runtime::ExternParam {
                    name: Some(#name.to_string()),
                    ty: #ty,
                    flow: #flow,
                    escape: anvyx_runtime::CallbackEscape::NonEscaping,
                }
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;
    let param_abis = params.iter().map(|param| param_abi_tokens(&param.abi));
    let ret_abi = return_abi_tokens(&ret.abi);
    let support = conversion_tokens(merge_conversions(
        params
            .iter()
            .map(|param| param.conversion)
            .chain(std::iter::once(ret.conversion)),
    ));
    let fallible = ret.fallible;
    let native_inputs = func.sig.inputs.iter();
    let native_output = &func.sig.output;
    let native_args = func.sig.inputs.iter().map(|arg| match arg {
        syn::FnArg::Typed(pat_ty) => &pat_ty.pat,
        syn::FnArg::Receiver(_) => unreachable!("validated by classifier"),
    });
    let doc = crate::codegen::extract_doc(&func.attrs)
        .map_or_else(|| quote! { None }, |doc| quote! { Some(#doc.to_string()) });
    Ok(quote! {
        #func

        #[doc(hidden)]
        pub fn #companion() -> anvyx_runtime::FunctionExport {
            anvyx_runtime::FunctionExport {
                descriptor: anvyx_runtime::ExternFunctionDescriptor {
                    name: #export_name.to_string(),
                    doc: #doc,
                    signature: anvyx_runtime::ExternSignature {
                        params: vec![#(#descriptor_params),*],
                        ret: #ret_ty,
                    },
                    effects: anvyx_runtime::ExternEffects { fallible: #fallible },
                },
                rust: anvyx_runtime::RustLocalBinding {
                    symbol: stringify!(#wrapper).to_string(),
                    abi: anvyx_runtime::RustExternAbi {
                        params: vec![#(#param_abis),*],
                        ret: #ret_abi,
                        needs_context: false,
                        fallible: #fallible,
                        support: #support,
                    },
                },
            }
        }

        #[doc(hidden)]
        pub mod #native_mod {
            use super::*;

            pub fn #wrapper(#(#native_inputs),*) #native_output {
                super::#ident(#(#native_args),*)
            }
        }
    })
}

fn validate_overrides(
    args: &FunctionArgs,
    params: &[crate::clean_type_map::CleanParam],
) -> syn::Result<()> {
    for key in args.params.keys() {
        if !params.iter().any(|param| param.name == *key) {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                format!("unknown #[function(params)] parameter `{key}`"),
            ));
        }
    }
    Ok(())
}
