use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    ItemFn, LitStr, Token,
    parse::{Parse, ParseStream},
    spanned::Spanned,
};

use crate::clean_type_map::{
    classify_param, classify_return, conversion_tokens, flow_tokens, has_scoped_lambda,
    merge_conversions, param_abi_tokens, return_abi_tokens, scoped_lambda_has_visible_borrow,
    type_tokens_with_override, validate_callable_signature, validate_ctx_param,
    validate_mut_place_ctx,
};

struct FunctionArgs {
    name: Option<String>,
    ret: Option<String>,
    params: HashMap<String, String>,
    ctx: bool,
}

impl Parse for FunctionArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut name = None;
        let mut ret = None;
        let mut params = None;
        let mut ctx = false;

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
                "ctx" => {
                    if ctx {
                        return Err(syn::Error::new_spanned(
                            key,
                            "duplicate #[function] key `ctx`",
                        ));
                    }
                    ctx = true;
                }
                _ => {
                    return Err(syn::Error::new(
                        key.span(),
                        "expected `name`, `ret`, `params`, or `ctx`",
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
            ctx,
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
    validate_callable_signature(&func.sig, "#[function]", "function", args.ctx)?;

    let ident = &func.sig.ident;
    let export_name = args.name.clone().unwrap_or_else(|| ident.to_string());
    let companion = crate::naming::fn_companion_ident(ident);
    let native_mod = crate::naming::native_export_module_ident(ident);
    let wrapper = format_ident!("{}", export_name);

    let (ctx_input, visible_inputs) = visible_function_inputs(&func.sig.inputs, args.ctx)?;
    let params = visible_inputs
        .iter()
        .copied()
        .map(|param| classify_param(param, args.ctx))
        .collect::<syn::Result<Vec<_>>>()?;
    let scoped_lambda = has_scoped_lambda(&params);
    if let Some(ctx) = ctx_input {
        validate_mut_place_ctx(&func.sig, ctx, &params, "#[function(ctx)]")?;
        if scoped_lambda {
            return Err(syn::Error::new_spanned(
                ctx,
                "#[function(ctx)] cannot be combined with ScopedLambda parameters",
            ));
        }
    }
    if scoped_lambda_has_visible_borrow(&params) {
        return Err(syn::Error::new_spanned(
            &func.sig,
            "ScopedLambda parameters cannot be combined with borrowed or mutable-place provider parameters",
        ));
    }
    validate_overrides(&args, &params)?;
    let ret = classify_return(&func.sig.output)?;
    let ret_ty = type_tokens_with_override(
        &ret.ty,
        args.ret.as_deref(),
        proc_macro2::Span::call_site(),
        "#[function(ret)] override does not match Rust return ABI",
    )?;

    let descriptor_params = params
        .iter()
        .map(|param| {
            let name = &param.name;
            let ty = type_tokens_with_override(
                &param.ty,
                args.params.get(name).map(String::as_str),
                proc_macro2::Span::call_site(),
                format!("#[function(params)] override for `{name}` does not match Rust ABI"),
            )?;
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
    let wrapper_ctx = if scoped_lambda {
        quote! { anvyx_runtime::RustWrapperCtx::None }
    } else {
        quote! { anvyx_runtime::RustWrapperCtx::HiddenRuntime }
    };
    let fallible = ret.fallible;
    let native_ctx = if args.ctx {
        quote! { ctx }
    } else {
        quote! { _ctx }
    };
    let native_inputs = visible_inputs.iter();
    let wrapper_inputs = if scoped_lambda {
        quote! { #(#native_inputs),* }
    } else {
        quote! { #native_ctx: &mut anvyx_runtime::Ctx<'cx, '_>, #(#native_inputs),* }
    };
    let native_output = &func.sig.output;
    let visible_args = params
        .iter()
        .map(|param| format_ident!("{}", param.name))
        .collect::<Vec<_>>();
    let native_args = if args.ctx {
        quote! { ctx, #(#visible_args),* }
    } else {
        quote! { #(#visible_args),* }
    };
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
                        fallible: #fallible,
                        support: #support,
                        ctx: #wrapper_ctx,
                    },
                },
            }
        }

        #[doc(hidden)]
        pub mod #native_mod {
            use super::*;

            pub fn #wrapper<'cx>(#wrapper_inputs) #native_output {
                super::#ident(#native_args)
            }
        }
    })
}

fn visible_function_inputs(
    inputs: &syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>,
    needs_ctx: bool,
) -> syn::Result<(Option<&syn::PatType>, Vec<&syn::PatType>)> {
    let typed = inputs
        .iter()
        .map(|arg| match arg {
            syn::FnArg::Typed(pat) => Ok(pat),
            syn::FnArg::Receiver(_) => Err(syn::Error::new_spanned(
                arg,
                "#[function] does not support self receivers",
            )),
        })
        .collect::<syn::Result<Vec<_>>>()?;
    if !needs_ctx {
        return Ok((None, typed));
    }
    let Some((ctx, rest)) = typed.split_first() else {
        return Err(syn::Error::new(
            inputs.span(),
            "#[function(ctx)] requires a first `ctx` parameter",
        ));
    };
    validate_ctx_param(ctx, "#[function(ctx)]")?;
    Ok((Some(ctx), rest.to_vec()))
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
