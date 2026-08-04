use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, LitStr, Token,
    parse::{Parse, ParseStream},
};

use crate::boundary::{BoundaryType, externs_type_expr_tokens, parse_descriptor_type_expr};

struct DescriptorArgs {
    provider: LitStr,
    module: LitStr,
    functions: Vec<Function>,
}

struct Function {
    name: Ident,
    params: Vec<Param>,
    ret: BoundaryType,
}

struct Param {
    name: Ident,
    ty: BoundaryType,
}

impl Parse for DescriptorArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let provider_key: Ident = input.parse()?;
        if provider_key != "provider" {
            return Err(syn::Error::new_spanned(provider_key, "expected `provider`"));
        }
        input.parse::<Token![=]>()?;
        let provider = input.parse()?;
        input.parse::<Token![,]>()?;

        let module_key: Ident = input.parse()?;
        if module_key != "module" {
            return Err(syn::Error::new_spanned(module_key, "expected `module`"));
        }
        input.parse::<Token![=]>()?;
        let module = input.parse()?;
        input.parse::<Token![,]>()?;

        let mut functions = vec![];
        while !input.is_empty() {
            functions.push(input.parse()?);
        }

        Ok(Self {
            provider,
            module,
            functions,
        })
    }
}

impl Parse for Function {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![fn]>()?;
        let name = input.parse()?;
        let content;
        syn::parenthesized!(content in input);
        let mut params = vec![];
        while !content.is_empty() {
            params.push(content.parse()?);
            if content.is_empty() {
                break;
            }
            content.parse::<Token![,]>()?;
        }
        input.parse::<Token![->]>()?;
        let ret = parse_descriptor_type_expr(input, false)?;
        input.parse::<Token![;]>()?;
        let _ = input.parse::<Token![,]>();
        Ok(Self { name, params, ret })
    }
}

impl Parse for Param {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty = parse_descriptor_type_expr(input, true)?;
        Ok(Self { name, ty })
    }
}

pub fn expand(input: TokenStream) -> TokenStream {
    match do_expand(input) {
        Ok(ts) => ts,
        Err(e) => e.to_compile_error(),
    }
}

fn do_expand(input: TokenStream) -> syn::Result<TokenStream> {
    let args: DescriptorArgs = syn::parse2(input)?;
    let provider = args.provider.value();
    let module = args.module.value();
    let module_segments = module
        .split('.')
        .map(|segment| quote! { #segment.to_string() });
    let functions = args.functions.iter().map(function_tokens);

    Ok(quote! {
        anvyx_externs::ProviderDescriptor {
            provider: anvyx_externs::ProviderId {
                name: #provider.to_string(),
            },
            modules: vec![anvyx_externs::ExternModuleDescriptor {
                path: anvyx_externs::ModulePath {
                    segments: vec![#(#module_segments),*],
                },
                types: vec![],
                functions: vec![#(#functions),*],
            }],
        }
    })
}

fn function_tokens(function: &Function) -> TokenStream {
    let name = function.name.to_string();
    let params = function.params.iter().map(|param| {
        let name = param.name.to_string();
        let ty = externs_type_expr_tokens(&param.ty);
        quote! {
            anvyx_externs::ExternParam {
                name: Some(#name.to_string()),
                ty: #ty,
                flow: anvyx_externs::ParamFlow::Value,
                escape: anvyx_externs::CallbackEscape::NonEscaping,
            }
        }
    });
    let ret = externs_type_expr_tokens(&function.ret);
    quote! {
        anvyx_externs::ExternFunctionDescriptor {
            name: #name.to_string(),
            doc: None,
            signature: anvyx_externs::ExternSignature {
                params: vec![#(#params),*],
                ret: #ret,
            },
            effects: anvyx_externs::ExternEffects::default(),
        }
    }
}
