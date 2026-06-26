use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, LitStr, Token,
    parse::{Parse, ParseStream},
};

use crate::clean_type_map::{CleanCallback, CleanType, parse_descriptor_type_expr};

struct DescriptorArgs {
    provider: LitStr,
    module: LitStr,
    functions: Vec<Function>,
}

struct Function {
    name: Ident,
    params: Vec<Param>,
    ret: CleanType,
}

struct Param {
    name: Ident,
    ty: CleanType,
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
    let module_segments = args
        .module
        .value()
        .split('.')
        .map(str::to_string)
        .collect::<Vec<_>>();
    let module_segments = module_segments
        .iter()
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
        let ty = type_tokens(&param.ty);
        quote! {
            anvyx_externs::ExternParam {
                name: Some(#name.to_string()),
                ty: #ty,
                flow: anvyx_externs::ParamFlow::Value,
                escape: anvyx_externs::CallbackEscape::NonEscaping,
            }
        }
    });
    let ret = type_tokens(&function.ret);
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

fn type_tokens(ty: &CleanType) -> TokenStream {
    match ty {
        CleanType::Void => quote! { anvyx_externs::ExternTypeExpr::Void },
        CleanType::Unit => quote! { anvyx_externs::ExternTypeExpr::Unit },
        CleanType::Bool => quote! { anvyx_externs::ExternTypeExpr::Bool },
        CleanType::Int => quote! { anvyx_externs::ExternTypeExpr::Int },
        CleanType::Float => quote! { anvyx_externs::ExternTypeExpr::Float },
        CleanType::String => quote! { anvyx_externs::ExternTypeExpr::String },
        CleanType::Named(name) => {
            quote! {
                anvyx_externs::ExternTypeExpr::Named {
                    module: None,
                    name: #name.to_string(),
                    args: vec![],
                }
            }
        }
        CleanType::Callback(callback) => callback_tokens(callback),
        CleanType::Option(item) => {
            let item = type_tokens(item);
            quote! { anvyx_externs::ExternTypeExpr::Option(Box::new(#item)) }
        }
        CleanType::Result(ok, err) => {
            let ok = type_tokens(ok);
            let err = type_tokens(err);
            quote! { anvyx_externs::ExternTypeExpr::Result(Box::new(#ok), Box::new(#err)) }
        }
        CleanType::Tuple(fields) => {
            let fields = fields.iter().map(type_tokens);
            quote! { anvyx_externs::ExternTypeExpr::Tuple(vec![#(#fields),*]) }
        }
        CleanType::Array { elem, len } => {
            let elem = type_tokens(elem);
            quote! { anvyx_externs::ExternTypeExpr::Array { elem: Box::new(#elem), len: #len } }
        }
        CleanType::VecList(item) | CleanType::List(item) => {
            let item = type_tokens(item);
            quote! { anvyx_externs::ExternTypeExpr::List(Box::new(#item)) }
        }
        CleanType::Map(key, value) => {
            let key = type_tokens(key);
            let value = type_tokens(value);
            quote! { anvyx_externs::ExternTypeExpr::Map(Box::new(#key), Box::new(#value)) }
        }
        CleanType::Slice(item) => {
            let item = type_tokens(item);
            quote! { anvyx_externs::ExternTypeExpr::Slice(Box::new(#item)) }
        }
    }
}

fn callback_tokens(callback: &CleanCallback) -> TokenStream {
    let params = callback.params.iter().map(|ty| {
        let ty = type_tokens(ty);
        quote! {
            anvyx_externs::ExternCallbackParam {
                ty: #ty,
                escape: anvyx_externs::CallbackEscape::NonEscaping,
            }
        }
    });
    let ret = type_tokens(&callback.ret);
    quote! {
        anvyx_externs::ExternTypeExpr::Callback(anvyx_externs::ExternCallbackSignature {
            params: vec![#(#params),*],
            ret: Box::new(#ret),
            policy: anvyx_externs::CallbackPolicy {
                escape: anvyx_externs::CallbackEscape::NonEscaping,
                thread: anvyx_externs::CallbackThread::SameThread,
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use quote::quote;

    use super::*;

    #[test]
    fn rejects_callback_return() {
        let err = syn::parse2::<DescriptorArgs>(quote! {
            provider = "host",
            module = "host",
            fn make() -> fn(int);
        })
        .err()
        .expect("expected callback return rejection");

        assert!(err.to_string().contains("callbacks are only supported"));
    }

    #[test]
    fn rejects_nested_callback() {
        let err = syn::parse2::<DescriptorArgs>(quote! {
            provider = "host",
            module = "host",
            fn each(fs: [fn(int)]) -> void;
        })
        .err()
        .expect("expected nested callback rejection");

        assert!(err.to_string().contains("callbacks are only supported"));
    }

    #[test]
    fn rejects_callback_above_max_arity() {
        let err = syn::parse2::<DescriptorArgs>(quote! {
            provider = "host",
            module = "host",
            fn bad(f: fn(int, int, int, int, int, int, int, int, int)) -> void;
        })
        .err()
        .expect("expected callback arity rejection");

        assert!(err.to_string().contains("at most 8"));
    }

    #[test]
    fn rejects_unsupported_callback_leaf_types() {
        for source in [
            quote! { fn bad(f: fn(void)) -> void; },
            quote! { fn bad(f: fn([int])) -> void; },
            quote! { fn bad(f: fn(int?)) -> void; },
            quote! { fn bad(f: fn(string)) -> void; },
            quote! { fn bad(f: fn(int) -> string) -> void; },
            quote! { fn bad(f: fn(int) -> [int]) -> void; },
            quote! { fn bad(f: fn(int) -> int?) -> void; },
        ] {
            let err = syn::parse2::<DescriptorArgs>(quote! {
                provider = "host",
                module = "host",
                #source
            })
            .err()
            .expect("expected unsupported callback leaf rejection");

            assert!(err.to_string().contains("unsupported callback"));
        }
    }
}
