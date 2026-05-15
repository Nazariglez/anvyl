use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, LitStr, Token, bracketed,
    parse::{Parse, ParseStream},
};

struct DescriptorArgs {
    provider: LitStr,
    module: LitStr,
    functions: Vec<Function>,
}

struct Function {
    name: Ident,
    params: Vec<Param>,
    ret: TypeExpr,
}

struct Param {
    name: Ident,
    ty: TypeExpr,
}

#[derive(Clone)]
enum TypeExpr {
    Void,
    Bool,
    Int,
    Float,
    String,
    List(Box<TypeExpr>),
    Option(Box<TypeExpr>),
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
        let ret = input.parse()?;
        input.parse::<Token![;]>()?;
        let _ = input.parse::<Token![,]>();
        Ok(Self { name, params, ret })
    }
}

impl Parse for Param {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty = input.parse()?;
        Ok(Self { name, ty })
    }
}

impl Parse for TypeExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut ty = if input.peek(syn::token::Bracket) {
            let content;
            bracketed!(content in input);
            Self::List(Box::new(content.parse()?))
        } else {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "void" => Self::Void,
                "bool" => Self::Bool,
                "int" => Self::Int,
                "float" => Self::Float,
                "string" => Self::String,
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
            ty = Self::Option(Box::new(ty));
        }
        Ok(ty)
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

fn type_tokens(ty: &TypeExpr) -> TokenStream {
    match ty {
        TypeExpr::Void => quote! { anvyx_externs::ExternTypeExpr::Void },
        TypeExpr::Bool => quote! { anvyx_externs::ExternTypeExpr::Bool },
        TypeExpr::Int => quote! { anvyx_externs::ExternTypeExpr::Int },
        TypeExpr::Float => quote! { anvyx_externs::ExternTypeExpr::Float },
        TypeExpr::String => quote! { anvyx_externs::ExternTypeExpr::String },
        TypeExpr::List(item) => {
            let item = type_tokens(item);
            quote! { anvyx_externs::ExternTypeExpr::List(Box::new(#item)) }
        }
        TypeExpr::Option(item) => {
            let item = type_tokens(item);
            quote! { anvyx_externs::ExternTypeExpr::Option(Box::new(#item)) }
        }
    }
}
