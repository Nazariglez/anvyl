use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Ident, LitStr, Path, Token,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

struct ModuleArgs {
    root: Option<Ident>,
    modules: Vec<Path>,
    exports: Vec<Path>,
}

struct BuiltinArgs {
    provider: Option<LitStr>,
    name: LitStr,
    root: bool,
    exports: Vec<Path>,
}

struct ProviderPackageArgs {
    modules: Vec<Path>,
}

impl Parse for ModuleArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut root = None;
        let mut modules = None;
        let mut exports = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _: Token![:] = input.parse()?;
            match key.to_string().as_str() {
                "root" => set_once(&mut root, &key, "module!", input.parse()?)?,
                "modules" => set_list(&mut modules, &key, "module!", input)?,
                "exports" => set_list(&mut exports, &key, "module!", input)?,
                _ => {
                    return Err(syn::Error::new_spanned(
                        key,
                        "expected one of `root`, `modules`, or `exports`",
                    ));
                }
            }
            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }
        Ok(Self {
            root,
            modules: modules.unwrap_or_default(),
            exports: exports.unwrap_or_default(),
        })
    }
}

impl Parse for BuiltinArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut provider = None;
        let mut name = None;
        let mut root = None;
        let mut exports = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _: Token![:] = input.parse()?;
            match key.to_string().as_str() {
                "provider" => set_once(&mut provider, &key, "builtin_module!", input.parse()?)?,
                "name" => set_once(&mut name, &key, "builtin_module!", input.parse()?)?,
                "root" => set_once(
                    &mut root,
                    &key,
                    "builtin_module!",
                    input.parse::<syn::LitBool>()?.value,
                )?,
                "exports" => set_list(&mut exports, &key, "builtin_module!", input)?,
                _ => {
                    return Err(syn::Error::new_spanned(
                        key,
                        "expected one of `provider`, `name`, `root`, or `exports`",
                    ));
                }
            }
            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }
        Ok(Self {
            provider,
            name: name.ok_or_else(|| missing("builtin_module!", "name"))?,
            root: root.unwrap_or(true),
            exports: exports.unwrap_or_default(),
        })
    }
}

impl Parse for ProviderPackageArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut modules = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _: Token![:] = input.parse()?;
            match key.to_string().as_str() {
                "modules" => set_provider_modules(&mut modules, &key, input)?,
                _ => {
                    return Err(syn::Error::new_spanned(
                        key,
                        "expected provider_package! key `modules`",
                    ));
                }
            }
            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }
        let modules = modules.ok_or_else(|| missing("provider_package!", "modules"))?;
        if modules.is_empty() {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "provider_package! key `modules` must not be empty",
            ));
        }
        Ok(Self { modules })
    }
}

pub fn expand_module(input: TokenStream) -> TokenStream {
    expand_module_inner(input).unwrap_or_else(|error| error.to_compile_error())
}

pub fn expand_builtin(input: TokenStream) -> TokenStream {
    expand_builtin_inner(input).unwrap_or_else(|error| error.to_compile_error())
}

pub fn expand_provider_package(input: TokenStream) -> TokenStream {
    expand_provider_package_inner(input).unwrap_or_else(|error| error.to_compile_error())
}

fn expand_module_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let args: ModuleArgs = syn::parse2(input)?;
    let exports = export_tokens(&args.exports)?;
    let native_exports = native_exports(&args.exports)?;
    let native_children = native_child_modules(&args.modules)?;
    let child_exports = child_module_exports(&args.modules);
    let root = args.root.as_ref().map(root_tokens);

    Ok(quote! {
        pub(crate) fn __anvyx_module_exports(
            package: &mut anvyx_runtime::RawProviderPackage,
            provider: anvyx_runtime::ProviderId,
            module_path: anvyx_runtime::ModulePath,
            function_prefix: Vec<String>,
            type_prefix: Option<Vec<String>>,
        ) {
            let exports = #exports;
            exports.finalize(
                package,
                provider.clone(),
                module_path.clone(),
                &function_prefix,
                type_prefix.as_deref(),
            );
            #(#child_exports)*
        }

        pub mod __anvyx_native {
            #(#native_exports)*
            #(#native_children)*
        }

        #root
    })
}

fn expand_builtin_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let args: BuiltinArgs = syn::parse2(input)?;
    let name = args.name.value();
    let provider = args.provider.unwrap_or_else(|| args.name.clone());
    let exports = export_tokens(&args.exports)?;
    let native_exports = native_exports(&args.exports)?;
    let path = module_path_tokens(&name);
    let root_export = args.root.then(|| {
        quote! {
            pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
                let mut package = anvyx_runtime::RawProviderPackage::default();
                __anvyx_provider_export(
                    &mut package,
                    vec!["__anvyx_native".to_string()],
                    None,
                );
                package
            }
        }
    });

    Ok(quote! {
        pub(crate) fn __anvyx_provider_export(
            package: &mut anvyx_runtime::RawProviderPackage,
            function_prefix: Vec<String>,
            type_prefix: Option<Vec<String>>,
        ) {
            let exports = #exports;
            exports.finalize(
                package,
                anvyx_runtime::ProviderId { name: #provider.to_string() },
                #path,
                &function_prefix,
                type_prefix.as_deref(),
            );
        }

        pub mod __anvyx_native {
            #(#native_exports)*
        }

        #root_export
    })
}

fn expand_provider_package_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let args: ProviderPackageArgs = syn::parse2(input)?;
    let children = provider_package_exports(&args.modules);
    let native_children = provider_package_native_tokens(&args.modules);

    Ok(quote! {
        pub mod __anvyx_native_package {
            #(#native_children)*
        }

        pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
            let package_root = module_path!()
                .split("::")
                .skip(1)
                .map(str::to_string)
                .collect::<Vec<_>>();
            let mut package = anvyx_runtime::RawProviderPackage::default();
            #(#children)*
            package
        }
    })
}

fn export_tokens(exports: &[Path]) -> syn::Result<TokenStream> {
    let mut pushes = vec![];
    for export in exports {
        validate_plain_path(export)?;
        let companion = companion_path(export)?;
        pushes.push(quote! {
            exports.extend(#companion());
        });
    }
    Ok(quote! {{
        let mut exports = anvyx_runtime::ModuleExport::default();
        #(#pushes)*
        exports
    }})
}

fn native_exports(exports: &[Path]) -> syn::Result<Vec<TokenStream>> {
    exports
        .iter()
        .map(|export| {
            validate_plain_path(export)?;
            let native = native_path(export)?;
            Ok(quote! { pub use super::#native::*; })
        })
        .collect()
}

fn native_child_modules(modules: &[Path]) -> syn::Result<Vec<TokenStream>> {
    modules
        .iter()
        .map(|module| {
            let ident = module
                .segments
                .last()
                .ok_or_else(|| syn::Error::new_spanned(module, "expected module path"))?
                .ident
                .clone();
            Ok(quote! { pub mod #ident { pub use super::super::#module::__anvyx_native::*; } })
        })
        .collect()
}

fn child_module_exports(modules: &[Path]) -> Vec<TokenStream> {
    modules
        .iter()
        .map(|module| {
            let module_segments = path_segment_tokens(module, false);
            let rust_segments = path_segment_tokens(module, true);
            quote! {
                let mut child_path = module_path.clone();
                child_path.segments.extend([#(#module_segments.to_string()),*]);
                let mut child_functions = function_prefix.clone();
                child_functions.extend([#(#rust_segments.to_string()),*]);
                let child_types = type_prefix.as_ref().map(|prefix| {
                    let mut prefix = prefix.clone();
                    prefix.extend([#(#rust_segments.to_string()),*]);
                    prefix
                });
                #module::__anvyx_module_exports(
                    package,
                    provider.clone(),
                    child_path,
                    child_functions,
                    child_types,
                );
            }
        })
        .collect()
}

fn provider_package_exports(modules: &[Path]) -> Vec<TokenStream> {
    modules
        .iter()
        .map(|module| {
            let rust_segments = path_segment_tokens(module, true);
            quote! {
                let mut prefix = package_root.clone();
                prefix.push("__anvyx_native_package".to_string());
                prefix.extend([#(#rust_segments.to_string()),*]);
                let mut function_prefix = prefix.clone();
                function_prefix.push("__anvyx_native".to_string());
                #module::__anvyx_provider_export(&mut package, function_prefix, Some(prefix));
            }
        })
        .collect()
}

#[derive(Default)]
struct NativeTree {
    target: Option<Path>,
    children: BTreeMap<String, NativeBranch>,
}

struct NativeBranch {
    ident: Ident,
    tree: NativeTree,
}

impl NativeBranch {
    fn new(ident: Ident) -> Self {
        Self {
            ident,
            tree: NativeTree::default(),
        }
    }
}

fn provider_package_native_tokens(modules: &[Path]) -> Vec<TokenStream> {
    let mut tree = NativeTree::default();
    for module in modules {
        let mut node = &mut tree;
        for segment in &module.segments {
            let branch = node
                .children
                .entry(ident_text(&segment.ident))
                .or_insert_with(|| NativeBranch::new(segment.ident.clone()));
            node = &mut branch.tree;
        }
        node.target = Some(module.clone());
    }
    tree.children
        .values()
        .map(|branch| provider_package_native_tree_token(branch, 1))
        .collect()
}

fn provider_package_native_tree_token(branch: &NativeBranch, depth: usize) -> TokenStream {
    let ident = &branch.ident;
    let reexport = branch.tree.target.as_ref().map(|module| {
        let parents = (0..=depth).map(|_| Ident::new("super", proc_macro2::Span::call_site()));
        quote! { pub use #(#parents)::*::#module::*; }
    });
    let children = branch
        .tree
        .children
        .values()
        .map(|child| provider_package_native_tree_token(child, depth + 1));
    quote! { pub mod #ident { #reexport #(#children)* } }
}

fn root_tokens(root: &Ident) -> TokenStream {
    let root_name = root.to_string();
    let path = module_path_tokens(&root_name);
    quote! {
        pub fn rust_providers() -> anvyx_runtime::RawProviderPackage {
            let mut package = anvyx_runtime::RawProviderPackage::default();
            __anvyx_module_exports(
                &mut package,
                anvyx_runtime::ProviderId { name: #root_name.to_string() },
                #path,
                vec!["__anvyx_native".to_string()],
                None,
            );
            package
        }
    }
}

fn companion_path(path: &Path) -> syn::Result<Path> {
    let mut path = path.clone();
    let last = path
        .segments
        .last_mut()
        .ok_or_else(|| syn::Error::new(proc_macro2::Span::call_site(), "expected export path"))?;
    last.ident = crate::naming::fn_companion_ident(&last.ident);
    Ok(path)
}

fn native_path(path: &Path) -> syn::Result<Path> {
    let mut path = path.clone();
    let last = path
        .segments
        .last_mut()
        .ok_or_else(|| syn::Error::new(proc_macro2::Span::call_site(), "expected export path"))?;
    last.ident = crate::naming::native_export_module_ident(&last.ident);
    Ok(path)
}

fn path_segment_tokens(path: &Path, preserve_raw: bool) -> Vec<String> {
    path.segments
        .iter()
        .map(|segment| {
            let text = segment.ident.to_string();
            if preserve_raw {
                text
            } else {
                text.strip_prefix("r#").unwrap_or(&text).to_string()
            }
        })
        .collect()
}

fn ident_text(ident: &Ident) -> String {
    let text = ident.to_string();
    text.strip_prefix("r#").unwrap_or(&text).to_string()
}

fn module_path_tokens(module: &str) -> TokenStream {
    let segments = module.split("::").map(str::to_string);
    quote! { anvyx_runtime::ModulePath { segments: vec![#(#segments.to_string()),*] } }
}

fn set_once<T>(slot: &mut Option<T>, key: &Ident, macro_name: &str, value: T) -> syn::Result<()> {
    if slot.is_some() {
        return Err(syn::Error::new_spanned(
            key,
            format!("duplicate {macro_name} key `{key}`"),
        ));
    }
    *slot = Some(value);
    Ok(())
}

fn set_list(
    slot: &mut Option<Vec<Path>>,
    key: &Ident,
    macro_name: &str,
    input: ParseStream,
) -> syn::Result<()> {
    if slot.is_some() {
        return Err(syn::Error::new_spanned(
            key,
            format!("duplicate {macro_name} key `{key}`"),
        ));
    }
    let content;
    syn::bracketed!(content in input);
    let items = Punctuated::<Path, Token![,]>::parse_terminated(&content)
        .map(|items| items.into_iter().collect::<Vec<_>>())?;
    for path in &items {
        validate_plain_path(path)?;
    }
    *slot = Some(items);
    Ok(())
}

fn set_provider_modules(
    slot: &mut Option<Vec<Path>>,
    key: &Ident,
    input: ParseStream,
) -> syn::Result<()> {
    if slot.is_some() {
        return Err(syn::Error::new_spanned(
            key,
            format!("duplicate provider_package! key `{key}`"),
        ));
    }
    let content;
    syn::bracketed!(content in input);
    let items = Punctuated::<Path, Token![,]>::parse_terminated(&content)
        .map(|items| items.into_iter().collect::<Vec<_>>())?;
    let mut seen = BTreeSet::new();
    for path in &items {
        validate_provider_module_path(path)?;
        let normalized = path_segment_tokens(path, false).join("::");
        if !seen.insert(normalized.clone()) {
            return Err(syn::Error::new_spanned(
                path,
                format!("duplicate provider_package! module `{normalized}`"),
            ));
        }
    }
    *slot = Some(items);
    Ok(())
}

fn validate_plain_path(path: &Path) -> syn::Result<()> {
    for segment in &path.segments {
        if !matches!(segment.arguments, syn::PathArguments::None) {
            return Err(syn::Error::new_spanned(
                path,
                "generic paths are not supported here",
            ));
        }
    }
    Ok(())
}

fn validate_provider_module_path(path: &Path) -> syn::Result<()> {
    validate_plain_path(path)?;
    if path.leading_colon.is_some() {
        return Err(syn::Error::new_spanned(
            path,
            "provider_package! module paths must be crate-root relative, not absolute",
        ));
    }
    let Some(first) = path.segments.first() else {
        return Err(syn::Error::new_spanned(
            path,
            "expected provider module path",
        ));
    };
    match first.ident.to_string().as_str() {
        "crate" | "self" | "super" => Err(syn::Error::new_spanned(
            path,
            "provider_package! module paths must not start with `crate`, `self`, or `super`",
        )),
        _ => Ok(()),
    }
}

fn missing(macro_name: &str, key: &str) -> syn::Error {
    syn::Error::new(
        proc_macro2::Span::call_site(),
        format!("missing {macro_name} key `{key}`"),
    )
}
