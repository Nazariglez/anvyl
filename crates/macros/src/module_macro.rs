use std::collections::{BTreeMap, BTreeSet};

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Expr, Ident, LitStr, Path, Token,
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
    source: Expr,
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
            let _colon: Token![:] = input.parse()?;
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
                let _comma: Token![,] = input.parse()?;
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
        let mut source = None;
        let mut exports = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _colon: Token![:] = input.parse()?;
            match key.to_string().as_str() {
                "provider" => set_once(&mut provider, &key, "builtin_module!", input.parse()?)?,
                "name" => set_once(&mut name, &key, "builtin_module!", input.parse()?)?,
                "source" => set_once(&mut source, &key, "builtin_module!", input.parse()?)?,
                "exports" => set_list(&mut exports, &key, "builtin_module!", input)?,
                _ => {
                    return Err(syn::Error::new_spanned(
                        key,
                        "expected one of `provider`, `name`, `source`, or `exports`",
                    ));
                }
            }
            if !input.is_empty() {
                let _comma: Token![,] = input.parse()?;
            }
        }
        Ok(Self {
            provider,
            name: name.ok_or_else(|| missing("builtin_module!", "name"))?,
            source: source.ok_or_else(|| missing("builtin_module!", "source"))?,
            exports: exports.unwrap_or_default(),
        })
    }
}

impl Parse for ProviderPackageArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut modules = None;
        while !input.is_empty() {
            let key: Ident = input.parse()?;
            let _colon: Token![:] = input.parse()?;
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
                let _comma: Token![,] = input.parse()?;
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
    match expand_module_inner(input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error(),
    }
}

pub fn expand_builtin(input: TokenStream) -> TokenStream {
    match expand_builtin_inner(input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error(),
    }
}

pub fn expand_provider_package(input: TokenStream) -> TokenStream {
    match expand_provider_package_inner(input) {
        Ok(tokens) => tokens,
        Err(err) => err.to_compile_error(),
    }
}

fn expand_module_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let args: ModuleArgs = syn::parse2(input)?;
    let export_descriptors = export_descriptor_tokens(&args.exports)?;
    let native_exports = native_exports(&args.exports)?;
    let native_children = native_child_modules(&args.modules)?;
    let descriptor_children = descriptor_child_tokens(&args.modules);
    let support_bindings = support_binding_tokens(&args.exports)?;
    let support_children = support_child_tokens(&args.modules);
    let root = args.root.as_ref().map(root_tokens);

    Ok(quote! {
        #[doc(hidden)]
        pub fn __anvyx_module_descriptor() -> anvyx_runtime::ExternModuleDescriptor {
            let exports = #export_descriptors;
            anvyx_runtime::ExternModuleDescriptor {
                path: anvyx_runtime::ModulePath { segments: vec![] },
                types: exports.types.into_iter().map(|export| export.descriptor).collect(),
                functions: exports.functions.into_iter().map(|export| export.descriptor).collect(),
            }
        }

        #[doc(hidden)]
        pub fn __anvyx_module_descriptors(
            module_path: anvyx_runtime::ModulePath,
        ) -> Vec<anvyx_runtime::ExternModuleDescriptor> {
            let mut module = __anvyx_module_descriptor();
            module.path = module_path.clone();
            let mut modules = vec![module];
            #(#descriptor_children)*
            modules
        }

        #[doc(hidden)]
        pub fn __anvyx_module_support(
            module_path: anvyx_runtime::ModulePath,
            crate_name: &str,
            native_prefix: Vec<String>,
        ) -> anvyx_runtime::RustModuleSupport {
            let mut types = vec![];
            let mut bindings = vec![];
            #(#support_bindings)*
            anvyx_runtime::RustModuleSupport {
                module: module_path,
                types,
                bindings,
            }
        }

        #[doc(hidden)]
        pub fn __anvyx_module_supports(
            module_path: anvyx_runtime::ModulePath,
            crate_name: &str,
            native_prefix: Vec<String>,
        ) -> Vec<anvyx_runtime::RustModuleSupport> {
            let mut supports = vec![__anvyx_module_support(
                module_path.clone(),
                crate_name,
                native_prefix.clone(),
            )];
            #(#support_children)*
            supports
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
    let provider_lit = args.provider.unwrap_or_else(|| args.name.clone());
    let _source = args.source;
    let export_descriptors = export_descriptor_tokens(&args.exports)?;
    let native_exports = native_exports(&args.exports)?;
    let module_path = module_path_tokens(&name);
    let bindings = binding_tokens(&name, &args.exports)?;

    Ok(quote! {
        #[doc(hidden)]
        pub fn __anvyx_module_descriptor() -> anvyx_runtime::ExternModuleDescriptor {
            let exports = #export_descriptors;
            anvyx_runtime::ExternModuleDescriptor {
                path: #module_path,
                types: exports.types.into_iter().map(|export| export.descriptor).collect(),
                functions: exports.functions.into_iter().map(|export| export.descriptor).collect(),
            }
        }

        pub mod __anvyx_native {
            #(#native_exports)*
        }

        pub fn provider_descriptor() -> anvyx_runtime::ProviderDescriptor {
            anvyx_runtime::ProviderDescriptor {
                provider: anvyx_runtime::ProviderId { name: #provider_lit.to_string() },
                modules: vec![__anvyx_module_descriptor()],
            }
        }

        pub fn rust_module_support() -> anvyx_runtime::RustModuleSupport {
            let mut types = vec![];
            let mut bindings = vec![];
            #(#bindings)*
            anvyx_runtime::RustModuleSupport {
                module: #module_path,
                types,
                bindings,
            }
        }

        pub fn provider_descriptors() -> Vec<anvyx_runtime::ProviderDescriptor> {
            vec![provider_descriptor()]
        }

        pub fn rust_module_supports() -> Vec<anvyx_runtime::RustModuleSupport> {
            vec![rust_module_support()]
        }
    })
}

fn expand_provider_package_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let args: ProviderPackageArgs = syn::parse2(input)?;
    let descriptor_children = provider_package_descriptor_tokens(&args.modules);
    let support_children = provider_package_support_tokens(&args.modules);
    let native_children = provider_package_native_tokens(&args.modules);

    Ok(quote! {
        pub mod __anvyx_native_package {
            #(#native_children)*
        }

        pub fn provider_descriptors() -> Vec<anvyx_runtime::ProviderDescriptor> {
            let mut descriptors = vec![];
            #(#descriptor_children)*
            descriptors
        }

        pub fn rust_module_supports() -> Vec<anvyx_runtime::RustModuleSupport> {
            let package_root = module_path!()
                .split("::")
                .skip(1)
                .map(str::to_string)
                .collect::<Vec<_>>();
            let mut supports = vec![];
            #(#support_children)*
            supports
        }
    })
}

fn export_descriptor_tokens(exports: &[Path]) -> syn::Result<TokenStream> {
    let mut pushes = vec![];
    for export in exports {
        validate_plain_path(export)?;
        let companion = companion_path(export)?;
        pushes.push(quote! {
            anvyx_runtime::ModuleExportItem::push_descriptor(#companion(), &mut exports);
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

fn descriptor_child_tokens(modules: &[Path]) -> Vec<TokenStream> {
    modules
        .iter()
        .map(|module| {
            let segments = path_segment_tokens(module);
            quote! {
                let mut child_path = module_path.clone();
                child_path.segments.extend([#(#segments.to_string()),*]);
                modules.extend(#module::__anvyx_module_descriptors(child_path));
            }
        })
        .collect()
}

fn support_child_tokens(modules: &[Path]) -> Vec<TokenStream> {
    modules
        .iter()
        .map(|module| {
            let segments = path_segment_tokens(module);
            quote! {
                let mut child_path = module_path.clone();
                child_path.segments.extend([#(#segments.to_string()),*]);
                let mut child_prefix = native_prefix.clone();
                child_prefix.extend([#(#segments.to_string()),*]);
                supports.extend(#module::__anvyx_module_supports(child_path, crate_name, child_prefix));
            }
        })
        .collect()
}

fn provider_package_descriptor_tokens(modules: &[Path]) -> Vec<TokenStream> {
    modules
        .iter()
        .map(|module| quote! { descriptors.extend(#module::provider_descriptors()); })
        .collect()
}

fn provider_package_support_tokens(modules: &[Path]) -> Vec<TokenStream> {
    modules
        .iter()
        .map(|module| {
            let segments = path_segment_tokens(module);
            quote! {
                let mut module_root = package_root.clone();
                module_root.extend([#(#segments.to_string()),*]);
                let mut native_root = package_root.clone();
                native_root.push("__anvyx_native_package".to_string());
                native_root.extend([#(#segments.to_string()),*]);
                let mut child_supports = #module::rust_module_supports();
                for support in &mut child_supports {
                    for ty in &mut support.types {
                        ty.path.retarget_prefix(&module_root, &native_root);
                    }
                    for binding in &mut support.bindings {
                        binding.path.retarget_native_root(&native_root);
                    }
                }
                supports.extend(child_supports);
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
    let node = &branch.tree;
    let reexport = node.target.as_ref().map(|module| {
        let parents = (0..=depth).map(|_| Ident::new("super", proc_macro2::Span::call_site()));
        quote! { pub use #(#parents)::*::#module::*; }
    });
    let children = node
        .children
        .values()
        .map(|branch| provider_package_native_tree_token(branch, depth + 1));
    quote! { pub mod #ident { #reexport #(#children)* } }
}

fn binding_tokens(module: &str, exports: &[Path]) -> syn::Result<Vec<TokenStream>> {
    exports
        .iter()
        .map(|export| {
            let companion = companion_path(export)?;
            let module_path = module_path_tokens(module);
            Ok(quote! {
                let export = #companion();
                types.extend(anvyx_runtime::ModuleExportItem::rust_type_bindings(
                    export.clone(),
                    #module_path.clone(),
                    "crate",
                ));
                bindings.extend(anvyx_runtime::ModuleExportItem::rust_bindings(
                    export,
                    #module_path,
                    "crate",
                ));
            })
        })
        .collect()
}

fn support_binding_tokens(exports: &[Path]) -> syn::Result<Vec<TokenStream>> {
    exports
        .iter()
        .map(|export| {
            let companion = companion_path(export)?;
            Ok(quote! {
                let export = #companion();
                types.extend(anvyx_runtime::ModuleExportItem::rust_type_bindings(
                    export.clone(),
                    module_path.clone(),
                    crate_name,
                ));
                let mut export_bindings = anvyx_runtime::ModuleExportItem::rust_bindings(
                    export,
                    module_path.clone(),
                    crate_name,
                );
                for binding in &mut export_bindings {
                    binding.path.prefix_native(&native_prefix);
                }
                bindings.extend(export_bindings);
            })
        })
        .collect()
}

fn root_tokens(root: &Ident) -> TokenStream {
    let root_name = root.to_string();
    let module_path = module_path_tokens(&root_name);
    quote! {
        pub fn provider_descriptor() -> anvyx_runtime::ProviderDescriptor {
            anvyx_runtime::ProviderDescriptor {
                provider: anvyx_runtime::ProviderId { name: #root_name.to_string() },
                modules: __anvyx_module_descriptors(#module_path),
            }
        }

        pub fn provider_descriptors() -> Vec<anvyx_runtime::ProviderDescriptor> {
            vec![provider_descriptor()]
        }

        pub fn rust_module_support() -> anvyx_runtime::RustModuleSupport {
            __anvyx_module_support(#module_path, "crate", vec![])
        }

        pub fn rust_module_supports() -> Vec<anvyx_runtime::RustModuleSupport> {
            __anvyx_module_supports(#module_path, "crate", vec![])
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

fn path_segment_tokens(path: &Path) -> Vec<String> {
    path.segments
        .iter()
        .map(|segment| ident_text(&segment.ident))
        .collect()
}

fn ident_text(ident: &Ident) -> String {
    let text = ident.to_string();
    match text.strip_prefix("r#") {
        Some(unraw) => unraw.to_string(),
        None => text,
    }
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
    let parsed: Punctuated<Path, Token![,]> = Punctuated::parse_terminated(&content)?;
    let items = parsed.into_iter().collect::<Vec<_>>();
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
    let parsed: Punctuated<Path, Token![,]> = Punctuated::parse_terminated(&content)?;
    let items = parsed.into_iter().collect::<Vec<_>>();
    let mut seen = BTreeSet::new();
    for path in &items {
        validate_provider_module_path(path)?;
        let normalized = path_segment_tokens(path).join("::");
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

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_provider_package(input: TokenStream) -> syn::Result<ProviderPackageArgs> {
        syn::parse2(input)
    }

    #[test]
    fn provider_package_accepts_raw_module_idents() {
        let args = parse_provider_package(quote! { modules: [r#type] }).unwrap();

        assert_eq!(path_segment_tokens(&args.modules[0]), ["type"]);
        let _ = provider_package_native_tokens(&args.modules);
    }

    #[test]
    fn provider_package_rejects_invalid_modules() {
        for input in [
            quote! {},
            quote! { exports: [window] },
            quote! { modules: [] },
            quote! { modules: [window, window] },
            quote! { modules: [crate::window] },
            quote! { modules: [self::window] },
            quote! { modules: [super::window] },
            quote! { modules: [window::<T>] },
        ] {
            assert!(parse_provider_package(input).is_err());
        }
    }
}
