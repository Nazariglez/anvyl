mod clean_function;
mod clean_inline;
mod clean_methods;
mod clean_module;
mod clean_ref;
mod clean_type_derive;
mod clean_type_map;
mod naming;
mod provider_descriptor;
mod util;

// Legacy VM/provider macro implementation used by `anvyx-lang` re-exports.
mod codegen;
mod expand;
mod export_methods;
mod export_type;
mod provider;
mod type_map;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn function(attr: TokenStream, item: TokenStream) -> TokenStream {
    clean_function::expand(attr.into(), item.into()).into()
}

#[proc_macro_derive(AnvyxInline, attributes(anvyx))]
pub fn derive_anvyx_inline(item: TokenStream) -> TokenStream {
    clean_inline::expand(item.into()).into()
}

#[proc_macro_derive(AnvyxRef, attributes(anvyx))]
pub fn derive_anvyx_ref(item: TokenStream) -> TokenStream {
    clean_ref::expand(item.into()).into()
}

#[proc_macro_derive(AnvyxEnum, attributes(anvyx))]
pub fn derive_anvyx_enum(item: TokenStream) -> TokenStream {
    clean_type_derive::expand(item.into(), clean_type_derive::TypeDeriveKind::Enum).into()
}

#[proc_macro_attribute]
pub fn methods(attr: TokenStream, item: TokenStream) -> TokenStream {
    clean_methods::expand(attr.into(), item.into()).into()
}

/// # Override the exported name
///
/// ```rust,ignore
/// #[export_fn(name = "add")]
/// pub fn engine_add(a: i64, b: i64) -> i64 { a + b }
/// ```
#[proc_macro_attribute]
pub fn export_fn(attr: TokenStream, item: TokenStream) -> TokenStream {
    expand::expand(attr.into(), item.into()).into()
}

/// Generates `pub fn anvyx_externs() -> HashMap<String, ExternHandler>`
///
/// # Example
///
/// ```rust,ignore
/// mod math { use anvyx_lang::export_fn; #[export_fn] pub fn add(a: i64, b: i64) -> i64 { a + b } }
///
/// anvyx_lang::provider!(math::add);
/// ```
#[proc_macro_attribute]
pub fn export_type(attr: TokenStream, item: TokenStream) -> TokenStream {
    export_type::expand(attr.into(), item.into()).into()
}

#[proc_macro_attribute]
pub fn export_methods(attr: TokenStream, item: TokenStream) -> TokenStream {
    export_methods::expand(attr.into(), item.into()).into()
}

#[proc_macro]
pub fn module(input: TokenStream) -> TokenStream {
    clean_module::expand_module(input.into()).into()
}

#[proc_macro]
pub fn builtin_module(input: TokenStream) -> TokenStream {
    clean_module::expand_builtin(input.into()).into()
}

#[proc_macro]
pub fn provider_package(input: TokenStream) -> TokenStream {
    clean_module::expand_provider_package(input.into()).into()
}

#[proc_macro]
pub fn provider(input: TokenStream) -> TokenStream {
    provider::expand(input.into()).into()
}

#[proc_macro]
pub fn provider_descriptor(input: TokenStream) -> TokenStream {
    provider_descriptor::expand(input.into()).into()
}

#[cfg(test)]
mod tests {
    const CLEAN_MODULES: &[(&str, &str)] = &[
        ("clean_function.rs", include_str!("clean_function.rs")),
        ("clean_inline.rs", include_str!("clean_inline.rs")),
        ("clean_methods.rs", include_str!("clean_methods.rs")),
        ("clean_module.rs", include_str!("clean_module.rs")),
        ("clean_ref.rs", include_str!("clean_ref.rs")),
        ("clean_type_derive.rs", include_str!("clean_type_derive.rs")),
        ("clean_type_map.rs", include_str!("clean_type_map.rs")),
        (
            "provider_descriptor.rs",
            include_str!("provider_descriptor.rs"),
        ),
    ];

    #[test]
    fn clean_macros_do_not_reference_legacy_runtime() {
        for (name, source) in CLEAN_MODULES {
            for banned in ["anvyx_lang", "ExternHandler", "StdModule"] {
                assert!(
                    !source.contains(banned),
                    "{name} must not reference legacy {banned}"
                );
            }
        }
    }
}
