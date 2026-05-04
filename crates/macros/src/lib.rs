mod codegen;
mod expand;
mod export_methods;
mod export_type;
mod naming;
mod provider;
mod provider_descriptor;
mod type_map;
mod util;

use proc_macro::TokenStream;

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
pub fn provider(input: TokenStream) -> TokenStream {
    provider::expand(input.into()).into()
}

#[proc_macro]
pub fn provider_descriptor(input: TokenStream) -> TokenStream {
    provider_descriptor::expand(input.into()).into()
}
