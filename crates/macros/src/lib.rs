mod boundary;
mod function_macro;
mod methods_macro;
mod module_macro;
mod naming;
mod provider_descriptor;
mod type_derive;
mod util;
use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn function(attr: TokenStream, item: TokenStream) -> TokenStream {
    function_macro::expand(attr.into(), item.into()).into()
}

#[proc_macro_derive(AnvyxInline, attributes(anvyx))]
pub fn derive_anvyx_inline(item: TokenStream) -> TokenStream {
    type_derive::expand(item.into(), type_derive::TypeDeriveKind::Inline).into()
}

#[proc_macro_derive(AnvyxRef, attributes(anvyx))]
pub fn derive_anvyx_ref(item: TokenStream) -> TokenStream {
    type_derive::expand(item.into(), type_derive::TypeDeriveKind::Ref).into()
}

#[proc_macro_derive(AnvyxEnum, attributes(anvyx))]
pub fn derive_anvyx_enum(item: TokenStream) -> TokenStream {
    type_derive::expand(item.into(), type_derive::TypeDeriveKind::Enum).into()
}

#[proc_macro_attribute]
pub fn methods(attr: TokenStream, item: TokenStream) -> TokenStream {
    methods_macro::expand(attr.into(), item.into()).into()
}

#[proc_macro]
pub fn module(input: TokenStream) -> TokenStream {
    module_macro::expand_module(input.into()).into()
}

#[proc_macro]
pub fn builtin_module(input: TokenStream) -> TokenStream {
    module_macro::expand_builtin(input.into()).into()
}

#[proc_macro]
pub fn provider_package(input: TokenStream) -> TokenStream {
    module_macro::expand_provider_package(input.into()).into()
}

#[proc_macro]
pub fn provider_descriptor(input: TokenStream) -> TokenStream {
    provider_descriptor::expand(input.into()).into()
}
