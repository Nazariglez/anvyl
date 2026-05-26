use proc_macro2::TokenStream;

pub fn expand(input: TokenStream) -> TokenStream {
    crate::clean_type_derive::expand(input, crate::clean_type_derive::TypeDeriveKind::Inline)
}
