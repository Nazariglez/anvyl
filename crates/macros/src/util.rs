use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn expand_or_error(item: &TokenStream, result: syn::Result<TokenStream>) -> TokenStream {
    match result {
        Ok(ts) => ts,
        Err(e) => {
            let err = e.to_compile_error();
            quote! { #err #item }
        }
    }
}

pub(crate) fn extract_doc(attrs: &[syn::Attribute]) -> Option<String> {
    let lines = attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("doc")
                && let syn::Meta::NameValue(nv) = &attr.meta
                && let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = &nv.value
            {
                return Some(s.value());
            }
            None
        })
        .collect::<Vec<_>>();
    if lines.is_empty() {
        None
    } else {
        Some(lines.join("\n").trim().to_string())
    }
}
