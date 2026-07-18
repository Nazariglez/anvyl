use proc_macro2::Ident;
use quote::format_ident;

/// `__anvyx_export_{name}`
pub fn fn_companion_ident(fn_name: &Ident) -> Ident {
    format_ident!("__anvyx_export_{}", generated_suffix(fn_name))
}

/// `__anvyx_native_export_{name}`
pub fn native_export_module_ident(fn_name: &Ident) -> Ident {
    format_ident!(
        "{}",
        anvyx_externs::native_materializer_module(&fn_name.to_string())
    )
}

pub fn materializer_fn_ident() -> Ident {
    format_ident!("{}", anvyx_externs::INLINE_MATERIALIZER_SYMBOL)
}

fn generated_suffix(ident: &Ident) -> String {
    let trimmed = ident.to_string().trim_start_matches('_').to_lowercase();
    if trimmed.is_empty() {
        "_".to_string()
    } else {
        trimmed
    }
}

/// `__anvyx_methods_{name}`
pub fn methods_fn_ident(name: &Ident) -> Ident {
    format_ident!("__anvyx_methods_{}", generated_suffix(name))
}
