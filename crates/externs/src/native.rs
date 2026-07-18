pub const INLINE_MATERIALIZER_SYMBOL: &str = "__anvyx_materialize";

pub fn native_materializer_module(rust_type_path: &str) -> String {
    let name = rust_type_path
        .rsplit("::")
        .next()
        .expect("Rust type path must contain a type name");
    let suffix = name.trim_start_matches('_').to_lowercase();
    let suffix = if suffix.is_empty() { "_" } else { &suffix };
    format!("__anvyx_native_export_{suffix}")
}
