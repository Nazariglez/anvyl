pub struct SourceFile {
    pub path: &'static [&'static str],
    pub label: &'static str,
    pub code: &'static str,
}

pub const ROOT: SourceFile = SourceFile {
    path: &[],
    label: "crates/core2/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[
    SourceFile {
        path: &["option"],
        label: "crates/core2/src/option.anv",
        code: include_str!("option.anv"),
    },
    SourceFile {
        path: &["range"],
        label: "crates/core2/src/range.anv",
        code: include_str!("range.anv"),
    },
    SourceFile {
        path: &["int"],
        label: "crates/core2/src/int.anv",
        code: include_str!("int.anv"),
    },
    SourceFile {
        path: &["float"],
        label: "crates/core2/src/float.anv",
        code: include_str!("float.anv"),
    },
    SourceFile {
        path: &["string"],
        label: "crates/core2/src/string.anv",
        code: include_str!("string.anv"),
    },
];

// FIXME: we need a way to re-export extend blocks without this tricks
pub const ALWAYS_ACTIVE: &[&[&str]] = &[&["int"], &["float"], &["string"]];

pub fn provider_descriptors() -> Vec<anvyx_externs::ProviderDescriptor> {
    vec![int_provider(), float_provider(), string_provider()]
}

fn int_provider() -> anvyx_externs::ProviderDescriptor {
    anvyx_lang::provider_descriptor!(
        provider = "core_int",
        module = "int",
        fn int_abs(x: int) -> int;,
        fn int_min(a: int, b: int) -> int;,
        fn int_max(a: int, b: int) -> int;,
        fn int_clamp(x: int, lo: int, hi: int) -> int;
    )
}

fn float_provider() -> anvyx_externs::ProviderDescriptor {
    anvyx_lang::provider_descriptor!(
        provider = "core_float",
        module = "float",
        fn float_sin(x: float) -> float;,
        fn float_cos(x: float) -> float;,
        fn float_tan(x: float) -> float;,
        fn float_asin(x: float) -> float;,
        fn float_acos(x: float) -> float;,
        fn float_atan(x: float) -> float;,
        fn float_atan2(y: float, x: float) -> float;,
        fn float_floor(x: float) -> float;,
        fn float_ceil(x: float) -> float;,
        fn float_round(x: float) -> float;,
        fn float_trunc(x: float) -> float;,
        fn float_sqrt(x: float) -> float;,
        fn float_cbrt(x: float) -> float;,
        fn float_pow(x: float, exp: float) -> float;,
        fn float_exp(x: float) -> float;,
        fn float_ln(x: float) -> float;,
        fn float_abs(x: float) -> float;,
        fn float_min(a: float, b: float) -> float;,
        fn float_max(a: float, b: float) -> float;,
        fn float_clamp(x: float, lo: float, hi: float) -> float;,
        fn float_lerp(x: float, target: float, t: float) -> float;,
        fn float_to_radians(x: float) -> float;,
        fn float_to_degrees(x: float) -> float;
    )
}

fn string_provider() -> anvyx_externs::ProviderDescriptor {
    anvyx_lang::provider_descriptor!(
        provider = "core_string",
        module = "string",
        fn str_len(s: string) -> int;
        fn str_contains(s: string, sub: string) -> bool;
        fn str_starts_with(s: string, prefix: string) -> bool;
        fn str_ends_with(s: string, suffix: string) -> bool;
        fn str_find(s: string, sub: string) -> int;
        fn str_to_upper(s: string) -> string;
        fn str_to_lower(s: string) -> string;
        fn str_trim(s: string) -> string;
        fn str_trim_start(s: string) -> string;
        fn str_trim_end(s: string) -> string;
        fn str_substring(s: string, start: int, len: int) -> string?;
        fn str_char_at(s: string, index: int) -> string?;
        fn str_split(s: string, sep: string) -> [string];
        fn str_replace(s: string, from: string, to: string) -> string;
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn paths(items: &[SourceFile]) -> Vec<&'static [&'static str]> {
        items.iter().map(|item| item.path).collect()
    }

    #[test]
    fn root_metadata() {
        assert_eq!(ROOT.path, &[] as &[&str]);
        assert_eq!(ROOT.label, "crates/core2/src/lib.anv");
        assert!(ROOT.code.contains("pub import option { * };"));
        assert!(ROOT.code.contains("pub import range { * };"));
        assert!(!ROOT.code.contains("int"));
        assert!(!ROOT.code.contains("float"));
        assert!(!ROOT.code.contains("string"));
    }

    #[test]
    fn module_metadata() {
        assert_eq!(
            paths(MODULES),
            vec![
                &["option"][..],
                &["range"],
                &["int"],
                &["float"],
                &["string"]
            ]
        );
        for module in MODULES {
            assert!(module.label.starts_with("crates/core2/src/"));
            assert!(module.label.ends_with(".anv"));
            assert!(!module.path.is_empty());
        }
    }

    #[test]
    fn always_active_modules() {
        assert_eq!(ALWAYS_ACTIVE, &[&["int"][..], &["float"], &["string"]]);
        for path in ALWAYS_ACTIVE {
            assert!(MODULES.iter().any(|module| module.path == *path));
        }
    }

    #[test]
    fn excludes_legacy_double_surface() {
        let mut code = String::new();
        code.push_str(ROOT.code);
        for module in MODULES {
            code.push_str(module.code);
        }

        assert!(!code.contains("double"));
        assert!(!code.contains("PI_D"));
        assert!(!code.contains("EPSILON_D"));
    }

    #[test]
    fn extension_helpers_are_not_reexported_by_root() {
        assert!(!ROOT.code.contains("pub import int"));
        assert!(!ROOT.code.contains("pub import float"));
        assert!(!ROOT.code.contains("pub import string"));
    }

    #[test]
    fn extension_modules_use_private_provider_imports() {
        let code = MODULES
            .iter()
            .filter(|module| matches!(module.path, ["int"] | ["float"] | ["string"]))
            .map(|module| module.code)
            .collect::<Vec<_>>()
            .join("\n");

        assert!(code.contains("import ext:int"));
        assert!(code.contains("import ext:float"));
        assert!(code.contains("import ext:string"));
        assert!(!code.contains(&["extern fn ", "int_"].concat()));
        assert!(!code.contains(&["extern fn ", "float_"].concat()));
        assert!(!code.contains(&["extern fn ", "str_"].concat()));
    }

    #[test]
    fn provider_metadata() {
        let providers = provider_descriptors();
        let modules = providers
            .iter()
            .map(|provider| provider.modules[0].path.segments.as_slice())
            .collect::<Vec<_>>();

        assert_eq!(modules, [&["int"][..], &["float"], &["string"]]);
        assert_eq!(providers[0].modules[0].functions[0].name, "int_abs");
    }
}
