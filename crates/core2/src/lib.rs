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
        path: &["result"],
        label: "crates/core2/src/result.anv",
        code: include_str!("result.anv"),
    },
    SourceFile {
        path: &["range"],
        label: "crates/core2/src/range.anv",
        code: include_str!("range.anv"),
    },
    SourceFile {
        path: &["collections"],
        label: "crates/core2/src/collections.anv",
        code: include_str!("collections.anv"),
    },
    SourceFile {
        path: &["runtime"],
        label: "crates/core2/src/runtime.anv",
        code: include_str!("runtime.anv"),
    },
    SourceFile {
        path: &["core_int"],
        label: "crates/core2/src/core_int.anv",
        code: include_str!("core_int.anv"),
    },
    SourceFile {
        path: &["core_float"],
        label: "crates/core2/src/core_float.anv",
        code: include_str!("core_float.anv"),
    },
    SourceFile {
        path: &["core_string"],
        label: "crates/core2/src/core_string.anv",
        code: include_str!("core_string.anv"),
    },
];

pub fn provider_descriptors() -> Vec<anvyx_externs::ProviderDescriptor> {
    vec![
        int_provider(),
        float_provider(),
        string_provider(),
        runtime_provider(),
    ]
}

fn int_provider() -> anvyx_externs::ProviderDescriptor {
    anvyx_lang::provider_descriptor!(
        provider = "core_int",
        module = "core_int",
        fn int_abs(x: int) -> int;,
        fn int_min(a: int, b: int) -> int;,
        fn int_max(a: int, b: int) -> int;,
        fn int_clamp(x: int, lo: int, hi: int) -> int;
    )
}

fn float_provider() -> anvyx_externs::ProviderDescriptor {
    anvyx_lang::provider_descriptor!(
        provider = "core_float",
        module = "core_float",
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
        module = "core_string",
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

fn runtime_provider() -> anvyx_externs::ProviderDescriptor {
    let mut provider = anvyx_lang::provider_descriptor!(
        provider = "core_runtime",
        module = "core_runtime",
        fn _println(message: string) -> void;,
        fn _assert(condition: bool, message: string) -> void;
    );
    provider.modules[0].functions[1].effects.fallible = true;
    provider
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
        assert!(ROOT.code.contains("pub import core_int { * };"));
        assert!(ROOT.code.contains("pub import core_float { * };"));
        assert!(ROOT.code.contains("pub import core_string { * };"));
        assert!(ROOT.code.contains("pub import runtime { * };"));
        assert!(ROOT.code.contains("pub import option { * };"));
        assert!(ROOT.code.contains("pub import result { * };"));
        assert!(ROOT.code.contains("pub import range { * };"));
        assert!(ROOT.code.contains("pub import collections { * };"));
        assert!(!ROOT.code.contains("pub import core_int;"));
        assert!(!ROOT.code.contains("pub import core_float;"));
        assert!(!ROOT.code.contains("pub import core_string;"));
    }

    #[test]
    fn module_metadata() {
        assert_eq!(
            paths(MODULES),
            vec![
                &["option"][..],
                &["result"],
                &["range"],
                &["collections"],
                &["runtime"],
                &["core_int"],
                &["core_float"],
                &["core_string"]
            ]
        );
        for module in MODULES {
            assert!(module.label.starts_with("crates/core2/src/"));
            assert_eq!(
                std::path::Path::new(module.label)
                    .extension()
                    .and_then(std::ffi::OsStr::to_str),
                Some("anv")
            );
            assert!(!module.path.is_empty());
        }
    }

    #[test]
    fn extension_helpers_are_not_module_reexported_by_root() {
        assert!(!ROOT.code.contains("pub import core_int;"));
        assert!(!ROOT.code.contains("pub import core_float;"));
        assert!(!ROOT.code.contains("pub import core_string;"));
    }

    #[test]
    fn extension_modules_use_private_provider_imports() {
        let code = MODULES
            .iter()
            .filter(|module| {
                matches!(
                    module.path,
                    ["core_int" | "core_float" | "core_string" | "runtime"]
                )
            })
            .map(|module| module.code)
            .collect::<Vec<_>>()
            .join("\n");

        assert!(code.contains("import ext:core_int"));
        assert!(code.contains("import ext:core_float"));
        assert!(code.contains("import ext:core_string"));
        assert!(code.contains("import ext:core_runtime"));
        assert!(!code.contains("import ext:int"));
        assert!(!code.contains("import ext:float"));
        assert!(!code.contains("import ext:string"));
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

        assert_eq!(
            modules,
            [
                &["core_int"][..],
                &["core_float"],
                &["core_string"],
                &["core_runtime"]
            ]
        );
        assert_eq!(providers[0].modules[0].functions[0].name, "int_abs");
        let runtime = &providers[3].modules[0].functions;
        assert_eq!(runtime[0].name, "_println");
        assert_eq!(
            runtime[0].signature.params[0].ty,
            anvyx_externs::ExternTypeExpr::String
        );
        assert_eq!(
            runtime[0].signature.ret,
            anvyx_externs::ExternTypeExpr::Void
        );
        assert!(!runtime[0].effects.fallible);

        assert_eq!(runtime[1].name, "_assert");
        assert_eq!(
            runtime[1].signature.params[0].ty,
            anvyx_externs::ExternTypeExpr::Bool
        );
        assert_eq!(
            runtime[1].signature.params[1].ty,
            anvyx_externs::ExternTypeExpr::String
        );
        assert_eq!(
            runtime[1].signature.ret,
            anvyx_externs::ExternTypeExpr::Void
        );
        assert!(runtime[1].effects.fallible);
    }

    #[test]
    fn providers_validate() {
        for provider in provider_descriptors() {
            assert_eq!(anvyx_externs::validate(&provider), Ok(()));
        }
    }
}
