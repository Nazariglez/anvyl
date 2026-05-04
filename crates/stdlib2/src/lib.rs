pub struct SourceFile {
    pub path: &'static [&'static str],
    pub label: &'static str,
    pub code: &'static str,
}

pub const ROOT: SourceFile = SourceFile {
    path: &[],
    label: "crates/stdlib2/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[SourceFile {
    path: &["mem"],
    label: "crates/stdlib2/src/mem.anv",
    code: include_str!("mem.anv"),
}];

pub fn provider_descriptors() -> Vec<anvyx_externs::ProviderDescriptor> {
    vec![anvyx_lang::provider_descriptor!(
        provider = "stdlib_mem",
        module = "mem",
        fn collect_cycles() -> void;,
        fn auto_collect(enabled: bool) -> void;,
        fn managed_count() -> int;
    )]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn root_metadata() {
        assert_eq!(ROOT.path, &[] as &[&str]);
        assert_eq!(ROOT.label, "crates/stdlib2/src/lib.anv");
        assert_eq!(ROOT.code.trim(), "pub import mem;");
    }

    #[test]
    fn module_metadata() {
        assert_eq!(MODULES.len(), 1);
        let module = &MODULES[0];

        assert_eq!(module.path, &["mem"]);
        assert_eq!(module.label, "crates/stdlib2/src/mem.anv");
        assert!(module.label.starts_with("crates/stdlib2/src/"));
        assert!(module.label.ends_with(".anv"));
    }

    #[test]
    fn mem_source_reexports_private_provider_members() {
        let code = MODULES[0].code;

        assert!(code.contains("pub import ext:mem"));
        assert!(!code.contains("extern fn "));
        assert!(!code.contains("pub fn "));
    }

    #[test]
    fn provider_metadata() {
        let providers = provider_descriptors();
        let module = &providers[0].modules[0];

        assert_eq!(module.path.segments, ["mem"]);
        assert_eq!(module.functions.len(), 3);
    }

    #[test]
    fn module_paths_are_package_local() {
        assert!(
            !MODULES
                .iter()
                .any(|module| module.path.starts_with(&["std"]))
        );
        assert!(!ROOT.code.contains("std:"));
    }

    #[test]
    fn excludes_unported_legacy_std_modules() {
        let code = MODULES[0].code;

        assert!(!code.contains("math"));
        assert!(!code.contains("maps"));
        assert!(!code.contains("time"));
        assert!(!code.contains("linalg"));
    }
}
