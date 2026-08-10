pub mod mem;

pub struct SourceFile {
    pub path: &'static [&'static str],
    pub label: &'static str,
    pub code: &'static str,
}

pub const ROOT: SourceFile = SourceFile {
    path: &[],
    label: "crates/stdlib/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[SourceFile {
    path: &["mem"],
    label: "crates/stdlib/src/mem.anv",
    code: include_str!("mem.anv"),
}];

anvyx_runtime::provider_package! { modules: [mem] }
