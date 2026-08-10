pub mod core_char;
pub mod core_float;
pub mod core_int;
pub mod core_runtime;
pub mod core_string;

pub struct SourceFile {
    pub path: &'static [&'static str],
    pub label: &'static str,
    pub code: &'static str,
}

pub const ROOT: SourceFile = SourceFile {
    path: &[],
    label: "crates/core/src/lib.anv",
    code: include_str!("lib.anv"),
};

pub const MODULES: &[SourceFile] = &[
    SourceFile {
        path: &["option"],
        label: "crates/core/src/option.anv",
        code: include_str!("option.anv"),
    },
    SourceFile {
        path: &["result"],
        label: "crates/core/src/result.anv",
        code: include_str!("result.anv"),
    },
    SourceFile {
        path: &["range"],
        label: "crates/core/src/range.anv",
        code: include_str!("range.anv"),
    },
    SourceFile {
        path: &["collections"],
        label: "crates/core/src/collections.anv",
        code: include_str!("collections.anv"),
    },
    SourceFile {
        path: &["flags"],
        label: "crates/core/src/flags.anv",
        code: include_str!("flags.anv"),
    },
    SourceFile {
        path: &["runtime"],
        label: "crates/core/src/runtime.anv",
        code: include_str!("runtime.anv"),
    },
    SourceFile {
        path: &["core_int"],
        label: "crates/core/src/core_int.anv",
        code: include_str!("core_int.anv"),
    },
    SourceFile {
        path: &["core_float"],
        label: "crates/core/src/core_float.anv",
        code: include_str!("core_float.anv"),
    },
    SourceFile {
        path: &["core_string"],
        label: "crates/core/src/core_string.anv",
        code: include_str!("core_string.anv"),
    },
    SourceFile {
        path: &["core_char"],
        label: "crates/core/src/core_char.anv",
        code: include_str!("core_char.anv"),
    },
];

anvyx_runtime::provider_package! {
    modules: [core_int, core_float, core_string, core_char, core_runtime]
}
