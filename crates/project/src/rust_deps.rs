use std::path::{Path, PathBuf};

use anvyx_backend::rust::cargo_job::{
    RustCargoDependency, RustCargoDependencySource, RustCargoName, RustCargoPackageName,
};

pub(crate) fn workspace_crate_path(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("project crate lives below workspace crates directory")
        .join(name)
}

pub(crate) fn runtime_dependency() -> RustCargoDependency {
    RustCargoDependency {
        name: RustCargoName::parse("anvyx_runtime").expect("valid runtime crate name"),
        package: Some(
            RustCargoPackageName::parse("anvyx-runtime").expect("valid runtime package name"),
        ),
        source: RustCargoDependencySource::Path(
            workspace_crate_path("runtime").display().to_string(),
        ),
        features: vec![],
        default_features: true,
    }
}
