use std::{
    ffi::OsString,
    path::{Path, PathBuf},
};

pub(crate) fn rust_cache_root_for(project_root: &Path) -> PathBuf {
    rust_cache_root_from_env(project_root, std::env::var_os("ANVYX_CACHE_DIR"))
}

pub(crate) fn default_rust_cache_root() -> PathBuf {
    let root = std::env::current_dir().unwrap_or_else(|_| std::env::temp_dir());
    rust_cache_root_for(&root)
}

fn rust_cache_root_from_env(project_root: &Path, override_root: Option<OsString>) -> PathBuf {
    override_root.map_or_else(
        || project_root.join(".anvyx").join("cache").join("rust"),
        PathBuf::from,
    )
}
