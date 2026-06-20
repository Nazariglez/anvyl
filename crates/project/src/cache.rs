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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rust_cache_root_defaults_under_project_root() {
        assert_eq!(
            rust_cache_root_from_env(Path::new("/game"), None),
            PathBuf::from("/game/.anvyx/cache/rust")
        );
    }

    #[test]
    fn rust_cache_root_uses_override_directly() {
        assert_eq!(
            rust_cache_root_from_env(Path::new("/game"), Some(OsString::from("/cache"))),
            PathBuf::from("/cache")
        );
    }

    #[test]
    fn rust_cache_root_preserves_relative_override() {
        assert_eq!(
            rust_cache_root_from_env(Path::new("/game"), Some(OsString::from("cache"))),
            PathBuf::from("cache")
        );
    }

    #[test]
    fn rust_cache_root_preserves_empty_override() {
        assert_eq!(
            rust_cache_root_from_env(Path::new("/game"), Some(OsString::from(""))),
            PathBuf::from("")
        );
    }
}
