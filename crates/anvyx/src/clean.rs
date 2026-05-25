use std::fs;

pub fn cmd() -> Result<(), String> {
    let cache_dir = std::env::current_dir()
        .map_err(|e| format!("Failed to get current directory: {e}"))?
        .join(".anvyx")
        .join("cache");

    if !cache_dir.exists() {
        crate::progress::status("Clean", "nothing to clean");
        return Ok(());
    }

    fs::remove_dir_all(&cache_dir)
        .map_err(|e| format!("Failed to remove cache {}: {e}", cache_dir.display()))?;
    crate::progress::status("Cleaned", ".anvyx/cache/");
    Ok(())
}
