use std::{
    fs,
    path::{Path, PathBuf},
    process,
};

use anvyx_project::rust::clean_artifact_name;

use crate::manifest::Manifest;

const ANVYX_CRATE_DIR: &str = env!("CARGO_MANIFEST_DIR");
const RUNTIME_INIT: &str = include_str!("../templates/runtime_init.txt");
const BUILD_TEMPLATE: &str = include_str!("../templates/build_main.txt");

fn sibling_crate_path(name: &str) -> String {
    Path::new(ANVYX_CRATE_DIR)
        .parent()
        .unwrap()
        .join(name)
        .to_string_lossy()
        .into_owned()
}

fn generate_crate(project_root: &Path, main_rs: &str) -> Result<PathBuf, String> {
    let runner_dir = project_root.join("build/runner");
    let src_dir = runner_dir.join("src");

    fs::create_dir_all(&src_dir)
        .map_err(|e| format!("Failed to create runner crate directory: {e}"))?;

    let cargo_toml = generate_cargo_toml();
    fs::write(runner_dir.join("Cargo.toml"), cargo_toml)
        .map_err(|e| format!("Failed to write runner Cargo.toml: {e}"))?;

    fs::write(src_dir.join("main.rs"), main_rs)
        .map_err(|e| format!("Failed to write runner src/main.rs: {e}"))?;

    Ok(runner_dir)
}

pub fn generate_build_runner_crate(
    project_root: &Path,
    manifest: &Manifest,
    release: bool,
) -> Result<PathBuf, String> {
    generate_crate(project_root, &generate_build_main_rs(manifest, release))
}

pub fn runner_binary_path(project_root: &Path) -> PathBuf {
    project_root.join("build/runner/target/release/anvyx-runner")
}

pub fn build_runner(runner_dir: &Path) -> Result<(), String> {
    let manifest_path = runner_dir.join("Cargo.toml");
    let target_dir = runner_dir.join("target");
    let output = process::Command::new("cargo")
        .args(["build", "--release", "--manifest-path"])
        .arg(&manifest_path)
        .arg("--target-dir")
        .arg(&target_dir)
        // unset CARGO_TARGET_DIR so the explicit --target-dir takes effect.
        .env_remove("CARGO_TARGET_DIR")
        .output()
        .map_err(|e| format!("Failed to run cargo build: {e}"))?;

    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("Failed to build runner crate:\n{stderr}"))
    }
}

fn generate_cargo_toml() -> String {
    let lang_path = sibling_crate_path("lang");
    let std_path = sibling_crate_path("std");
    let core_path = sibling_crate_path("core");

    format!(
        "[package]\nname = \"anvyx-runner\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[workspace]\n\n[dependencies]\nanvyx-lang = {{ path = \"{lang_path}\" }}\nanvyx-std = {{ path = \"{std_path}\" }}\nanvyx-core = {{ path = \"{core_path}\" }}\n"
    )
}

fn generate_build_main_rs(manifest: &Manifest, release: bool) -> String {
    let profile_expr = if release {
        "anvyx_lang::Profile::Release"
    } else {
        "anvyx_lang::Profile::Debug"
    };
    let entry = manifest
        .project
        .entry
        .as_deref()
        .expect("build requires project.entry");
    BUILD_TEMPLATE
        .replace("%ENTRY_POINT%", entry)
        .replace("%RUNTIME_INIT%", RUNTIME_INIT)
        .replace("%PROFILE%", profile_expr)
}

pub fn resolve_project_name(manifest: &Manifest, project_root: &Path) -> String {
    let raw = manifest.project.name.as_deref().unwrap_or_else(|| {
        project_root
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("anvyx-project")
    });
    clean_artifact_name(raw)
}

pub fn assemble_dist(project_root: &Path, project_name: &str) -> Result<PathBuf, String> {
    let dist_dir = project_root.join("build/dist");

    if dist_dir.exists() {
        fs::remove_dir_all(&dist_dir)
            .map_err(|e| format!("Failed to clean previous dist directory: {e}"))?;
    }

    fs::create_dir_all(&dist_dir).map_err(|e| format!("Failed to create dist directory: {e}"))?;

    let src_binary = runner_binary_path(project_root);
    if !src_binary.exists() {
        return Err(format!(
            "Runner binary not found at {}",
            src_binary.display()
        ));
    }

    let dest_binary = dist_dir.join(project_name);
    fs::copy(&src_binary, &dest_binary)
        .map_err(|e| format!("Failed to copy binary to dist: {e}"))?;

    Ok(dist_dir)
}

pub fn bundle_sources(
    project_root: &Path,
    dist_dir: &Path,
    manifest: &Manifest,
) -> Result<(), String> {
    walk_and_copy_anv(project_root, project_root, dist_dir)?;

    let Some(entry) = manifest.project.entry.as_deref() else {
        return Err("project.entry is required to bundle sources".to_string());
    };
    let entry_in_dist = dist_dir.join(entry);
    if !entry_in_dist.exists() {
        return Err(format!(
            "Entry point '{entry}' not found in bundled sources. \
            Check project.entry in anvyx.toml."
        ));
    }

    Ok(())
}

fn walk_and_copy_anv(dir: &Path, project_root: &Path, dist_dir: &Path) -> Result<(), String> {
    let read_dir = fs::read_dir(dir)
        .map_err(|e| format!("Failed to read directory {}: {e}", dir.display()))?;

    for entry in read_dir {
        let entry = entry.map_err(|e| format!("Failed to read directory entry: {e}"))?;
        let path = entry.path();

        let rel = path
            .strip_prefix(project_root)
            .map_err(|_| format!("Path {} is not under project root", path.display()))?;

        let file_type = entry
            .file_type()
            .map_err(|e| format!("Failed to get file type for {}: {e}", path.display()))?;

        if file_type.is_dir() {
            let dir_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

            let is_hidden = dir_name.starts_with('.');
            let is_build = rel == Path::new("build");

            if !is_hidden && !is_build {
                walk_and_copy_anv(&path, project_root, dist_dir)?;
            }
        } else if file_type.is_file() {
            let is_anv = path.extension().and_then(|e| e.to_str()) == Some("anv");
            if is_anv {
                let dest = dist_dir.join(rel);
                if let Some(parent) = dest.parent() {
                    fs::create_dir_all(parent).map_err(|e| {
                        format!("Failed to create directory {}: {e}", parent.display())
                    })?;
                }
                fs::copy(&path, &dest).map_err(|e| {
                    format!(
                        "Failed to copy {} to {}: {e}",
                        path.display(),
                        dest.display()
                    )
                })?;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

    use super::*;
    use crate::manifest::{Manifest, Project};

    fn test_manifest(name: Option<&str>, entry: &str) -> Manifest {
        Manifest {
            project: Project {
                name: name.map(str::to_string),
                version: None,
                entry: Some(entry.into()),
            },
            dependencies: HashMap::new(),
            lint: BTreeMap::default(),
        }
    }

    fn manifest() -> Manifest {
        test_manifest(None, "src/main.anv")
    }

    #[test]
    fn cargo_toml_contains_runner_dependencies() {
        let output = generate_cargo_toml();

        assert!(output.contains("[package]"));
        assert!(output.contains("name = \"anvyx-runner\""));
        assert!(output.contains("[workspace]"));
        assert!(output.contains("anvyx-lang"));
        assert!(output.contains("anvyx-std"));
        assert!(output.contains("anvyx-core"));
    }

    #[test]
    fn build_main_rs_hardcodes_entry_and_profile() {
        let output = generate_build_main_rs(&test_manifest(None, "game/start.anv"), true);

        assert!(output.contains("const ENTRY_POINT: &str = \"game/start.anv\";"));
        assert!(output.contains("anvyx_lang::Profile::Release"));
        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("anvyx_std::std_modules()"));
    }

    #[test]
    fn generate_build_runner_crate_creates_files() {
        let tmp = std::env::temp_dir().join(format!("anvyx-build-runner-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let runner_dir = generate_build_runner_crate(&tmp, &manifest(), false).unwrap();

        assert!(runner_dir.join("Cargo.toml").exists());
        assert!(runner_dir.join("src/main.rs").exists());
        assert!(
            fs::read_to_string(runner_dir.join("Cargo.toml"))
                .unwrap()
                .contains("anvyx-runner")
        );
        assert!(
            fs::read_to_string(runner_dir.join("src/main.rs"))
                .unwrap()
                .contains("src/main.anv")
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn resolve_project_name_from_manifest() {
        assert_eq!(
            resolve_project_name(
                &test_manifest(Some("my_game"), "src/main.anv"),
                Path::new("/tmp/game")
            ),
            "my_game"
        );
    }

    #[test]
    fn resolve_project_name_sanitizes() {
        assert_eq!(
            resolve_project_name(
                &test_manifest(Some("My Cool Game!"), "src/main.anv"),
                Path::new("/tmp/game")
            ),
            "my-cool-game"
        );
    }

    #[test]
    fn assemble_dist_copies_binary() {
        let tmp = std::env::temp_dir().join(format!("anvyx-dist-copy-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        let binary_dir = tmp.join("build/runner/target/release");
        fs::create_dir_all(&binary_dir).unwrap();
        fs::write(binary_dir.join("anvyx-runner"), b"fake binary content").unwrap();

        let dist_dir = assemble_dist(&tmp, "my_game").unwrap();

        assert_eq!(dist_dir, tmp.join("build/dist"));
        assert_eq!(
            fs::read(dist_dir.join("my_game")).unwrap(),
            b"fake binary content"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn assemble_dist_missing_binary_errors() {
        let tmp = std::env::temp_dir().join(format!("anvyx-dist-missing-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let error = assemble_dist(&tmp, "my_game").unwrap_err();

        assert!(error.contains("not found"));
        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_copies_anv_files() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-copy-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src/utils")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::write(tmp.join("src/utils/helpers.anv"), b"fn helper() {}").unwrap();
        fs::write(tmp.join("src/ignore.txt"), b"not anv").unwrap();
        let dist = tmp.join("build/dist");
        fs::create_dir_all(&dist).unwrap();

        bundle_sources(&tmp, &dist, &manifest()).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert!(dist.join("src/utils/helpers.anv").exists());
        assert!(!dist.join("src/ignore.txt").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_skips_build_and_hidden_dirs() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-skip-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::create_dir_all(tmp.join("build/cache")).unwrap();
        fs::create_dir_all(tmp.join(".git")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::write(tmp.join("build/cache/generated.anv"), b"generated").unwrap();
        fs::write(tmp.join(".git/hidden.anv"), b"hidden").unwrap();
        let dist = tmp.join("build/dist");
        fs::create_dir_all(&dist).unwrap();

        bundle_sources(&tmp, &dist, &manifest()).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert!(!dist.join("build/cache/generated.anv").exists());
        assert!(!dist.join(".git/hidden.anv").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_validates_entry_point() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-missing-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(tmp.join("src")).unwrap();
        let dist = tmp.join("build/dist");
        fs::create_dir_all(&dist).unwrap();

        let error = bundle_sources(&tmp, &dist, &manifest()).unwrap_err();

        assert!(error.contains("Entry point"));
        let _ = fs::remove_dir_all(&tmp);
    }
}
