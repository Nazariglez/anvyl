use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
    process,
};

use anvyx_lang::LintConfig;

use crate::manifest::Manifest;

const ANVYX_CRATE_DIR: &str = env!("CARGO_MANIFEST_DIR");
const RUNTIME_INIT: &str = include_str!("../templates/runtime_init.txt");
const RUNNER_TEMPLATE: &str = include_str!("../templates/runner_main.txt");
const BUILD_TEMPLATE: &str = include_str!("../templates/build_main.txt");

fn sibling_crate_path(name: &str) -> String {
    Path::new(ANVYX_CRATE_DIR)
        .parent()
        .unwrap()
        .join(name)
        .to_string_lossy()
        .into_owned()
}

fn generate_crate(
    project_root: &Path,
    manifest: &Manifest,
    main_rs: &str,
) -> Result<PathBuf, String> {
    for (name, entry) in &manifest.externs {
        let resolved = project_root.join(&entry.path);
        if !resolved.exists() {
            return Err(format!(
                "Extern provider '{name}' not found at path: {}",
                resolved.display()
            ));
        }
    }

    let runner_dir = project_root.join("build/runner");
    let src_dir = runner_dir.join("src");

    fs::create_dir_all(&src_dir)
        .map_err(|e| format!("Failed to create runner crate directory: {e}"))?;

    let cargo_toml = generate_cargo_toml(project_root, manifest);
    fs::write(runner_dir.join("Cargo.toml"), cargo_toml)
        .map_err(|e| format!("Failed to write runner Cargo.toml: {e}"))?;

    fs::write(src_dir.join("main.rs"), main_rs)
        .map_err(|e| format!("Failed to write runner src/main.rs: {e}"))?;

    Ok(runner_dir)
}

pub fn generate_runner_crate(project_root: &Path, manifest: &Manifest) -> Result<PathBuf, String> {
    generate_crate(project_root, manifest, &generate_main_rs(manifest))
}

pub fn generate_build_runner_crate(
    project_root: &Path,
    manifest: &Manifest,
    release: bool,
) -> Result<PathBuf, String> {
    generate_crate(
        project_root,
        manifest,
        &generate_build_main_rs(manifest, release),
    )
}

pub fn runner_binary_path(project_root: &Path) -> PathBuf {
    project_root.join("build/runner/target/release/anvyx-runner")
}

pub fn execute_runner(
    project_root: &Path,
    entry_path: &Path,
    backend: &str,
    release: bool,
    lint_config: LintConfig,
    features: &[String],
    cfgs: &[String],
) -> Result<(), String> {
    let binary = runner_binary_path(project_root);
    if !binary.exists() {
        return Err(format!("Runner binary not found at {}", binary.display()));
    }

    let metadata_dir = project_root.join("build/metadata");

    let mut cmd = process::Command::new(&binary);
    cmd.arg(entry_path).arg(backend).arg(&metadata_dir);
    if release {
        cmd.arg("--release");
    }
    if lint_config.internal_access != anvyx_lang::LintLevel::Warn {
        cmd.arg("--lint")
            .arg(format!("internal_access={}", lint_config.internal_access));
    }
    for feat in features {
        cmd.args(["--feature", feat]);
    }
    for cfg_val in cfgs {
        cmd.args(["--cfg", cfg_val]);
    }
    let status = cmd
        .status()
        .map_err(|e| format!("Failed to execute runner binary: {e}"))?;

    if status.success() {
        Ok(())
    } else {
        let code = status.code().unwrap_or(1);
        Err(format!("Runner exited with code {code}"))
    }
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
        Err(format_build_error(&stderr))
    }
}

pub fn extract_metadata(project_root: &Path) -> Result<(), String> {
    let binary = runner_binary_path(project_root);
    if !binary.exists() {
        return Err(format!("Runner binary not found at {}", binary.display()));
    }

    let output_dir = project_root.join("build/metadata");

    let status = process::Command::new(&binary)
        .arg("--metadata")
        .arg(&output_dir)
        .status()
        .map_err(|e| format!("Failed to run metadata extraction: {e}"))?;

    if status.success() {
        Ok(())
    } else {
        let code = status.code().unwrap_or(1);
        Err(format!(
            "Metadata extraction failed (exit code {code}). \
            Check that extern provider crates compile correctly and export anvyx_exports()/anvyx_type_exports()."
        ))
    }
}

fn format_build_error(stderr: &str) -> String {
    let mut msg = format!("Failed to build runner crate:\n{stderr}");
    if stderr.contains("anvyx_externs") {
        msg.push_str(
            "\nHint: each extern provider must export a \
            `pub fn anvyx_externs() -> HashMap<String, ExternHandler>` function",
        );
    }
    msg
}

fn generate_cargo_toml(project_root: &Path, manifest: &Manifest) -> String {
    // TODO: use `anvyx-lang = "x.y.z"` once published to crates.io
    let lang_path = sibling_crate_path("lang");
    let std_path = sibling_crate_path("std");
    let core_path = sibling_crate_path("core");

    let mut deps = format!(
        "anvyx-lang = {{ path = \"{lang_path}\" }}\nanvyx-std = {{ path = \"{std_path}\" }}\nanvyx-core = {{ path = \"{core_path}\" }}\n"
    );

    let mut entries: Vec<(&String, &crate::manifest::ExternEntry)> =
        manifest.externs.iter().collect();
    entries.sort_by_key(|(name, _)| *name);

    for (name, entry) in entries {
        let extern_path = resolve_relative_path(project_root, &entry.path);
        let _ = writeln!(deps, "{name} = {{ path = \"{extern_path}\" }}");
    }

    format!(
        "[package]\nname = \"anvyx-runner\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[workspace]\n\n[dependencies]\n{deps}"
    )
}

fn sorted_extern_names(manifest: &Manifest) -> Vec<&String> {
    let mut names: Vec<&String> = manifest.externs.keys().collect();
    names.sort();
    names
}

fn generate_extern_extends(names: &[&String]) -> String {
    names
        .iter()
        .map(|n| format!("    externs.extend({n}::anvyx_externs());"))
        .collect::<Vec<_>>()
        .join("\n")
}

fn generate_main_rs(manifest: &Manifest) -> String {
    let entries = sorted_extern_names(manifest);
    RUNNER_TEMPLATE
        .replace("%RUNTIME_INIT%", RUNTIME_INIT)
        .replace("%METADATA_WRITE%", &generate_metadata_lines(&entries))
        .replace("%METADATA_READ%", &generate_metadata_read_lines(&entries))
        .replace("%EXTERN_EXTENDS%", &generate_extern_extends(&entries))
}

fn generate_metadata_lines(sorted_names: &[&String]) -> String {
    let mut lines = String::new();
    for name in sorted_names {
        let _ = write!(
            lines,
            "        {{\n\
             \x20           let json = anvyx_lang::exports_to_json(&{name}::anvyx_exports(), &{name}::anvyx_type_exports());\n\
             \x20           fs::write(format!(\"{{output_dir}}/{name}.json\"), json)\n\
             \x20               .unwrap_or_else(|e| {{ eprintln!(\"Failed to write metadata for '{name}': {{e}}\"); std::process::exit(1); }});\n\
             \x20       }}\n"
        );
    }
    lines
}

fn generate_metadata_read_lines(sorted_names: &[&String]) -> String {
    let mut lines = String::new();
    for name in sorted_names {
        let _ = write!(
            lines,
            "    {{\n\
             \x20       let json = fs::read_to_string(format!(\"{{metadata_dir}}/{name}.json\"))\n\
             \x20           .unwrap_or_else(|e| {{ eprintln!(\"Failed to read metadata for '{name}': {{e}}\"); std::process::exit(1); }});\n\
             \x20       extern_metadata.insert(\"{name}\".to_string(), json);\n\
             \x20   }}\n"
        );
    }
    lines
}

fn generate_build_main_rs(manifest: &Manifest, release: bool) -> String {
    let profile_expr = if release {
        "anvyx_lang::Profile::Release"
    } else {
        "anvyx_lang::Profile::Debug"
    };
    let entries = sorted_extern_names(manifest);
    let entry = manifest
        .project
        .entry
        .as_deref()
        .expect("build requires project.entry");
    BUILD_TEMPLATE
        .replace("%ENTRY_POINT%", entry)
        .replace("%RUNTIME_INIT%", RUNTIME_INIT)
        .replace(
            "%METADATA_CONSTS%",
            &generate_build_metadata_consts(&entries),
        )
        .replace(
            "%METADATA_INSERTS%",
            &generate_build_metadata_inserts(&entries),
        )
        .replace("%EXTERN_EXTENDS%", &generate_extern_extends(&entries))
        .replace("%PROFILE%", profile_expr)
}

fn generate_build_metadata_consts(sorted_names: &[&String]) -> String {
    let mut lines = String::new();
    for name in sorted_names {
        let const_name = format!("META_{}", name.to_uppercase());
        let _ = writeln!(
            lines,
            "const {const_name}: &str = include_str!(\"../../metadata/{name}.json\");"
        );
    }
    lines
}

fn generate_build_metadata_inserts(sorted_names: &[&String]) -> String {
    let mut lines = String::new();
    for name in sorted_names {
        let const_name = format!("META_{}", name.to_uppercase());
        let _ = writeln!(
            lines,
            "    extern_metadata.insert(\"{name}\".to_string(), {const_name}.to_string());"
        );
    }
    lines
}

pub fn read_metadata(
    project_root: &Path,
    manifest: &Manifest,
) -> Result<std::collections::HashMap<String, String>, String> {
    let metadata_dir = project_root.join("build/metadata");
    let mut result = std::collections::HashMap::new();
    for name in manifest.externs.keys() {
        let path = metadata_dir.join(format!("{name}.json"));
        let json = fs::read_to_string(&path)
            .map_err(|e| format!("Failed to read metadata for extern '{name}': {e}"))?;
        result.insert(name.clone(), json);
    }
    Ok(result)
}

pub fn resolve_project_name(manifest: &Manifest, project_root: &Path) -> String {
    let raw = match &manifest.project.name {
        Some(name) => name.clone(),
        None => project_root
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("anvyx-project")
            .to_string(),
    };

    raw.to_lowercase()
        .chars()
        .map(|c| if c == ' ' { '-' } else { c })
        .filter(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_')
        .collect()
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
    use std::collections::HashSet;

    let mut skip_dirs: HashSet<PathBuf> = HashSet::new();
    skip_dirs.insert(PathBuf::from("build"));
    for entry in manifest.externs.values() {
        let normalized = Path::new(&entry.path).components().collect::<PathBuf>();
        skip_dirs.insert(normalized);
    }

    walk_and_copy_anv(project_root, project_root, dist_dir, &skip_dirs)?;

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

fn walk_and_copy_anv(
    dir: &Path,
    project_root: &Path,
    dist_dir: &Path,
    skip_dirs: &std::collections::HashSet<PathBuf>,
) -> Result<(), String> {
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
            let is_skipped = skip_dirs.contains(rel);

            if !is_hidden && !is_skipped {
                walk_and_copy_anv(&path, project_root, dist_dir, skip_dirs)?;
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

fn resolve_relative_path(project_root: &Path, rel_to_root: &str) -> String {
    let abs = project_root.join(rel_to_root);
    // build/runner/ is 2 levels below project_root, so prefix with ../../
    let from_runner = format!("../../{rel_to_root}");

    // if the absolute path exists, prefer it so Cargo can verify it; otherwise
    // fall back to the relative form (covers tests using temp dirs).
    if abs.exists() {
        abs.to_string_lossy().into_owned()
    } else {
        from_runner
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use anvyx_lang::LintConfig;

    use super::*;
    use crate::manifest::{ExternEntry, Manifest, Project};

    fn test_manifest(
        name: Option<&str>,
        entry: &str,
        externs: HashMap<String, ExternEntry>,
    ) -> Manifest {
        Manifest {
            project: Project {
                name: name.map(str::to_string),
                entry: Some(entry.into()),
            },
            dependencies: HashMap::new(),
            externs,
            lint: anvyx_lang::LintConfig::default(),
        }
    }

    fn manifest_no_externs() -> Manifest {
        test_manifest(None, "src/main.anv", HashMap::new())
    }

    fn manifest_one_extern() -> Manifest {
        let mut externs = HashMap::new();
        externs.insert(
            "engine".into(),
            ExternEntry {
                path: "my_externs/engine".into(),
            },
        );
        test_manifest(None, "src/main.anv", externs)
    }

    fn manifest_two_externs() -> Manifest {
        let mut externs = HashMap::new();
        externs.insert(
            "engine".into(),
            ExternEntry {
                path: "my_externs/engine".into(),
            },
        );
        externs.insert(
            "audio".into(),
            ExternEntry {
                path: "my_externs/audio".into(),
            },
        );
        test_manifest(None, "src/main.anv", externs)
    }

    #[test]
    fn cargo_toml_no_externs() {
        let root = Path::new("/fake/project");
        let output = generate_cargo_toml(root, &manifest_no_externs());

        assert!(output.contains("[package]"));
        assert!(output.contains("name = \"anvyx-runner\""));
        assert!(output.contains("[workspace]"));
        assert!(output.contains("anvyx-lang"));
        assert!(output.contains("anvyx-std"));
        assert!(!output.contains("engine"));
        assert!(!output.contains("audio"));
    }

    #[test]
    fn cargo_toml_one_extern() {
        let root = Path::new("/fake/project");
        let output = generate_cargo_toml(root, &manifest_one_extern());

        assert!(output.contains("anvyx-std"));
        assert!(output.contains("engine"));
        assert!(output.contains("my_externs/engine"));
    }

    #[test]
    fn cargo_toml_multiple_externs() {
        let root = Path::new("/fake/project");
        let output = generate_cargo_toml(root, &manifest_two_externs());

        assert!(output.contains("anvyx-std"));
        assert!(output.contains("engine"));
        assert!(output.contains("my_externs/engine"));
        assert!(output.contains("audio"));
        assert!(output.contains("my_externs/audio"));
    }

    #[test]
    fn main_rs_no_externs() {
        let output = generate_main_rs(&manifest_no_externs());

        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("anvyx_std::std_modules()"));
        assert!(output.contains("std_sources"));
    }

    #[test]
    fn main_rs_one_extern() {
        let output = generate_main_rs(&manifest_one_extern());

        assert!(output.contains("externs.extend(engine::anvyx_externs());"));
        assert!(output.contains("extern_metadata"));
        assert!(output.contains("metadata_dir"));
        assert!(output.contains("engine.json"));
        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("anvyx_std::std_modules()"));
    }

    #[test]
    fn main_rs_multiple_externs() {
        let output = generate_main_rs(&manifest_two_externs());

        assert!(output.contains("externs.extend(engine::anvyx_externs());"));
        assert!(output.contains("externs.extend(audio::anvyx_externs());"));
        assert!(output.contains("engine.json"));
        assert!(output.contains("audio.json"));
        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("anvyx_std::std_modules()"));
    }

    #[test]
    fn main_rs_passes_extern_metadata_to_run() {
        let output = generate_main_rs(&manifest_one_extern());

        assert!(output.contains("extern_metadata"));
        assert!(output.contains("run_program_with_std"));
        // the call should include both externs and extern_metadata
        let run_call = output.find("run_program_with_std").unwrap();
        let call_snippet = &output[run_call..run_call + 120];
        assert!(
            call_snippet.contains("extern_metadata"),
            "run call should pass extern_metadata"
        );
    }

    #[test]
    fn main_rs_reads_metadata_dir_from_arg() {
        let output = generate_main_rs(&manifest_one_extern());
        assert!(output.contains("args.get(3)"));
        assert!(output.contains("metadata_dir"));
    }

    #[test]
    fn generate_runner_crate_creates_files() {
        let tmp = std::env::temp_dir().join(format!("anvyx-test-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(tmp.join("my_externs/engine")).unwrap();

        let manifest = manifest_one_extern();
        let runner_dir = generate_runner_crate(&tmp, &manifest).unwrap();

        assert!(runner_dir.join("Cargo.toml").exists());
        assert!(runner_dir.join("src/main.rs").exists());

        let cargo_content = fs::read_to_string(runner_dir.join("Cargo.toml")).unwrap();
        assert!(cargo_content.contains("anvyx-runner"));
        assert!(cargo_content.contains("[workspace]"));
        assert!(cargo_content.contains("anvyx-std"));
        assert!(cargo_content.contains("engine"));

        let main_content = fs::read_to_string(runner_dir.join("src/main.rs")).unwrap();
        assert!(main_content.contains("run_program_with_std"));
        assert!(main_content.contains("anvyx_std::std_modules()"));
        assert!(main_content.contains("engine::anvyx_externs"));
        assert!(main_content.contains("extern_metadata"));
        assert!(main_content.contains("metadata_dir"));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn generate_runner_validates_extern_paths() {
        let tmp = std::env::temp_dir().join(format!("anvyx-validate-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let mut externs = HashMap::new();
        externs.insert(
            "missing_crate".into(),
            ExternEntry {
                path: "does_not_exist".into(),
            },
        );
        let manifest = test_manifest(None, "src/main.anv", externs);

        let result = generate_runner_crate(&tmp, &manifest);

        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(msg.contains("not found at path"), "unexpected error: {msg}");
        assert!(msg.contains("missing_crate"), "unexpected error: {msg}");

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn build_runner_invalid_crate_returns_error() {
        let tmp = std::env::temp_dir().join(format!("anvyx-build-err-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        // invalid cargo.toml, missing [package] section
        fs::write(tmp.join("Cargo.toml"), "[invalid_section]\nfoo = \"bar\"\n").unwrap();

        let result = build_runner(&tmp);

        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(
            msg.contains("Failed to build runner crate"),
            "unexpected error: {msg}"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn execute_runner_missing_binary_returns_error() {
        let tmp = std::env::temp_dir().join(format!("anvyx-exec-err-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let result = execute_runner(
            &tmp,
            Path::new("src/main.anv"),
            "vm",
            false,
            LintConfig::default(),
            &[],
            &[],
        );

        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(
            msg.contains("Runner binary not found"),
            "unexpected error: {msg}"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn format_build_error_no_hint_when_no_anvyx_externs() {
        let msg = format_build_error("error[E0425]: cannot find function `something_else`");
        assert!(msg.contains("Failed to build runner crate"));
        assert!(!msg.contains("Hint:"));
    }

    #[test]
    fn format_build_error_adds_hint_when_anvyx_externs_mentioned() {
        let msg = format_build_error("error[E0425]: cannot find function `anvyx_externs`");
        assert!(msg.contains("Failed to build runner crate"));
        assert!(msg.contains("Hint:"));
        assert!(msg.contains("anvyx_externs()"));
    }

    #[test]
    fn integration_manifest_to_runner_crate() {
        let toml_src = r#"
            [project]
            name = "my_game"
            entry = "src/main.anv"

            [externs.engine]
            path = "my_externs/engine"

            [externs.audio]
            path = "my_externs/audio"
        "#;

        let manifest: Manifest = toml::from_str(toml_src).unwrap();

        assert!(manifest.has_externs());
        assert_eq!(manifest.externs.len(), 2);

        let tmp = std::env::temp_dir().join(format!("anvyx-integration-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(tmp.join("my_externs/engine")).unwrap();
        fs::create_dir_all(tmp.join("my_externs/audio")).unwrap();

        let runner_dir = generate_runner_crate(&tmp, &manifest).unwrap();

        let cargo_content = fs::read_to_string(runner_dir.join("Cargo.toml")).unwrap();
        assert!(cargo_content.contains("[workspace]"));
        assert!(cargo_content.contains("anvyx-std"));
        assert!(cargo_content.contains("engine"));
        assert!(cargo_content.contains("my_externs/engine"));
        assert!(cargo_content.contains("audio"));
        assert!(cargo_content.contains("my_externs/audio"));

        let main_content = fs::read_to_string(runner_dir.join("src/main.rs")).unwrap();
        assert!(main_content.contains("externs.extend(audio::anvyx_externs());"));
        assert!(main_content.contains("externs.extend(engine::anvyx_externs());"));
        assert!(main_content.contains("extern_metadata"));
        assert!(main_content.contains("metadata_dir"));
        assert!(main_content.contains("anvyx_std::std_modules()"));
        assert!(main_content.contains("run_program_with_std"));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn read_metadata_missing_file_returns_error() {
        let tmp = std::env::temp_dir().join(format!("anvyx-meta-read-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let result = read_metadata(&tmp, &manifest_one_extern());

        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(
            msg.contains("Failed to read metadata for extern 'engine'"),
            "unexpected error: {msg}"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn read_metadata_reads_json_files() {
        let tmp = std::env::temp_dir().join(format!("anvyx-meta-ok-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        let metadata_dir = tmp.join("build/metadata");
        fs::create_dir_all(&metadata_dir).unwrap();

        let json = r#"{"types":[],"functions":[]}"#;
        fs::write(metadata_dir.join("engine.json"), json).unwrap();

        let result = read_metadata(&tmp, &manifest_one_extern()).unwrap();
        assert_eq!(result.get("engine").map(String::as_str), Some(json));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn main_rs_metadata_mode_present() {
        let output = generate_main_rs(&manifest_one_extern());
        assert!(output.contains("--metadata"));
        assert!(output.contains("engine::anvyx_exports()"));
        assert!(output.contains("engine::anvyx_type_exports()"));
        assert!(output.contains("anvyx_lang::exports_to_json"));
        assert!(output.contains("engine.json"));
        assert!(output.contains("anvyx_std::std_modules()"));
    }

    #[test]
    fn main_rs_metadata_mode_multiple_externs() {
        let output = generate_main_rs(&manifest_two_externs());
        assert!(output.contains("--metadata"));
        assert!(output.contains("engine::anvyx_exports()"));
        assert!(output.contains("engine::anvyx_type_exports()"));
        assert!(output.contains("audio::anvyx_exports()"));
        assert!(output.contains("audio::anvyx_type_exports()"));
        assert!(output.contains("engine.json"));
        assert!(output.contains("audio.json"));
    }

    #[test]
    fn main_rs_metadata_mode_no_externs() {
        // With no externs, the --metadata block is still present but writes nothing
        let output = generate_main_rs(&manifest_no_externs());
        assert!(output.contains("--metadata"));
        assert!(!output.contains("anvyx_exports"));
        assert!(!output.contains("anvyx_type_exports"));
        assert!(output.contains("anvyx_std::std_modules()"));
        assert!(output.contains("run_program_with_std"));
    }

    #[test]
    fn main_rs_metadata_and_run_both_present() {
        let output = generate_main_rs(&manifest_one_extern());
        // both metadata extraction and normal run paths exist
        assert!(output.contains("--metadata"));
        assert!(output.contains("run_program_with_std"));
    }

    #[test]
    fn extract_metadata_missing_binary_returns_error() {
        let tmp = std::env::temp_dir().join(format!("anvyx-meta-err-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let result = extract_metadata(&tmp);

        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(
            msg.contains("Runner binary not found"),
            "unexpected error: {msg}"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn generate_runner_crate_includes_metadata_mode() {
        let tmp = std::env::temp_dir().join(format!("anvyx-meta-gen-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(tmp.join("my_externs/engine")).unwrap();

        let runner_dir = generate_runner_crate(&tmp, &manifest_one_extern()).unwrap();
        let main_content = fs::read_to_string(runner_dir.join("src/main.rs")).unwrap();

        assert!(main_content.contains("--metadata"));
        assert!(main_content.contains("engine::anvyx_exports()"));
        assert!(main_content.contains("engine.json"));
        assert!(main_content.contains("anvyx_std::std_modules()"));
        assert!(main_content.contains("run_program_with_std"));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn build_main_rs_no_externs() {
        let output = generate_build_main_rs(&manifest_no_externs(), false);

        assert!(output.contains("ENTRY_POINT"));
        assert!(output.contains("\"src/main.anv\""));
        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("current_exe"));
        assert!(output.contains("HashMap"));
        assert!(output.contains("anvyx_std::std_modules()"));
        assert!(output.contains("std_sources"));
        assert!(!output.contains("include_str!"));
        assert!(output.contains("extern_metadata"));
        assert!(!output.contains("anvyx_externs"));
    }

    #[test]
    fn build_main_rs_one_extern() {
        let output = generate_build_main_rs(&manifest_one_extern(), false);

        assert!(output.contains("ENTRY_POINT"));
        assert!(output.contains("\"src/main.anv\""));
        assert!(output.contains("current_exe"));
        assert!(output.contains("include_str!"));
        assert!(output.contains("META_ENGINE"));
        assert!(output.contains("engine.json"));
        assert!(output.contains("../../metadata/engine.json"));
        assert!(output.contains("extern_metadata.insert(\"engine\""));
        assert!(output.contains("engine::anvyx_externs()"));
        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("anvyx_std::std_modules()"));
        assert!(!output.contains("--metadata"));
        assert!(!output.contains("args[1]"));
        assert!(!output.contains("args.get(2)"));
        assert!(!output.contains("args.get(3)"));
    }

    #[test]
    fn build_main_rs_two_externs() {
        let output = generate_build_main_rs(&manifest_two_externs(), false);

        assert!(output.contains("META_AUDIO"));
        assert!(output.contains("META_ENGINE"));
        assert!(output.contains("audio.json"));
        assert!(output.contains("engine.json"));
        assert!(output.contains("audio::anvyx_externs()"));
        assert!(output.contains("engine::anvyx_externs()"));
        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("anvyx_std::std_modules()"));

        let audio_pos = output.find("META_AUDIO").unwrap();
        let engine_pos = output.find("META_ENGINE").unwrap();
        assert!(
            audio_pos < engine_pos,
            "META_AUDIO should appear before META_ENGINE (alphabetical)"
        );
    }

    #[test]
    fn build_main_rs_hardcoded_entry() {
        let manifest = test_manifest(None, "game/start.anv", HashMap::new());
        let output = generate_build_main_rs(&manifest, false);

        assert!(output.contains("const ENTRY_POINT: &str = \"game/start.anv\";"));
    }

    #[test]
    fn build_main_rs_hardcoded_vm_backend() {
        let output = generate_build_main_rs(&manifest_one_extern(), false);

        assert!(output.contains("Backend::from_str(\"vm\")"));
        assert!(!output.contains("args.get(2)"));
    }

    #[test]
    fn build_main_rs_no_cli_args() {
        let output = generate_build_main_rs(&manifest_one_extern(), false);

        assert!(!output.contains("env::args()"));
        assert!(!output.contains("args[1]"));
        assert!(!output.contains("--metadata"));
        assert!(output.contains("run_program_with_std"));
        assert!(output.contains("anvyx_std::std_modules()"));
    }

    #[test]
    fn build_main_rs_debug_profile() {
        let output = generate_build_main_rs(&manifest_no_externs(), false);
        assert!(output.contains("Profile::Debug"));
        assert!(!output.contains("Profile::Release"));
    }

    #[test]
    fn build_main_rs_release_profile() {
        let output = generate_build_main_rs(&manifest_no_externs(), true);
        assert!(output.contains("Profile::Release"));
        assert!(!output.contains("Profile::Debug"));
    }

    #[test]
    fn runner_main_rs_parses_release_flag() {
        let output = generate_main_rs(&manifest_one_extern());
        assert!(output.contains("\"--release\""));
        assert!(output.contains("RustBackendConfig"));
        assert!(!output.contains("RustBackendConfig::default()"));
    }

    #[test]
    fn generate_build_runner_crate_creates_files() {
        let tmp = std::env::temp_dir().join(format!("anvyx-build-crate-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(tmp.join("my_externs/engine")).unwrap();

        let runner_dir = generate_build_runner_crate(&tmp, &manifest_one_extern(), false).unwrap();

        assert!(runner_dir.join("Cargo.toml").exists());
        assert!(runner_dir.join("src/main.rs").exists());

        let cargo_content = fs::read_to_string(runner_dir.join("Cargo.toml")).unwrap();
        assert!(cargo_content.contains("anvyx-runner"));
        assert!(cargo_content.contains("anvyx-std"));
        assert!(cargo_content.contains("engine"));

        let main_content = fs::read_to_string(runner_dir.join("src/main.rs")).unwrap();
        assert!(main_content.contains("ENTRY_POINT"));
        assert!(main_content.contains("include_str!"));
        assert!(main_content.contains("current_exe"));
        assert!(main_content.contains("anvyx_std::std_modules()"));
        assert!(main_content.contains("run_program_with_std"));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn generate_build_runner_crate_no_externs() {
        let tmp = std::env::temp_dir().join(format!("anvyx-build-noext-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let runner_dir = generate_build_runner_crate(&tmp, &manifest_no_externs(), false).unwrap();

        assert!(runner_dir.join("Cargo.toml").exists());
        assert!(runner_dir.join("src/main.rs").exists());

        let cargo_content = fs::read_to_string(runner_dir.join("Cargo.toml")).unwrap();
        assert!(cargo_content.contains("anvyx-lang"));
        assert!(cargo_content.contains("anvyx-std"));
        assert!(!cargo_content.contains("engine"));

        let main_content = fs::read_to_string(runner_dir.join("src/main.rs")).unwrap();
        assert!(main_content.contains("ENTRY_POINT"));
        assert!(main_content.contains("run_program_with_std"));
        assert!(main_content.contains("anvyx_std::std_modules()"));
        assert!(!main_content.contains("include_str!"));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn resolve_project_name_from_manifest() {
        let manifest = test_manifest(Some("my_game"), "src/main.anv", HashMap::new());
        assert_eq!(
            resolve_project_name(&manifest, Path::new("/any/path")),
            "my_game"
        );
    }

    #[test]
    fn resolve_project_name_from_directory() {
        let manifest = test_manifest(None, "src/main.anv", HashMap::new());
        assert_eq!(
            resolve_project_name(&manifest, Path::new("/home/user/cool_project")),
            "cool_project"
        );
    }

    #[test]
    fn resolve_project_name_sanitizes() {
        let manifest = test_manifest(Some("My Cool Game!"), "src/main.anv", HashMap::new());
        assert_eq!(
            resolve_project_name(&manifest, Path::new("/any/path")),
            "my-cool-game"
        );
    }

    #[test]
    fn resolve_project_name_fallback() {
        let manifest = test_manifest(None, "src/main.anv", HashMap::new());
        assert_eq!(
            resolve_project_name(&manifest, Path::new("/")),
            "anvyx-project"
        );
    }

    #[test]
    fn assemble_dist_copies_binary() {
        let tmp = std::env::temp_dir().join(format!("anvyx-dist-copy-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        let binary_dir = tmp.join("build/runner/target/release");
        fs::create_dir_all(&binary_dir).unwrap();
        let binary_path = binary_dir.join("anvyx-runner");
        fs::write(&binary_path, b"fake binary content").unwrap();

        let dist_dir = assemble_dist(&tmp, "my_game").unwrap();

        assert_eq!(dist_dir, tmp.join("build/dist"));
        assert!(dist_dir.join("my_game").exists());
        let content = fs::read(dist_dir.join("my_game")).unwrap();
        assert_eq!(content, b"fake binary content");

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn assemble_dist_cleans_previous() {
        let tmp = std::env::temp_dir().join(format!("anvyx-dist-clean-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        let stale_dir = tmp.join("build/dist");
        fs::create_dir_all(&stale_dir).unwrap();
        fs::write(stale_dir.join("stale_file"), b"old").unwrap();

        let binary_dir = tmp.join("build/runner/target/release");
        fs::create_dir_all(&binary_dir).unwrap();
        fs::write(binary_dir.join("anvyx-runner"), b"new binary").unwrap();

        assemble_dist(&tmp, "my_game").unwrap();

        assert!(!stale_dir.join("stale_file").exists());
        assert!(stale_dir.join("my_game").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn assemble_dist_missing_binary_errors() {
        let tmp = std::env::temp_dir().join(format!("anvyx-dist-missing-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);
        fs::create_dir_all(&tmp).unwrap();

        let result = assemble_dist(&tmp, "my_game");

        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(msg.contains("not found"), "unexpected error: {msg}");

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_copies_anv_files() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-copy-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src/utils")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::write(tmp.join("src/utils/helpers.anv"), b"fn helper() {}").unwrap();

        let dist = tmp.join("dist");
        fs::create_dir_all(&dist).unwrap();

        bundle_sources(&tmp, &dist, &manifest_no_externs()).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert_eq!(
            fs::read(dist.join("src/main.anv")).unwrap(),
            b"fn main() {}"
        );
        assert!(dist.join("src/utils/helpers.anv").exists());
        assert_eq!(
            fs::read(dist.join("src/utils/helpers.anv")).unwrap(),
            b"fn helper() {}"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_skips_non_anv() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-non-anv-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::write(tmp.join("README.md"), b"readme").unwrap();
        fs::write(tmp.join("anvyx.toml"), b"[project]").unwrap();

        let dist = tmp.join("dist");
        fs::create_dir_all(&dist).unwrap();

        bundle_sources(&tmp, &dist, &manifest_no_externs()).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert!(!dist.join("README.md").exists());
        assert!(!dist.join("anvyx.toml").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_skips_build_dir() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-build-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::create_dir_all(tmp.join("build/runner/src")).unwrap();
        fs::write(tmp.join("build/runner/src/main.rs"), b"fn main() {}").unwrap();
        fs::write(tmp.join("build/something.anv"), b"should be skipped").unwrap();

        let dist = tmp.join("dist");
        fs::create_dir_all(&dist).unwrap();

        bundle_sources(&tmp, &dist, &manifest_no_externs()).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert!(!dist.join("build").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_skips_extern_dirs() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-extern-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::create_dir_all(tmp.join("my_externs/engine/src")).unwrap();
        fs::write(tmp.join("my_externs/engine/src/lib.rs"), b"// rust").unwrap();

        let dist = tmp.join("dist");
        fs::create_dir_all(&dist).unwrap();

        bundle_sources(&tmp, &dist, &manifest_one_extern()).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert!(!dist.join("my_externs").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_skips_dotprefix_extern_path() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-dotprefix-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::create_dir_all(tmp.join("my_extern/src")).unwrap();
        fs::write(tmp.join("my_extern/src/lib.rs"), b"// rust").unwrap();

        let dist = tmp.join("dist");
        fs::create_dir_all(&dist).unwrap();

        let mut externs = HashMap::new();
        externs.insert(
            "my_extern".into(),
            ExternEntry {
                path: "./my_extern".into(),
            },
        );
        let manifest = test_manifest(None, "src/main.anv", externs);

        bundle_sources(&tmp, &dist, &manifest).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert!(!dist.join("my_extern").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_skips_hidden_dirs() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-hidden-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::create_dir_all(tmp.join(".git")).unwrap();
        fs::write(tmp.join(".git/config"), b"git config").unwrap();
        fs::create_dir_all(tmp.join(".vscode")).unwrap();
        fs::write(tmp.join(".vscode/settings.json"), b"{}").unwrap();

        let dist = tmp.join("dist");
        fs::create_dir_all(&dist).unwrap();

        bundle_sources(&tmp, &dist, &manifest_no_externs()).unwrap();

        assert!(dist.join("src/main.anv").exists());
        assert!(!dist.join(".git").exists());
        assert!(!dist.join(".vscode").exists());

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn bundle_sources_validates_entry_point() {
        let tmp = std::env::temp_dir().join(format!("anvyx-bundle-entry-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("other")).unwrap();
        fs::write(tmp.join("other/file.anv"), b"fn other() {}").unwrap();

        let dist = tmp.join("dist");
        fs::create_dir_all(&dist).unwrap();

        let result = bundle_sources(&tmp, &dist, &manifest_no_externs());

        assert!(result.is_err());
        let msg = result.unwrap_err();
        assert!(msg.contains("Entry point"), "unexpected error: {msg}");
        assert!(msg.contains("not found"), "unexpected error: {msg}");
        assert!(msg.contains("anvyx.toml"), "hint missing in error: {msg}");

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn build_e2e_no_externs() {
        let tmp = std::env::temp_dir().join(format!("anvyx-e2e-noext-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::write(
            tmp.join("src/main.anv"),
            b"fn main() { println(\"hello\"); }",
        )
        .unwrap();

        let runner_dir = generate_build_runner_crate(&tmp, &manifest_no_externs(), false).unwrap();

        assert!(runner_dir.join("Cargo.toml").exists());
        assert!(runner_dir.join("src/main.rs").exists());

        let main_content = fs::read_to_string(runner_dir.join("src/main.rs")).unwrap();
        assert!(main_content.contains("ENTRY_POINT"));
        assert!(main_content.contains("run_program_with_std"));
        assert!(main_content.contains("anvyx_std::std_modules()"));
        assert!(!main_content.contains("include_str!"));
        assert!(!main_content.contains("--metadata"));
        assert!(!main_content.contains("args[1]"));

        let cargo_content = fs::read_to_string(runner_dir.join("Cargo.toml")).unwrap();
        assert!(cargo_content.contains("anvyx-lang"));
        assert!(cargo_content.contains("anvyx-std"));
        assert!(!cargo_content.contains("engine"));

        let result = assemble_dist(&tmp, "test_project");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not found"));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn build_e2e_with_externs_generates_correct_runner() {
        let tmp = std::env::temp_dir().join(format!("anvyx-e2e-ext-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        fs::create_dir_all(tmp.join("src")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::create_dir_all(tmp.join("my_externs/engine")).unwrap();

        let dev_runner_dir = generate_runner_crate(&tmp, &manifest_one_extern()).unwrap();
        let dev_main = fs::read_to_string(dev_runner_dir.join("src/main.rs")).unwrap();
        assert!(dev_main.contains("--metadata"));
        assert!(dev_main.contains("args[1]"));
        assert!(dev_main.contains("anvyx_std::std_modules()"));
        assert!(dev_main.contains("run_program_with_std"));
        assert!(!dev_main.contains("ENTRY_POINT"));
        assert!(!dev_main.contains("include_str!"));

        let build_runner_dir =
            generate_build_runner_crate(&tmp, &manifest_one_extern(), false).unwrap();
        let build_main = fs::read_to_string(build_runner_dir.join("src/main.rs")).unwrap();
        assert!(build_main.contains("ENTRY_POINT"));
        assert!(build_main.contains("include_str!"));
        assert!(build_main.contains("current_exe"));
        assert!(build_main.contains("anvyx_std::std_modules()"));
        assert!(build_main.contains("run_program_with_std"));
        assert!(!build_main.contains("--metadata"));
        assert!(!build_main.contains("args[1]"));

        let cargo_content = fs::read_to_string(build_runner_dir.join("Cargo.toml")).unwrap();
        assert!(cargo_content.contains("anvyx-lang"));
        assert!(cargo_content.contains("anvyx-std"));
        assert!(cargo_content.contains("engine"));

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn build_dist_layout_with_sources() {
        let tmp = std::env::temp_dir().join(format!("anvyx-e2e-layout-{}", process::id()));
        let _ = fs::remove_dir_all(&tmp);

        let binary_dir = tmp.join("build/runner/target/release");
        fs::create_dir_all(&binary_dir).unwrap();
        fs::write(binary_dir.join("anvyx-runner"), b"fake binary").unwrap();

        fs::create_dir_all(tmp.join("src/utils")).unwrap();
        fs::write(tmp.join("src/main.anv"), b"fn main() {}").unwrap();
        fs::write(tmp.join("src/utils/helper.anv"), b"fn help() {}").unwrap();

        fs::create_dir_all(tmp.join("build")).unwrap();
        fs::write(tmp.join("build/something.anv"), b"skipped").unwrap();

        fs::create_dir_all(tmp.join(".git")).unwrap();
        fs::write(tmp.join(".git/config"), b"git config").unwrap();

        let dist_dir = assemble_dist(&tmp, "test_game").unwrap();
        bundle_sources(&tmp, &dist_dir, &manifest_no_externs()).unwrap();

        assert!(dist_dir.join("test_game").exists());
        assert!(dist_dir.join("src/main.anv").exists());
        assert!(dist_dir.join("src/utils/helper.anv").exists());
        assert!(!dist_dir.join("build").exists());
        assert!(!dist_dir.join(".git").exists());

        let _ = fs::remove_dir_all(&tmp);
    }
}
