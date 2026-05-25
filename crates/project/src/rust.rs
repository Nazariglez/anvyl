use std::{
    fs,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

const STALE_ARTIFACT_LOCK_AFTER: Duration = Duration::from_mins(30);

pub use anvyx_backend::rust::cargo_job::RustCargoProfile;
use anvyx_backend::rust::{
    cargo_job::{
        self, LockFile, RustCargoBatchCase, RustCargoBatchOutput, RustCargoFailure, RustCargoMode,
        RustCargoOutput, host_executable_name,
    },
    emit::RustSource,
};
use anvyx_lang2::{AirBuildError, FrontendConfig};

use crate::check::{air_error_ref, build_air_path_typed};

#[derive(Debug, Clone)]
pub struct CleanRustRunInput {
    pub file: PathBuf,
    pub frontend: FrontendConfig,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct CleanRustBatchInput {
    pub cases: Vec<CleanRustBatchCase>,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
    pub timeout: Option<Duration>,
}

#[derive(Debug, Clone)]
pub struct CleanRustBatchCase {
    pub file: PathBuf,
    pub frontend: FrontendConfig,
}

pub struct CleanRustBatchOutput {
    pub binaries: Vec<CleanRustBatchBinary>,
}

pub struct CleanRustBatchBinary {
    pub file: PathBuf,
    pub binary: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CleanRustBuildInput {
    pub file: PathBuf,
    pub project_root: PathBuf,
    pub project_name: String,
    pub frontend: FrontendConfig,
    pub cargo_profile: RustCargoProfile,
    pub cache_root: Option<PathBuf>,
    pub output_root: PathBuf,
}

#[derive(Debug)]
pub enum CleanRustError {
    Air(AirBuildError<anvyx_lang2::CheckError>),
    Plan(anvyx_backend::rust::RustPlanError),
    Cargo(cargo_job::RustCargoError),
    CargoBuild(RustCargoFailure),
    Run(RustCargoFailure),
}

impl std::fmt::Display for CleanRustError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Air(error) => write!(f, "{}", air_error_ref(error)),
            Self::Plan(error) => write!(f, "{error}"),
            Self::Cargo(error) => write!(f, "{error}"),
            Self::CargoBuild(output) => {
                write!(f, "generated Rust Cargo build failed")?;
                if !output.stderr.is_empty() {
                    write!(f, "\n{}", output.stderr)?;
                }
                Ok(())
            }
            Self::Run(output) => {
                write!(f, "generated Rust binary failed")?;
                if !output.stderr.is_empty() {
                    write!(f, "\n{}", output.stderr)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for CleanRustError {}

pub struct CleanRustRunOutput {
    pub stdout: String,
    pub stderr: String,
}

pub struct CleanRustBuildOutput {
    pub generated_manifest: PathBuf,
    pub generated_source: PathBuf,
    pub cargo_binary: PathBuf,
    pub artifact: PathBuf,
}

pub fn run_clean_rust(input: CleanRustRunInput) -> Result<CleanRustRunOutput, CleanRustError> {
    let cargo_profile = input.cargo_profile;
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| default_run_cache_root(&input.file));
    let semantic_profile = semantic_profile_name(input.frontend.context.profile);
    let source = emit_source(input.file, input.frontend)?;
    let job = cargo_job::single_program_job(
        source,
        cache_root,
        cargo_profile,
        RustCargoMode::Run,
        semantic_profile,
    );

    match cargo_job::execute(&job).map_err(CleanRustError::Cargo)? {
        RustCargoOutput::Success(output) => Ok(CleanRustRunOutput {
            stdout: output.stdout,
            stderr: output.stderr,
        }),
        RustCargoOutput::CargoFailed(output) => Err(CleanRustError::CargoBuild(output)),
        RustCargoOutput::RunFailed(output) => Err(CleanRustError::Run(output)),
    }
}

pub fn build_clean_rust_batch(input: CleanRustBatchInput) -> Result<CleanRustBatchOutput, String> {
    let cargo_profile = input.cargo_profile;
    let cache_root = input.cache_root.unwrap_or_else(default_cache_root);
    let mut files = std::collections::HashMap::new();
    let mut cases = vec![];
    let mut semantic_profile = None;
    for (index, case) in input.cases.into_iter().enumerate() {
        let profile = semantic_profile_name(case.frontend.context.profile);
        match semantic_profile {
            Some(existing) if existing != profile => {
                return Err("clean Rust batch cases must share a semantic profile".to_string());
            }
            None => semantic_profile = Some(profile),
            Some(_) => {}
        }
        let file = case.file;
        let source = emit_source(file.clone(), case.frontend).map_err(|error| error.to_string())?;
        let seed = format!("{index}\0{}\0{}", file.display(), source.as_str());
        let fingerprint = cargo_job::cargo_fingerprint(&cargo_job::RustCargoFingerprintInput {
            source: &seed,
            manifest_template: "batch-case-bin-name",
            semantic_profile: profile,
            cargo_profile,
            dependencies: &[],
        });
        let name = cargo_job::batch_case_name(&fingerprint);
        if files.insert(name.clone(), file).is_some() {
            return Err(format!("duplicate clean Rust batch case name `{name}`"));
        }
        cases.push(RustCargoBatchCase { name, source });
    }
    let job = cargo_job::batch_job(
        cases,
        cache_root,
        cargo_profile,
        semantic_profile.unwrap_or("debug"),
    );
    match cargo_job::execute_batch_with_timeout(&job, input.timeout)
        .map_err(|error| error.to_string())?
    {
        RustCargoBatchOutput::Success(output) => Ok(CleanRustBatchOutput {
            binaries: output
                .binaries
                .into_iter()
                .map(|(name, binary)| CleanRustBatchBinary {
                    file: files
                        .remove(&name)
                        .expect("batch binary maps to input file"),
                    binary,
                })
                .collect(),
        }),
        RustCargoBatchOutput::CargoFailed(output) => Err(format!(
            "generated Rust Cargo batch build failed\n{}",
            output.stderr
        )),
    }
}

pub fn build_clean_rust(
    input: CleanRustBuildInput,
) -> Result<CleanRustBuildOutput, CleanRustError> {
    let cargo_profile = input.cargo_profile;
    let cache_root = input
        .cache_root
        .unwrap_or_else(|| default_cache_root_for(&input.project_root));
    let semantic_profile = semantic_profile_name(input.frontend.context.profile);
    let source = emit_source(input.file, input.frontend)?;
    let job = cargo_job::single_program_job(
        source,
        cache_root,
        cargo_profile,
        RustCargoMode::Build,
        semantic_profile,
    );

    let output = match cargo_job::execute(&job).map_err(CleanRustError::Cargo)? {
        RustCargoOutput::Success(output) => output,
        RustCargoOutput::CargoFailed(output) => return Err(CleanRustError::CargoBuild(output)),
        RustCargoOutput::RunFailed(output) => return Err(CleanRustError::Run(output)),
    };
    let project_name = clean_artifact_name(&input.project_name);
    let artifact = public_artifact_path(&input.output_root, cargo_profile, &project_name);
    publish_artifact(&output.binary_path, &artifact).map_err(CleanRustError::Cargo)?;

    Ok(CleanRustBuildOutput {
        generated_manifest: output.manifest_path,
        generated_source: output.source_path,
        cargo_binary: output.binary_path,
        artifact,
    })
}

fn default_cache_root() -> PathBuf {
    std::env::current_dir()
        .map_or_else(|_| std::env::temp_dir(), |dir| default_cache_root_for(&dir))
}

fn default_run_cache_root(file: &Path) -> PathBuf {
    match crate::manifest::find_nearest_manifest(file) {
        Ok(Some(manifest)) => manifest
            .parent()
            .map_or_else(default_cache_root, default_cache_root_for),
        Ok(None) | Err(_) => default_cache_root(),
    }
}

fn default_cache_root_for(project_root: &Path) -> PathBuf {
    if let Some(path) = std::env::var_os("ANVYX_CACHE_DIR") {
        return PathBuf::from(path);
    }
    project_root.join(".anvyx").join("cache").join("rust")
}

fn public_artifact_path(
    output_root: &Path,
    profile: RustCargoProfile,
    project_name: &str,
) -> PathBuf {
    output_root
        .join(profile.dir_name())
        .join(host_executable_name(project_name))
}

fn clean_artifact_name(raw: &str) -> String {
    let name = raw
        .to_lowercase()
        .chars()
        .map(|c| if c == ' ' { '-' } else { c })
        .filter(|c| c.is_ascii_alphanumeric() || *c == '-' || *c == '_')
        .collect::<String>();
    if name.is_empty() {
        "anvyx-project".to_string()
    } else {
        name
    }
}

fn publish_artifact(source: &Path, artifact: &Path) -> Result<(), cargo_job::RustCargoError> {
    let dir = artifact.parent().expect("artifact path has parent");
    fs::create_dir_all(dir)?;
    let _lock = LockFile::acquire_stale(
        artifact.with_extension("lock"),
        None,
        STALE_ARTIFACT_LOCK_AFTER,
    )?;
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let tmp = artifact.with_file_name(format!(
        ".{}.{}.tmp",
        artifact
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("artifact"),
        stamp
    ));
    fs::copy(source, &tmp)?;
    let _ = fs::remove_file(artifact);
    fs::rename(tmp, artifact)?;
    Ok(())
}

fn semantic_profile_name(profile: anvyx_lang2::Profile) -> &'static str {
    match profile {
        anvyx_lang2::Profile::Debug => "debug",
        anvyx_lang2::Profile::Release => "release",
    }
}

fn emit_source(file: PathBuf, frontend: FrontendConfig) -> Result<RustSource, CleanRustError> {
    let output = build_air_path_typed(&file, frontend).map_err(CleanRustError::Air)?;
    let plan = anvyx_backend::rust::plan(
        &output.air.as_verified(),
        anvyx_backend::rust::RustPlanConfig::default(),
    )
    .map_err(CleanRustError::Plan)?;
    Ok(anvyx_backend::rust::emit::emit(&plan.verified()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_project(root: &Path, name: &str) -> PathBuf {
        fs::write(
            root.join("anvyx.toml"),
            format!("[project]\nname = \"{name}\"\nentry = \"main.anv\"\n"),
        )
        .unwrap();
        let file = root.join("main.anv");
        fs::write(&file, "fn main() { println(42); }\n").unwrap();
        file
    }

    fn build(root: &Path, profile: RustCargoProfile) -> CleanRustBuildOutput {
        build_clean_rust(CleanRustBuildInput {
            file: write_project(root, "demo"),
            project_root: root.to_path_buf(),
            project_name: "demo".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: profile,
            cache_root: None,
            output_root: root.join("build"),
        })
        .unwrap()
    }

    #[test]
    fn clean_run_cache_uses_nearest_manifest_root() {
        let project = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        let file = write_project(project.path(), "demo");
        let old_dir = std::env::current_dir().unwrap();
        std::env::set_current_dir(outside.path()).unwrap();

        let root = default_run_cache_root(&file);

        std::env::set_current_dir(old_dir).unwrap();
        assert_eq!(
            root,
            project
                .path()
                .canonicalize()
                .unwrap()
                .join(".anvyx/cache/rust")
        );
    }

    #[test]
    fn batch_binary_names_are_fingerprinted() {
        let temp = tempfile::tempdir().unwrap();
        let first = temp.path().join("first.anv");
        let second = temp.path().join("second.anv");
        fs::write(&first, "fn main() { println(1); }\n").unwrap();
        fs::write(&second, "fn main() { println(2); }\n").unwrap();

        let output = build_clean_rust_batch(CleanRustBatchInput {
            cases: [first, second]
                .into_iter()
                .map(|file| CleanRustBatchCase {
                    file,
                    frontend: FrontendConfig::default(),
                })
                .collect(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: Some(temp.path().join("cache")),
            timeout: Some(Duration::from_mins(1)),
        })
        .unwrap();
        let names = output
            .binaries
            .iter()
            .map(|binary| {
                binary
                    .binary
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned()
            })
            .collect::<Vec<_>>();

        assert_eq!(names.len(), 2);
        assert!(names.iter().all(|name| name.starts_with("case_")));
        assert!(
            !names
                .iter()
                .any(|name| name == "case_0" || name == "case_1")
        );
        assert_ne!(names[0], names[1]);
    }

    #[test]
    fn clean_build_copies_debug_artifact() {
        let temp = tempfile::tempdir().unwrap();

        let output = build(temp.path(), RustCargoProfile::Dev);

        assert_eq!(
            output.artifact,
            temp.path()
                .join("build")
                .join("debug")
                .join(host_executable_name("demo"))
        );
        assert!(output.artifact.exists());
        assert!(
            output
                .generated_manifest
                .starts_with(temp.path().join(".anvyx/cache/rust"))
        );
        assert!(!temp.path().join("build/runner").exists());
        assert!(!temp.path().join("build/dist").exists());
    }

    #[test]
    fn clean_build_copies_release_artifact() {
        let temp = tempfile::tempdir().unwrap();

        let output = build(temp.path(), RustCargoProfile::Release);

        assert_eq!(
            output.artifact,
            temp.path()
                .join("build")
                .join("release")
                .join(host_executable_name("demo"))
        );
        assert!(output.artifact.exists());
    }

    #[test]
    fn clean_build_repeats_with_stable_public_path() {
        let temp = tempfile::tempdir().unwrap();

        let first = build(temp.path(), RustCargoProfile::Dev);
        let second = build(temp.path(), RustCargoProfile::Dev);

        assert_eq!(first.artifact, second.artifact);
        assert_eq!(first.generated_manifest, second.generated_manifest);
    }

    #[test]
    fn clean_build_sanitizes_public_artifact_name() {
        let temp = tempfile::tempdir().unwrap();
        let output = build_clean_rust(CleanRustBuildInput {
            file: write_project(temp.path(), "../Bad Name"),
            project_root: temp.path().to_path_buf(),
            project_name: "../Bad Name".to_string(),
            frontend: FrontendConfig::default(),
            cargo_profile: RustCargoProfile::Dev,
            cache_root: None,
            output_root: temp.path().join("build"),
        })
        .unwrap();

        assert_eq!(
            output.artifact,
            temp.path()
                .join("build")
                .join("debug")
                .join(host_executable_name("bad-name"))
        );
    }
}
