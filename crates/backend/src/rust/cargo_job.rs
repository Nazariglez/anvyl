use std::{
    error::Error,
    fmt::{self, Write},
    fs,
    fs::{File, OpenOptions},
    io::Read,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    thread,
    time::{Duration, Instant},
};

use sha2::{Digest, Sha256};

use super::emit::RustSource;

pub const RUST_CARGO_FINGERPRINT_SCHEMA: &str = "anvyx-cargo-v1";
pub const RUST_CARGO_NAME_DIGEST_LEN: usize = 16;
#[derive(Debug, Clone)]
pub struct RustCargoJob {
    source: RustSource,
    package: RustPackageSpec,
    mode: RustCargoMode,
    profile: RustCargoProfile,
    cache_root: PathBuf,
    fingerprint: RustCargoFingerprint,
}

#[derive(Debug, Clone)]
pub struct RustCargoBatchJob {
    cases: Vec<RustCargoBatchCase>,
    package: RustPackageSpec,
    profile: RustCargoProfile,
    cache_root: PathBuf,
    fingerprint: RustCargoFingerprint,
}

#[derive(Debug, Clone)]
pub struct RustCargoBatchCase {
    pub name: RustCargoName,
    pub source: RustSource,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustCargoBatchOutput {
    Success(RustCargoBatchSuccess),
    CargoFailed(RustCargoFailure),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCargoBatchSuccess {
    pub binaries: Vec<(RustCargoName, PathBuf)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustCargoMode {
    Build,
    Run,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustCargoProfile {
    Dev,
    Release,
}

impl RustCargoProfile {
    pub fn from_release(release: bool) -> Self {
        if release { Self::Release } else { Self::Dev }
    }

    fn name(self) -> &'static str {
        match self {
            Self::Dev => "dev",
            Self::Release => "release",
        }
    }

    pub fn dir_name(self) -> &'static str {
        match self {
            Self::Dev => "debug",
            Self::Release => "release",
        }
    }

    fn build_args(self) -> &'static [&'static str] {
        match self {
            Self::Dev => &[],
            Self::Release => &["--release"],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RustPackageSpec {
    package_name: RustCargoName,
    binary_name: RustCargoName,
    dependencies: Vec<RustCargoDependency>,
}

const SINGLE_PROGRAM_MANIFEST: &str = "single-bin-edition-2024-empty-deps-profile-sections";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCargoDependency {
    pub name: RustCargoName,
    pub package: Option<RustCargoName>,
    pub source: RustCargoDependencySource,
    pub features: Vec<String>,
    pub default_features: bool,
}

impl RustCargoDependency {
    fn sort_key(&self) -> String {
        let source = match &self.source {
            RustCargoDependencySource::Registry { version } => format!("version:{version}"),
            RustCargoDependencySource::Path(path) => format!("path:{path}"),
        };
        format!(
            "{}|{}|{source}",
            self.name.as_str(),
            self.package.as_ref().map_or("", RustCargoName::as_str)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustCargoDependencySource {
    Registry { version: String },
    Path(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustCargoOutput {
    Success(RustCargoSuccess),
    CargoFailed(RustCargoFailure),
    RunFailed(RustCargoFailure),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCargoSuccess {
    pub manifest_path: PathBuf,
    pub source_path: PathBuf,
    pub target_dir: PathBuf,
    pub binary_path: PathBuf,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCargoFailure {
    pub manifest_path: PathBuf,
    pub source_path: PathBuf,
    pub target_dir: PathBuf,
    pub status: Option<i32>,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug)]
pub enum RustCargoError {
    Io(std::io::Error),
    CargoUnavailable,
    BinaryMissing(PathBuf),
    InvalidJob(String),
    LockTimeout(PathBuf),
    Timeout,
}

impl fmt::Display for RustCargoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(error) => write!(f, "Cargo job I/O failed: {error}"),
            Self::CargoUnavailable => write!(f, "cargo is unavailable"),
            Self::BinaryMissing(path) => {
                write!(f, "Cargo build did not produce binary {}", path.display())
            }
            Self::InvalidJob(message) => write!(f, "invalid Cargo job: {message}"),
            Self::LockTimeout(path) => {
                write!(f, "timed out waiting for Cargo job lock {}", path.display())
            }
            Self::Timeout => write!(f, "Cargo job timed out"),
        }
    }
}

impl Error for RustCargoError {}

impl From<std::io::Error> for RustCargoError {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

pub fn single_program_job(
    source: RustSource,
    cache_root: PathBuf,
    profile: RustCargoProfile,
    mode: RustCargoMode,
    semantic_profile: &str,
) -> RustCargoJob {
    single_program_job_with_dependencies(
        source,
        cache_root,
        profile,
        mode,
        semantic_profile,
        vec![],
    )
}

pub fn single_program_job_with_dependencies(
    source: RustSource,
    cache_root: PathBuf,
    profile: RustCargoProfile,
    mode: RustCargoMode,
    semantic_profile: &str,
    dependencies: Vec<RustCargoDependency>,
) -> RustCargoJob {
    let dependencies = normalized_dependencies(dependencies);
    let fingerprint = cargo_fingerprint(&RustCargoFingerprintInput {
        source: source.as_str(),
        manifest_template: SINGLE_PROGRAM_MANIFEST,
        semantic_profile,
        cargo_profile: profile,
        dependencies: &dependencies,
    });
    let name = single_package_name(&fingerprint);
    RustCargoJob {
        source,
        package: RustPackageSpec {
            package_name: name.clone(),
            binary_name: name,
            dependencies,
        },
        mode,
        profile,
        cache_root,
        fingerprint,
    }
}

pub fn batch_job(
    cases: Vec<RustCargoBatchCase>,
    cache_root: PathBuf,
    profile: RustCargoProfile,
    semantic_profile: &str,
) -> RustCargoBatchJob {
    let mut writer = FingerprintWriter::default();
    writer.field(RUST_CARGO_FINGERPRINT_SCHEMA);
    writer.field("batch-bin-edition-2024-empty-deps");
    writer.field(semantic_profile);
    writer.field(profile.name());
    let mut cases = cases;
    cases.sort_by_key(|case| case.name.as_str().to_string());
    for case in &cases {
        writer.field(case.name.as_str());
        writer.field(case.source.as_str());
    }
    let fingerprint = writer.finish();
    let package_name = batch_package_name(&fingerprint);
    RustCargoBatchJob {
        cases,
        package: RustPackageSpec {
            package_name: package_name.clone(),
            binary_name: package_name,
            dependencies: vec![],
        },
        profile,
        cache_root,
        fingerprint,
    }
}

pub fn execute(job: &RustCargoJob) -> Result<RustCargoOutput, RustCargoError> {
    validate_package(&job.package)?;

    let layout = RustCargoLayout::new(job.cache_root.clone(), job.fingerprint.clone());
    let _lock = LockFile::acquire(layout.lock_path(), None)?;
    write_single_package(job, &layout)?;

    let cargo = Command::new("cargo")
        .arg("build")
        .args(job.profile.build_args())
        .arg("--manifest-path")
        .arg(layout.manifest_path())
        .arg("--target-dir")
        .arg(layout.target_dir())
        .env_remove("CARGO_TARGET_DIR")
        .output()
        .map_err(|error| {
            if error.kind() == std::io::ErrorKind::NotFound {
                RustCargoError::CargoUnavailable
            } else {
                RustCargoError::Io(error)
            }
        })?;

    if !cargo.status.success() {
        return Ok(RustCargoOutput::CargoFailed(failure(
            &layout,
            cargo.status.code(),
            cargo.stdout,
            cargo.stderr,
        )));
    }

    let binary_path = layout.binary_path(job.profile, &job.package.binary_name);
    if !binary_path.exists() {
        return Err(RustCargoError::BinaryMissing(binary_path));
    }

    match job.mode {
        RustCargoMode::Build => Ok(RustCargoOutput::Success(success(
            &layout,
            binary_path,
            cargo.stdout,
            cargo.stderr,
        ))),
        RustCargoMode::Run => {
            let run = Command::new(&binary_path).output()?;
            if !run.status.success() {
                return Ok(RustCargoOutput::RunFailed(failure(
                    &layout,
                    run.status.code(),
                    run.stdout,
                    run.stderr,
                )));
            }
            Ok(RustCargoOutput::Success(success(
                &layout,
                binary_path,
                run.stdout,
                run.stderr,
            )))
        }
    }
}

pub fn execute_batch_with_timeout(
    job: &RustCargoBatchJob,
    timeout: Option<Duration>,
) -> Result<RustCargoBatchOutput, RustCargoError> {
    validate_package(&job.package)?;
    validate_batch_cases(&job.cases)?;
    let started = Instant::now();
    let layout = RustCargoLayout::new(job.cache_root.clone(), job.fingerprint.clone());
    let _lock = LockFile::acquire(layout.lock_path(), timeout)?;
    write_batch_package(job, &layout)?;

    let mut cargo = Command::new("cargo");
    cargo
        .arg("build")
        .args(job.profile.build_args())
        .arg("--manifest-path")
        .arg(layout.manifest_path())
        .arg("--target-dir")
        .arg(layout.target_dir())
        .env_remove("CARGO_TARGET_DIR");
    let cargo = command_output(cargo, remaining_timeout(started, timeout)?)?;

    if !cargo.status.success() {
        return Ok(RustCargoBatchOutput::CargoFailed(failure(
            &layout,
            cargo.status.code(),
            cargo.stdout,
            cargo.stderr,
        )));
    }

    let mut binaries = vec![];
    for case in &job.cases {
        let path = layout.binary_path(job.profile, &case.name);
        if !path.exists() {
            return Err(RustCargoError::BinaryMissing(path));
        }
        binaries.push((case.name.clone(), path));
    }

    Ok(RustCargoBatchOutput::Success(RustCargoBatchSuccess {
        binaries,
    }))
}

fn write_single_package(
    job: &RustCargoJob,
    layout: &RustCargoLayout,
) -> Result<(), RustCargoError> {
    let src_dir = layout
        .source_path()
        .parent()
        .expect("source path has parent")
        .to_path_buf();
    fs::create_dir_all(src_dir)?;
    write_if_changed(
        &layout.manifest_path(),
        render_manifest(&job.package).as_bytes(),
    )?;
    write_if_changed(&layout.source_path(), job.source.as_str().as_bytes())?;
    Ok(())
}

fn write_batch_package(
    job: &RustCargoBatchJob,
    layout: &RustCargoLayout,
) -> Result<(), RustCargoError> {
    fs::create_dir_all(layout.crate_dir().join("src").join("bin"))?;
    write_if_changed(
        &layout.manifest_path(),
        render_batch_manifest(&job.package, &job.cases).as_bytes(),
    )?;
    for case in &job.cases {
        write_if_changed(
            &layout.batch_case_source_path(&case.name),
            case.source.as_str().as_bytes(),
        )?;
    }
    Ok(())
}

fn render_batch_manifest(package: &RustPackageSpec, cases: &[RustCargoBatchCase]) -> String {
    let mut text = format!(
        "[package]\nname = \"{}\"\nversion = \"0.0.0\"\nedition = \"2024\"\npublish = false\n\n[workspace]\n\n[profile.dev]\nopt-level = 0\n\n[profile.release]\nopt-level = 3\n",
        package.package_name.as_str()
    );
    for case in cases {
        write!(
            text,
            "\n[[bin]]\nname = \"{}\"\npath = \"src/bin/{}.rs\"\n",
            case.name.as_str(),
            case.name.as_str()
        )
        .expect("write to string succeeds");
    }
    text
}

fn render_manifest(package: &RustPackageSpec) -> String {
    let mut text = format!(
        "[package]\nname = \"{}\"\nversion = \"0.0.0\"\nedition = \"2024\"\npublish = false\n\n[workspace]\n\n[[bin]]\nname = \"{}\"\npath = \"src/main.rs\"\n\n[profile.dev]\nopt-level = 0\n\n[profile.release]\nopt-level = 3\n",
        package.package_name.as_str(),
        package.binary_name.as_str()
    );
    let dependencies = sorted_dependencies(&package.dependencies);
    if !dependencies.is_empty() {
        text.push_str("\n[dependencies]\n");
        for dep in dependencies {
            text.push_str(&render_dependency(dep));
        }
    }
    text
}

fn validate_package(package: &RustPackageSpec) -> Result<(), RustCargoError> {
    let mut names = std::collections::HashSet::new();
    for dep in &package.dependencies {
        if !names.insert(dep.name.as_str()) {
            return Err(RustCargoError::InvalidJob(format!(
                "duplicate Cargo dependency `{}`",
                dep.name.as_str()
            )));
        }
    }
    Ok(())
}

fn validate_batch_cases(cases: &[RustCargoBatchCase]) -> Result<(), RustCargoError> {
    let mut names = std::collections::HashSet::new();
    for case in cases {
        if !names.insert(case.name.as_str()) {
            return Err(RustCargoError::InvalidJob(format!(
                "duplicate Cargo batch case `{}`",
                case.name.as_str()
            )));
        }
    }
    Ok(())
}

fn sorted_dependencies(dependencies: &[RustCargoDependency]) -> Vec<&RustCargoDependency> {
    let mut deps = dependencies.iter().collect::<Vec<_>>();
    deps.sort_by_key(|dep| dep.name.as_str());
    deps
}

fn normalized_dependencies(mut dependencies: Vec<RustCargoDependency>) -> Vec<RustCargoDependency> {
    for dep in &mut dependencies {
        dep.features.sort();
        dep.features.dedup();
    }
    dependencies.sort_by_key(RustCargoDependency::sort_key);
    dependencies
}

fn render_dependency(dep: &RustCargoDependency) -> String {
    let mut fields = vec![];
    match &dep.source {
        RustCargoDependencySource::Registry { version } => {
            fields.push(format!("version = \"{}\"", toml_escape(version)));
        }
        RustCargoDependencySource::Path(path) => {
            fields.push(format!("path = \"{}\"", toml_escape(path)));
        }
    }
    if let Some(package) = &dep.package {
        fields.push(format!("package = \"{}\"", toml_escape(package.as_str())));
    }
    if !dep.default_features {
        fields.push("default-features = false".to_string());
    }
    if !dep.features.is_empty() {
        let features = dep
            .features
            .iter()
            .map(|feature| format!("\"{}\"", toml_escape(feature)))
            .collect::<Vec<_>>()
            .join(", ");
        fields.push(format!("features = [{features}]"));
    }
    format!("{} = {{ {} }}\n", dep.name.as_str(), fields.join(", "))
}

fn toml_escape(text: &str) -> String {
    text.replace('\\', "\\\\").replace('"', "\\\"")
}

fn write_if_changed(path: &Path, bytes: &[u8]) -> Result<(), RustCargoError> {
    match fs::read(path) {
        Ok(current) if current == bytes => return Ok(()),
        Ok(_) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(RustCargoError::Io(error)),
    }
    fs::write(path, bytes)?;
    Ok(())
}

fn remaining_timeout(
    started: Instant,
    timeout: Option<Duration>,
) -> Result<Option<Duration>, RustCargoError> {
    let Some(timeout) = timeout else {
        return Ok(None);
    };
    timeout
        .checked_sub(started.elapsed())
        .filter(|remaining| !remaining.is_zero())
        .map(Some)
        .ok_or(RustCargoError::Timeout)
}

fn command_output(
    mut command: Command,
    timeout: Option<Duration>,
) -> Result<std::process::Output, RustCargoError> {
    let Some(timeout) = timeout else {
        return command.output().map_err(|error| {
            if error.kind() == std::io::ErrorKind::NotFound {
                RustCargoError::CargoUnavailable
            } else {
                RustCargoError::Io(error)
            }
        });
    };
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = command.spawn().map_err(|error| {
        if error.kind() == std::io::ErrorKind::NotFound {
            RustCargoError::CargoUnavailable
        } else {
            RustCargoError::Io(error)
        }
    })?;
    let start = Instant::now();
    loop {
        if let Some(status) = child.try_wait()? {
            let mut stdout = vec![];
            let mut stderr = vec![];
            if let Some(mut out) = child.stdout.take() {
                let _ = out.read_to_end(&mut stdout);
            }
            if let Some(mut err) = child.stderr.take() {
                let _ = err.read_to_end(&mut stderr);
            }
            return Ok(std::process::Output {
                status,
                stdout,
                stderr,
            });
        }
        if start.elapsed() >= timeout {
            let _ = child.kill();
            let _ = child.wait();
            return Err(RustCargoError::Timeout);
        }
        thread::sleep(LOCK_POLL);
    }
}

fn success(
    layout: &RustCargoLayout,
    binary_path: PathBuf,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
) -> RustCargoSuccess {
    RustCargoSuccess {
        manifest_path: layout.manifest_path(),
        source_path: layout.source_path(),
        target_dir: layout.target_dir(),
        binary_path,
        stdout: String::from_utf8_lossy(&stdout).into_owned(),
        stderr: String::from_utf8_lossy(&stderr).into_owned(),
    }
}

fn failure(
    layout: &RustCargoLayout,
    status: Option<i32>,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
) -> RustCargoFailure {
    RustCargoFailure {
        manifest_path: layout.manifest_path(),
        source_path: layout.source_path(),
        target_dir: layout.target_dir(),
        status,
        stdout: String::from_utf8_lossy(&stdout).into_owned(),
        stderr: String::from_utf8_lossy(&stderr).into_owned(),
    }
}

const LOCK_POLL: Duration = Duration::from_millis(25);
const STALE_LOCK_AFTER: Duration = Duration::from_mins(30);

pub struct LockFile {
    path: PathBuf,
    _file: File,
}

impl LockFile {
    fn acquire(path: PathBuf, timeout: Option<Duration>) -> Result<Self, RustCargoError> {
        Self::acquire_stale(path, timeout, STALE_LOCK_AFTER)
    }

    pub fn acquire_stale(
        path: PathBuf,
        timeout: Option<Duration>,
        stale_after: Duration,
    ) -> Result<Self, RustCargoError> {
        let dir = path.parent().expect("lock path has parent");
        fs::create_dir_all(dir)?;
        let started = Instant::now();
        loop {
            match OpenOptions::new().write(true).create_new(true).open(&path) {
                Ok(file) => return Ok(Self { path, _file: file }),
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
                    if remove_stale_lock(&path, stale_after)? {
                        continue;
                    }
                    if timeout.is_some_and(|timeout| started.elapsed() >= timeout) {
                        return Err(RustCargoError::LockTimeout(path));
                    }
                    thread::sleep(LOCK_POLL);
                }
                Err(error) => return Err(RustCargoError::Io(error)),
            }
        }
    }
}

fn remove_stale_lock(path: &Path, stale_after: Duration) -> Result<bool, RustCargoError> {
    let Ok(modified) = fs::metadata(path).and_then(|metadata| metadata.modified()) else {
        return Ok(false);
    };
    if modified.elapsed().unwrap_or_default() < stale_after {
        return Ok(false);
    }
    match fs::remove_file(path) {
        Ok(()) => Ok(true),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(error) => Err(RustCargoError::Io(error)),
    }
}

impl Drop for LockFile {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RustCargoLayout {
    cache_root: PathBuf,
    fingerprint: RustCargoFingerprint,
}

impl RustCargoLayout {
    fn new(cache_root: PathBuf, fingerprint: RustCargoFingerprint) -> Self {
        Self {
            cache_root,
            fingerprint,
        }
    }

    fn crate_dir(&self) -> PathBuf {
        self.cache_root
            .join("crates")
            .join(self.fingerprint.as_str())
    }

    fn manifest_path(&self) -> PathBuf {
        self.crate_dir().join("Cargo.toml")
    }

    fn source_path(&self) -> PathBuf {
        self.crate_dir().join("src").join("main.rs")
    }

    fn target_dir(&self) -> PathBuf {
        self.cache_root.join("target")
    }

    fn lock_path(&self) -> PathBuf {
        self.cache_root
            .join("locks")
            .join(format!("{}.lock", self.fingerprint.as_str()))
    }

    fn batch_case_source_path(&self, case_name: &RustCargoName) -> PathBuf {
        self.crate_dir()
            .join("src")
            .join("bin")
            .join(format!("{}.rs", case_name.as_str()))
    }

    fn binary_path(&self, profile: RustCargoProfile, binary_name: &RustCargoName) -> PathBuf {
        self.target_dir()
            .join(profile.dir_name())
            .join(host_binary_name(binary_name))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RustCargoFingerprint(String);

impl RustCargoFingerprint {
    pub fn parse(text: impl Into<String>) -> Result<Self, String> {
        let text = text.into();
        if text.len() != 64 || !text.bytes().all(|b| b.is_ascii_hexdigit()) {
            return Err("Cargo fingerprint must be a 64-character hex digest".to_string());
        }
        Ok(Self(text.to_ascii_lowercase()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    fn from_digest(bytes: impl AsRef<[u8]>) -> Self {
        Self(format!("{:x}", Sha256::digest(bytes)))
    }

    fn short(&self) -> &str {
        &self.0[..RUST_CARGO_NAME_DIGEST_LEN]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RustCargoName(String);

impl RustCargoName {
    pub fn parse(text: impl Into<String>) -> Result<Self, String> {
        let text = text.into();
        if !valid_cargo_name(&text) {
            return Err(format!("invalid Cargo name `{text}`"));
        }
        Ok(Self(text))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for RustCargoName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

fn single_package_name(fingerprint: &RustCargoFingerprint) -> RustCargoName {
    generated_name("anvyx_gen", fingerprint)
}

fn batch_package_name(fingerprint: &RustCargoFingerprint) -> RustCargoName {
    generated_name("anvyx_test_batch", fingerprint)
}

pub fn batch_case_name(fingerprint: &RustCargoFingerprint) -> RustCargoName {
    generated_name("case", fingerprint)
}

fn generated_name(prefix: &str, fingerprint: &RustCargoFingerprint) -> RustCargoName {
    RustCargoName::parse(format!("{prefix}_{}", fingerprint.short()))
        .expect("generated Cargo name is valid")
}

fn valid_cargo_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    first.is_ascii_alphabetic() && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn host_binary_name(name: &RustCargoName) -> String {
    host_executable_name(name.as_str())
}

pub fn host_executable_name(name: &str) -> String {
    format!("{name}{}", std::env::consts::EXE_SUFFIX)
}

pub fn cargo_fingerprint(input: &RustCargoFingerprintInput<'_>) -> RustCargoFingerprint {
    let mut writer = FingerprintWriter::default();
    writer.field(RUST_CARGO_FINGERPRINT_SCHEMA);
    writer.field(input.source);
    writer.field(input.manifest_template);
    writer.field(input.semantic_profile);
    writer.field(input.cargo_profile.name());

    let mut dependencies = input.dependencies.to_vec();
    dependencies.sort_by_key(RustCargoDependency::sort_key);
    for dep in dependencies {
        writer.field(dep.name.as_str());
        writer.field(dep.package.as_ref().map_or("", RustCargoName::as_str));
        match &dep.source {
            RustCargoDependencySource::Registry { version } => {
                writer.field("version");
                writer.field(version);
            }
            RustCargoDependencySource::Path(path) => {
                writer.field("path");
                writer.field(path);
            }
        }
        writer.field(if dep.default_features {
            "default"
        } else {
            "no-default"
        });
        let mut features = dep.features.clone();
        features.sort();
        features.dedup();
        for feature in features {
            writer.field(&feature);
        }
    }

    writer.finish()
}

#[derive(Debug, Clone, Copy)]
pub struct RustCargoFingerprintInput<'a> {
    pub source: &'a str,
    pub manifest_template: &'a str,
    pub semantic_profile: &'a str,
    pub cargo_profile: RustCargoProfile,
    pub dependencies: &'a [RustCargoDependency],
}

#[derive(Default)]
struct FingerprintWriter {
    bytes: Vec<u8>,
}

impl FingerprintWriter {
    fn field(&mut self, text: &str) {
        let len = u64::try_from(text.len()).expect("fingerprint field length fits in u64");
        self.bytes.extend_from_slice(&len.to_le_bytes());
        self.bytes.extend_from_slice(text.as_bytes());
    }

    fn finish(self) -> RustCargoFingerprint {
        RustCargoFingerprint::from_digest(self.bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fp() -> RustCargoFingerprint {
        RustCargoFingerprint::parse(
            "abcdef0123456789fedcba9876543210abcdef0123456789fedcba9876543210",
        )
        .unwrap()
    }

    fn name(text: &str) -> RustCargoName {
        RustCargoName::parse(text).unwrap()
    }

    fn dep(name_text: &str, version: &str) -> RustCargoDependency {
        RustCargoDependency {
            name: name(name_text),
            package: None,
            source: RustCargoDependencySource::Registry {
                version: version.to_string(),
            },
            features: vec![],
            default_features: true,
        }
    }

    fn fingerprint(
        source: &str,
        manifest_template: &str,
        semantic_profile: &str,
        cargo_profile: RustCargoProfile,
        dependencies: &[RustCargoDependency],
    ) -> RustCargoFingerprint {
        cargo_fingerprint(&RustCargoFingerprintInput {
            source,
            manifest_template,
            semantic_profile,
            cargo_profile,
            dependencies,
        })
    }

    #[test]
    fn layout_paths_are_centralized() {
        let layout = RustCargoLayout::new(PathBuf::from("cache"), fp());
        let digest = fp().as_str().to_string();

        assert_eq!(
            layout.crate_dir(),
            PathBuf::from(format!("cache/crates/{digest}"))
        );
        assert_eq!(
            layout.manifest_path(),
            PathBuf::from(format!("cache/crates/{digest}/Cargo.toml"))
        );
        assert_eq!(
            layout.source_path(),
            PathBuf::from(format!("cache/crates/{digest}/src/main.rs"))
        );
        assert_eq!(layout.target_dir(), PathBuf::from("cache/target"));
        assert_eq!(
            layout.lock_path(),
            PathBuf::from(format!("cache/locks/{digest}.lock"))
        );
        assert_eq!(
            layout.batch_case_source_path(&name("case_deadbeef")),
            PathBuf::from(format!("cache/crates/{digest}/src/bin/case_deadbeef.rs"))
        );
    }

    #[test]
    fn binary_paths_use_profile_and_host_suffix() {
        let layout = RustCargoLayout::new(PathBuf::from("cache"), fp());
        let bin = name("anvyx_gen_abcdef");
        let suffix = std::env::consts::EXE_SUFFIX;

        assert_eq!(
            layout.binary_path(RustCargoProfile::Dev, &bin),
            PathBuf::from(format!("cache/target/debug/anvyx_gen_abcdef{suffix}"))
        );
        assert_eq!(
            layout.binary_path(RustCargoProfile::Release, &bin),
            PathBuf::from(format!("cache/target/release/anvyx_gen_abcdef{suffix}"))
        );
    }

    #[test]
    fn generated_names_are_deterministic_and_cargo_safe() {
        let fingerprint = fp();

        for name in [
            single_package_name(&fingerprint),
            batch_package_name(&fingerprint),
            batch_case_name(&fingerprint),
        ] {
            assert!(valid_cargo_name(name.as_str()), "{name}");
            assert!(name.as_str().ends_with("abcdef0123456789"));
        }
        assert!(RustCargoName::parse("").is_err());
        assert!(RustCargoName::parse("1bad").is_err());
        assert!(RustCargoName::parse("bad-name").is_err());
        assert!(RustCargoFingerprint::parse("not-hex").is_err());
    }

    #[test]
    fn fingerprint_is_stable_for_identical_inputs() {
        let deps = [dep("serde", "1")];

        assert_eq!(
            fingerprint(
                "fn main() {}",
                "package-template",
                "debug",
                RustCargoProfile::Dev,
                &deps
            ),
            fingerprint(
                "fn main() {}",
                "package-template",
                "debug",
                RustCargoProfile::Dev,
                &deps
            )
        );
    }

    #[test]
    fn fingerprint_changes_for_source_manifest_template_and_profiles() {
        let base = fingerprint("a", "m", "debug", RustCargoProfile::Dev, &[]);

        assert_ne!(
            base,
            fingerprint("b", "m", "debug", RustCargoProfile::Dev, &[])
        );
        assert_ne!(
            base,
            fingerprint("a", "n", "debug", RustCargoProfile::Dev, &[])
        );
        assert_ne!(
            base,
            fingerprint("a", "m", "release", RustCargoProfile::Dev, &[])
        );
        assert_ne!(
            base,
            fingerprint("a", "m", "debug", RustCargoProfile::Release, &[])
        );
    }

    #[test]
    fn fingerprint_sorts_dependencies_and_excludes_cache_paths() {
        let a = dep("a", "1");
        let b = dep("b", "1");

        assert_eq!(
            fingerprint(
                "src",
                "manifest",
                "debug",
                RustCargoProfile::Dev,
                &[a.clone(), b.clone()]
            ),
            fingerprint("src", "manifest", "debug", RustCargoProfile::Dev, &[b, a])
        );
    }

    #[test]
    fn fingerprint_changes_for_dependency_specs() {
        assert_ne!(
            fingerprint(
                "src",
                "manifest",
                "debug",
                RustCargoProfile::Dev,
                &[dep("a", "1")]
            ),
            fingerprint(
                "src",
                "manifest",
                "debug",
                RustCargoProfile::Dev,
                &[dep("a", "2")]
            )
        );
    }

    #[test]
    fn renders_single_package_manifest() {
        let package = RustPackageSpec {
            package_name: name("anvyx_gen_abcdef"),
            binary_name: name("anvyx_gen_abcdef"),
            dependencies: vec![],
        };

        assert_eq!(
            render_manifest(&package),
            "[package]\nname = \"anvyx_gen_abcdef\"\nversion = \"0.0.0\"\nedition = \"2024\"\npublish = false\n\n[workspace]\n\n[[bin]]\nname = \"anvyx_gen_abcdef\"\npath = \"src/main.rs\"\n\n[profile.dev]\nopt-level = 0\n\n[profile.release]\nopt-level = 3\n"
        );
    }

    #[test]
    fn renders_dependencies_deterministically() {
        let package = RustPackageSpec {
            package_name: name("anvyx_gen_abcdef"),
            binary_name: name("anvyx_gen_abcdef"),
            dependencies: vec![
                RustCargoDependency {
                    name: name("z_dep"),
                    package: Some(name("real_z")),
                    source: RustCargoDependencySource::Path("../native crate".to_string()),
                    features: vec!["b".to_string(), "a".to_string(), "a".to_string()],
                    default_features: false,
                },
                dep("a_dep", "1.2.3"),
            ],
        };
        let mut package = package;
        package.dependencies = normalized_dependencies(package.dependencies);

        assert_eq!(
            render_manifest(&package),
            "[package]\nname = \"anvyx_gen_abcdef\"\nversion = \"0.0.0\"\nedition = \"2024\"\npublish = false\n\n[workspace]\n\n[[bin]]\nname = \"anvyx_gen_abcdef\"\npath = \"src/main.rs\"\n\n[profile.dev]\nopt-level = 0\n\n[profile.release]\nopt-level = 3\n\n[dependencies]\na_dep = { version = \"1.2.3\" }\nz_dep = { path = \"../native crate\", package = \"real_z\", default-features = false, features = [\"a\", \"b\"] }\n"
        );
    }

    #[test]
    fn cargo_job_builds_and_runs_binary() {
        let cache = tempfile::tempdir().unwrap();
        let job = single_program_job(
            RustSource::new("fn main() { println!(\"ok\"); }\n".to_string()),
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            RustCargoMode::Run,
            "debug",
        );

        let output = execute(&job).unwrap();
        let RustCargoOutput::Success(output) = output else {
            panic!("expected success: {output:?}");
        };

        assert_eq!(output.stdout, "ok\n");
        assert!(output.manifest_path.exists());
        assert!(output.source_path.exists());
        assert!(output.binary_path.exists());
    }

    #[test]
    fn cargo_job_builds_with_path_dependency() {
        let cache = tempfile::tempdir().unwrap();
        let dep_dir = tempfile::tempdir().unwrap();
        fs::write(
            dep_dir.path().join("Cargo.toml"),
            "[package]\nname = \"dep_crate\"\nversion = \"0.0.0\"\nedition = \"2024\"\n",
        )
        .unwrap();
        fs::create_dir(dep_dir.path().join("src")).unwrap();
        fs::write(
            dep_dir.path().join("src/lib.rs"),
            "pub fn value() -> i32 { 42 }\n",
        )
        .unwrap();
        let dep_path = dep_dir.path().to_string_lossy().into_owned();
        let job = single_program_job_with_dependencies(
            RustSource::new("fn main() { println!(\"{}\", dep_crate::value()); }\n".to_string()),
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            RustCargoMode::Run,
            "debug",
            vec![RustCargoDependency {
                name: name("dep_crate"),
                package: None,
                source: RustCargoDependencySource::Path(dep_path),
                features: vec![],
                default_features: true,
            }],
        );

        let output = execute(&job).unwrap();
        let RustCargoOutput::Success(output) = output else {
            panic!("expected success: {output:?}");
        };

        assert_eq!(output.stdout, "42\n");

        fs::write(
            dep_dir.path().join("src/lib.rs"),
            "pub fn value() -> i32 { 7 }\n",
        )
        .unwrap();
        let output = execute(&job).unwrap();
        let RustCargoOutput::Success(output) = output else {
            panic!("expected rebuild success: {output:?}");
        };

        assert_eq!(output.stdout, "7\n");
    }

    #[test]
    fn cargo_batch_job_builds_binaries() {
        let cache = tempfile::tempdir().unwrap();
        let job = batch_job(
            vec![
                RustCargoBatchCase {
                    name: name("case_0"),
                    source: RustSource::new("fn main() { println!(\"a\"); }\n".to_string()),
                },
                RustCargoBatchCase {
                    name: name("case_1"),
                    source: RustSource::new("fn main() { println!(\"b\"); }\n".to_string()),
                },
            ],
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            "debug",
        );

        let output = execute_batch_with_timeout(&job, None).unwrap();
        let RustCargoBatchOutput::Success(output) = output else {
            panic!("expected batch success: {output:?}");
        };

        assert_eq!(output.binaries.len(), 2);
        for (_, binary) in output.binaries {
            assert!(binary.exists());
        }
    }

    #[test]
    fn unchanged_generated_files_are_not_rewritten() {
        let cache = tempfile::tempdir().unwrap();
        let job = single_program_job(
            RustSource::new("fn main() { println!(\"ok\"); }\n".to_string()),
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            RustCargoMode::Build,
            "debug",
        );

        let RustCargoOutput::Success(first) = execute(&job).unwrap() else {
            panic!("expected first build success");
        };
        let manifest_time = fs::metadata(&first.manifest_path)
            .unwrap()
            .modified()
            .unwrap();
        let source_time = fs::metadata(&first.source_path)
            .unwrap()
            .modified()
            .unwrap();
        thread::sleep(Duration::from_millis(20));
        execute(&job).unwrap();

        assert_eq!(
            fs::metadata(&first.manifest_path)
                .unwrap()
                .modified()
                .unwrap(),
            manifest_time
        );
        assert_eq!(
            fs::metadata(&first.source_path)
                .unwrap()
                .modified()
                .unwrap(),
            source_time
        );
    }
}
