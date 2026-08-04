use std::{
    error::Error,
    fmt::{self, Write as FmtWrite},
    fs,
    fs::OpenOptions,
    io::Read,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    thread,
    time::{Duration, Instant},
};

use sha2::{Digest, Sha256};

use super::emit::RustSource;

pub const RUST_CARGO_FINGERPRINT_SCHEMA: &str = "anvyx-cargo-v1";
pub const RUST_CARGO_IDENTITY_SCHEMA: &str = "anvyx-cargo-id-v1";
pub const RUST_CARGO_NAME_DIGEST_LEN: usize = 16;
pub const DEFAULT_CARGO_EDITION: &str = "2024";
pub const DEFAULT_CARGO_VERSION: &str = "0.0.0";

#[derive(Debug, Clone)]
pub struct RustCargoJob {
    source: RustSource,
    package: RustPackageSpec,
    mode: RustCargoMode,
    profile: RustCargoProfile,
    cache_root: PathBuf,
    crate_identity: RustCargoCrateIdentity,
}

#[derive(Debug, Clone)]
pub struct RustCargoBatchJob {
    cases: Vec<RustCargoBatchCase>,
    package: RustPackageSpec,
    profile: RustCargoProfile,
    cache_root: PathBuf,
    crate_identity: RustCargoCrateIdentity,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCargoJobPaths {
    pub manifest_path: PathBuf,
    pub source_path: PathBuf,
    pub target_dir: PathBuf,
    pub binary_path: PathBuf,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustCargoMode {
    Build,
    Run,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustCargoEvent {
    Compiling,
    Running,
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
    metadata: RustCargoPackageMetadata,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCargoPackageMetadata {
    pub edition: String,
    pub version: String,
}

impl Default for RustCargoPackageMetadata {
    fn default() -> Self {
        Self {
            edition: DEFAULT_CARGO_EDITION.to_string(),
            version: DEFAULT_CARGO_VERSION.to_string(),
        }
    }
}

const SINGLE_MANIFEST_TEMPLATE: &str = include_str!("templates/cargo_single_manifest.toml.in");
const BATCH_MANIFEST_TEMPLATE: &str = include_str!("templates/cargo_batch_manifest.toml.in");

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustCargoDependency {
    pub name: RustCargoName,
    pub package: Option<RustCargoPackageName>,
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
            "{}|{}|{source}|{}|{}",
            self.name.as_str(),
            self.package
                .as_ref()
                .map_or("", RustCargoPackageName::as_str),
            self.default_features,
            self.features.join(",")
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
    single_program_job_configured(
        source,
        cache_root,
        profile,
        mode,
        semantic_profile,
        dependencies,
        RustCargoPackageMetadata::default(),
        None,
    )
}

pub fn single_program_job_configured(
    source: RustSource,
    cache_root: PathBuf,
    profile: RustCargoProfile,
    mode: RustCargoMode,
    semantic_profile: &str,
    dependencies: Vec<RustCargoDependency>,
    metadata: RustCargoPackageMetadata,
    crate_identity: Option<RustCargoCrateIdentity>,
) -> RustCargoJob {
    let dependencies = normalized_dependencies(dependencies);
    let crate_identity = crate_identity.unwrap_or_else(|| {
        let fingerprint = cargo_fingerprint(&RustCargoFingerprintInput {
            source: source.as_str(),
            manifest_template: SINGLE_MANIFEST_TEMPLATE,
            semantic_profile,
            cargo_profile: profile,
            dependencies: &dependencies,
        });
        RustCargoCrateIdentity(fingerprint.0)
    });
    let name = single_package_name(&crate_identity);
    RustCargoJob {
        source,
        package: RustPackageSpec {
            package_name: name.clone(),
            binary_name: name,
            dependencies,
            metadata,
        },
        mode,
        profile,
        cache_root,
        crate_identity,
    }
}

pub fn batch_job(
    cases: Vec<RustCargoBatchCase>,
    cache_root: PathBuf,
    profile: RustCargoProfile,
    semantic_profile: &str,
) -> RustCargoBatchJob {
    batch_job_with_dependencies(cases, cache_root, profile, semantic_profile, vec![])
}

impl RustCargoJob {
    pub fn paths(&self) -> RustCargoJobPaths {
        let layout = RustCargoLayout::new(self.cache_root.clone(), self.crate_identity.clone());
        RustCargoJobPaths {
            manifest_path: layout.manifest_path(),
            source_path: layout.source_path(),
            target_dir: layout.target_dir(),
            binary_path: layout.binary_path(self.profile, &self.package.binary_name),
        }
    }
}

pub fn batch_job_with_dependencies(
    cases: Vec<RustCargoBatchCase>,
    cache_root: PathBuf,
    profile: RustCargoProfile,
    semantic_profile: &str,
    dependencies: Vec<RustCargoDependency>,
) -> RustCargoBatchJob {
    let dependencies = normalized_dependencies(dependencies);
    let mut writer = FingerprintWriter::default();
    writer.field(RUST_CARGO_FINGERPRINT_SCHEMA);
    writer.field(BATCH_MANIFEST_TEMPLATE);
    writer.field(semantic_profile);
    writer.field(profile.name());
    fingerprint_dependencies(&mut writer, &dependencies);
    let mut cases = cases;
    cases.sort_by(|left, right| left.name.as_str().cmp(right.name.as_str()));
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
            dependencies,
            metadata: RustCargoPackageMetadata::default(),
        },
        profile,
        cache_root,
        crate_identity: RustCargoCrateIdentity(fingerprint.0.clone()),
    }
}

pub fn execute(job: &RustCargoJob) -> Result<RustCargoOutput, RustCargoError> {
    execute_with_events(job, |_| {})
}

pub fn execute_with_events(
    job: &RustCargoJob,
    events: impl FnMut(RustCargoEvent),
) -> Result<RustCargoOutput, RustCargoError> {
    execute_with_events_and_timeout(job, None, events)
}

pub fn execute_with_timeout(
    job: &RustCargoJob,
    timeout: Duration,
) -> Result<RustCargoOutput, RustCargoError> {
    execute_with_events_and_timeout(job, Some(timeout), |_| {})
}

pub fn execute_with_events_and_timeout(
    job: &RustCargoJob,
    timeout: Option<Duration>,
    mut events: impl FnMut(RustCargoEvent),
) -> Result<RustCargoOutput, RustCargoError> {
    validate_package(&job.package)?;
    let started = Instant::now();
    let layout = RustCargoLayout::new(job.cache_root.clone(), job.crate_identity.clone());
    with_lock(
        layout.lock_path(),
        remaining_timeout(started, timeout)?,
        || {
            write_single_package(job, &layout)?;

            let cargo_timeout = remaining_timeout(started, timeout)?;
            events(RustCargoEvent::Compiling);
            let cargo = build_cargo_output(job.profile, &layout, cargo_timeout)?;

            if !cargo.status.success() {
                return Ok(RustCargoOutput::CargoFailed(failure(
                    &layout,
                    cargo.status.code(),
                    &cargo.stdout,
                    &cargo.stderr,
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
                    &cargo.stdout,
                    &cargo.stderr,
                ))),
                RustCargoMode::Run => {
                    let run_timeout = remaining_timeout(started, timeout)?;
                    events(RustCargoEvent::Running);
                    let run = command_output(Command::new(&binary_path), run_timeout)?;
                    if !run.status.success() {
                        return Ok(RustCargoOutput::RunFailed(failure(
                            &layout,
                            run.status.code(),
                            &run.stdout,
                            &run.stderr,
                        )));
                    }
                    Ok(RustCargoOutput::Success(success(
                        &layout,
                        binary_path,
                        &run.stdout,
                        &run.stderr,
                    )))
                }
            }
        },
    )
}

pub fn execute_batch_with_timeout(
    job: &RustCargoBatchJob,
    timeout: Option<Duration>,
) -> Result<RustCargoBatchOutput, RustCargoError> {
    validate_package(&job.package)?;
    validate_batch_cases(&job.cases)?;
    let started = Instant::now();
    let layout = RustCargoLayout::new(job.cache_root.clone(), job.crate_identity.clone());
    with_lock(
        layout.lock_path(),
        remaining_timeout(started, timeout)?,
        || {
            write_batch_package(job, &layout)?;

            let cargo =
                build_cargo_output(job.profile, &layout, remaining_timeout(started, timeout)?)?;

            if !cargo.status.success() {
                return Ok(RustCargoBatchOutput::CargoFailed(failure(
                    &layout,
                    cargo.status.code(),
                    &cargo.stdout,
                    &cargo.stderr,
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
        },
    )
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
    let mut bin_sections = String::new();
    for case in cases {
        write!(
            bin_sections,
            "\n[[bin]]\nname = \"{}\"\npath = \"src/bin/{}.rs\"\n",
            case.name.as_str(),
            case.name.as_str()
        )
        .expect("write to string succeeds");
    }
    render_template(
        BATCH_MANIFEST_TEMPLATE,
        &[
            ("package_name", package.package_name.as_str()),
            ("version", &package.metadata.version),
            ("edition", &package.metadata.edition),
            ("bin_sections", &bin_sections),
            ("dependencies", &render_dependencies(&package.dependencies)),
        ],
    )
}

fn render_manifest(package: &RustPackageSpec) -> String {
    render_template(
        SINGLE_MANIFEST_TEMPLATE,
        &[
            ("package_name", package.package_name.as_str()),
            ("version", &package.metadata.version),
            ("edition", &package.metadata.edition),
            ("binary_name", package.binary_name.as_str()),
            ("dependencies", &render_dependencies(&package.dependencies)),
        ],
    )
}

fn render_dependencies(dependencies: &[RustCargoDependency]) -> String {
    if dependencies.is_empty() {
        return String::new();
    }
    let mut text = "\n[dependencies]\n".to_string();
    for dep in dependencies {
        text.push_str(&render_dependency(dep));
    }
    text
}

fn render_template(template: &str, fields: &[(&str, &str)]) -> String {
    let mut text = template.to_string();
    for (name, value) in fields {
        text = text.replace(&format!("{{{{{name}}}}}"), value);
    }
    text
}

fn validate_package(package: &RustPackageSpec) -> Result<(), RustCargoError> {
    validate_metadata(&package.metadata)?;
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

fn validate_metadata(metadata: &RustCargoPackageMetadata) -> Result<(), RustCargoError> {
    if !matches!(metadata.edition.as_str(), "2015" | "2018" | "2021" | "2024") {
        return Err(RustCargoError::InvalidJob(format!(
            "unsupported Cargo edition `{}`",
            metadata.edition
        )));
    }
    if !valid_version(&metadata.version) {
        return Err(RustCargoError::InvalidJob(format!(
            "invalid Cargo package version `{}`",
            metadata.version
        )));
    }
    Ok(())
}

fn valid_version(version: &str) -> bool {
    let parts = version.split('.').collect::<Vec<_>>();
    parts.len() == 3 && parts.iter().all(|part| valid_version_number(part))
}

fn valid_version_number(part: &str) -> bool {
    !part.is_empty()
        && (part == "0" || !part.starts_with('0'))
        && part.bytes().all(|b| b.is_ascii_digit())
        && part.parse::<u64>().is_ok()
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
    deps.sort_by_key(|dep| dep.sort_key());
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
    if let Some(package) = &dep.package {
        fields.push(format!("package = \"{}\"", toml_escape(package.as_str())));
    }
    match &dep.source {
        RustCargoDependencySource::Registry { version } => {
            fields.push(format!("version = \"{}\"", toml_escape(version)));
        }
        RustCargoDependencySource::Path(path) => {
            fields.push(format!("path = \"{}\"", toml_escape(path)));
        }
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

fn build_cargo_output(
    profile: RustCargoProfile,
    layout: &RustCargoLayout,
    timeout: Option<Duration>,
) -> Result<std::process::Output, RustCargoError> {
    let mut cargo = Command::new("cargo");
    cargo
        .arg("build")
        .args(profile.build_args())
        .arg("--manifest-path")
        .arg(layout.manifest_path())
        .arg("--target-dir")
        .arg(layout.target_dir())
        .env_remove("CARGO_TARGET_DIR");
    cargo_output(cargo, timeout)
}

fn cargo_output(
    command: Command,
    timeout: Option<Duration>,
) -> Result<std::process::Output, RustCargoError> {
    command_output(command, timeout).map_err(|error| match error {
        RustCargoError::Io(error) if error.kind() == std::io::ErrorKind::NotFound => {
            RustCargoError::CargoUnavailable
        }
        error => error,
    })
}

fn command_output(
    mut command: Command,
    timeout: Option<Duration>,
) -> Result<std::process::Output, RustCargoError> {
    let Some(timeout) = timeout else {
        return command.output().map_err(RustCargoError::Io);
    };
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = command.spawn().map_err(RustCargoError::Io)?;
    let stdout = child
        .stdout
        .take()
        .ok_or_else(|| RustCargoError::Io(std::io::Error::other("missing child stdout")))?;
    let stderr = child
        .stderr
        .take()
        .ok_or_else(|| RustCargoError::Io(std::io::Error::other("missing child stderr")))?;
    let stdout_reader = spawn_output_reader(stdout);
    let stderr_reader = spawn_output_reader(stderr);

    let start = Instant::now();
    let status = loop {
        if let Some(status) = child.try_wait()? {
            break status;
        }
        if start.elapsed() >= timeout {
            let _ = child.kill();
            let _ = child.wait();
            let _ = join_output_reader(stdout_reader);
            let _ = join_output_reader(stderr_reader);
            return Err(RustCargoError::Timeout);
        }
        thread::sleep(LOCK_POLL);
    };

    Ok(std::process::Output {
        status,
        stdout: join_output_reader(stdout_reader)?,
        stderr: join_output_reader(stderr_reader)?,
    })
}

fn spawn_output_reader<R: Read + Send + 'static>(
    mut reader: R,
) -> thread::JoinHandle<std::io::Result<Vec<u8>>> {
    thread::spawn(move || {
        let mut output = vec![];
        reader.read_to_end(&mut output)?;
        Ok(output)
    })
}

fn join_output_reader(
    reader: thread::JoinHandle<std::io::Result<Vec<u8>>>,
) -> Result<Vec<u8>, RustCargoError> {
    reader
        .join()
        .map_err(|_| RustCargoError::Io(std::io::Error::other("output reader thread panicked")))?
        .map_err(RustCargoError::Io)
}

fn success(
    layout: &RustCargoLayout,
    binary_path: PathBuf,
    stdout: &[u8],
    stderr: &[u8],
) -> RustCargoSuccess {
    RustCargoSuccess {
        manifest_path: layout.manifest_path(),
        source_path: layout.source_path(),
        target_dir: layout.target_dir(),
        binary_path,
        stdout: String::from_utf8_lossy(stdout).into_owned(),
        stderr: String::from_utf8_lossy(stderr).into_owned(),
    }
}

fn failure(
    layout: &RustCargoLayout,
    status: Option<i32>,
    stdout: &[u8],
    stderr: &[u8],
) -> RustCargoFailure {
    RustCargoFailure {
        manifest_path: layout.manifest_path(),
        source_path: layout.source_path(),
        target_dir: layout.target_dir(),
        status,
        stdout: String::from_utf8_lossy(stdout).into_owned(),
        stderr: String::from_utf8_lossy(stderr).into_owned(),
    }
}

const LOCK_POLL: Duration = Duration::from_millis(25);

pub fn with_lock<T>(
    path: PathBuf,
    timeout: Option<Duration>,
    f: impl FnOnce() -> Result<T, RustCargoError>,
) -> Result<T, RustCargoError> {
    let dir = path.parent().expect("lock path has parent");
    fs::create_dir_all(dir)?;
    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(&path)?;
    let mut lock = fd_lock::RwLock::new(file);

    let Some(timeout) = timeout else {
        let _guard = lock.write().map_err(RustCargoError::Io)?;
        return f();
    };

    let started = Instant::now();
    loop {
        match lock.try_write() {
            Ok(_guard) => return f(),
            Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {
                if started.elapsed() >= timeout {
                    return Err(RustCargoError::LockTimeout(path));
                }
                thread::sleep(LOCK_POLL);
            }
            Err(error) => return Err(RustCargoError::Io(error)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RustCargoLayout {
    cache_root: PathBuf,
    crate_identity: RustCargoCrateIdentity,
}

impl RustCargoLayout {
    fn new(cache_root: PathBuf, crate_identity: RustCargoCrateIdentity) -> Self {
        Self {
            cache_root,
            crate_identity,
        }
    }

    fn crate_dir(&self) -> PathBuf {
        self.cache_root
            .join("crates")
            .join(self.crate_identity.as_str())
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
            .join(format!("{}.lock", self.crate_identity.as_str()))
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
pub struct RustCargoCrateIdentity(String);

impl RustCargoCrateIdentity {
    pub fn parse(text: impl Into<String>) -> Result<Self, String> {
        RustCargoFingerprint::parse(text).map(|fingerprint| Self(fingerprint.0))
    }

    pub fn as_str(&self) -> &str {
        &self.0
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RustCargoPackageName(String);

impl RustCargoPackageName {
    pub fn parse(text: impl Into<String>) -> Result<Self, String> {
        let text = text.into();
        if !valid_cargo_package_name(&text) {
            return Err(format!("invalid Cargo package name `{text}`"));
        }
        Ok(Self(text))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for RustCargoPackageName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

fn single_package_name(identity: &RustCargoCrateIdentity) -> RustCargoName {
    generated_name("anvyx_gen", identity.short())
}

fn batch_package_name(fingerprint: &RustCargoFingerprint) -> RustCargoName {
    generated_name("anvyx_test_batch", fingerprint.short())
}

pub fn batch_case_name(fingerprint: &RustCargoFingerprint) -> RustCargoName {
    generated_name("case", fingerprint.short())
}

fn generated_name(prefix: &str, suffix: &str) -> RustCargoName {
    RustCargoName::parse(format!("{prefix}_{suffix}")).expect("generated Cargo name is valid")
}

fn valid_cargo_name(name: &str) -> bool {
    valid_cargo_identifier(name, false)
}

fn valid_cargo_package_name(name: &str) -> bool {
    valid_cargo_identifier(name, true)
}

fn valid_cargo_identifier(name: &str, allow_hyphen: bool) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    first.is_ascii_alphabetic()
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || allow_hyphen && c == '-')
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

    fingerprint_dependencies(&mut writer, input.dependencies);
    writer.finish()
}

pub fn single_program_crate_identity(
    input: &RustCargoCrateIdentityInput<'_>,
) -> RustCargoCrateIdentity {
    let mut writer = FingerprintWriter::default();
    writer.field(RUST_CARGO_IDENTITY_SCHEMA);
    writer.field(input.seed);
    writer.field(SINGLE_MANIFEST_TEMPLATE);
    writer.field(input.semantic_profile);
    writer.field(input.cargo_profile.name());
    fingerprint_dependencies(&mut writer, input.dependencies);
    RustCargoCrateIdentity(writer.finish().0)
}

fn fingerprint_dependencies(writer: &mut FingerprintWriter, dependencies: &[RustCargoDependency]) {
    for dep in sorted_dependencies(dependencies) {
        writer.field(dep.name.as_str());
        writer.field(
            dep.package
                .as_ref()
                .map_or("", RustCargoPackageName::as_str),
        );
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
}

#[derive(Debug, Clone, Copy)]
pub struct RustCargoFingerprintInput<'a> {
    pub source: &'a str,
    pub manifest_template: &'a str,
    pub semantic_profile: &'a str,
    pub cargo_profile: RustCargoProfile,
    pub dependencies: &'a [RustCargoDependency],
}

#[derive(Debug, Clone, Copy)]
pub struct RustCargoCrateIdentityInput<'a> {
    pub seed: &'a str,
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
    fn name(text: &str) -> RustCargoName {
        RustCargoName::parse(text).unwrap()
    }
    #[test]
    fn rejects_invalid_package_metadata() {
        let mut package = RustPackageSpec {
            package_name: name("anvyx_gen_abcdef"),
            binary_name: name("anvyx_gen_abcdef"),
            dependencies: vec![],
            metadata: RustCargoPackageMetadata::default(),
        };

        package.metadata.version = "01.0.0".to_string();
        assert!(matches!(
            validate_package(&package),
            Err(RustCargoError::InvalidJob(_))
        ));

        package.metadata.version = DEFAULT_CARGO_VERSION.to_string();
        package.metadata.edition = "2030".to_string();
        assert!(matches!(
            validate_package(&package),
            Err(RustCargoError::InvalidJob(_))
        ));
    }

    #[test]
    fn existing_lock_file_does_not_block_after_guard_drops() {
        let cache = tempfile::tempdir().unwrap();
        let path = cache.path().join("job.lock");
        fs::write(&path, "stale sentinel\n").unwrap();

        with_lock(path.clone(), Some(Duration::ZERO), || Ok(())).unwrap();

        assert!(path.exists());
    }

    #[test]
    fn live_lock_times_out_second_acquisition() {
        let cache = tempfile::tempdir().unwrap();
        let path = cache.path().join("job.lock");

        with_lock(path.clone(), Some(Duration::from_millis(250)), || {
            assert!(matches!(
                with_lock(path.clone(), Some(Duration::ZERO), || Ok(())),
                Err(RustCargoError::LockTimeout(_))
            ));
            Ok(())
        })
        .unwrap();
    }

    #[cfg(unix)]
    #[test]
    fn command_output_drains_piped_streams_while_child_runs() {
        let mut command = Command::new("sh");
        command.arg("-c").arg(
            "i=0; \
             while [ \"$i\" -lt 4096 ]; do \
                 printf '0123456789abcdef0123456789abcdef\\n'; \
                 printf 'fedcba9876543210fedcba9876543210\\n' >&2; \
                 i=$((i + 1)); \
             done",
        );

        let output = cargo_output(command, Some(Duration::from_secs(5))).unwrap();

        assert!(output.status.success());
        assert!(output.stdout.len() > 100_000);
        assert!(output.stderr.len() > 100_000);
    }

    #[cfg(unix)]
    #[test]
    fn command_output_times_out_long_running_child() {
        let mut command = Command::new("sh");
        command.arg("-c").arg("sleep 10");

        assert!(matches!(
            cargo_output(command, Some(Duration::from_millis(50))),
            Err(RustCargoError::Timeout)
        ));
    }

    #[test]
    fn single_run_job_times_out() {
        let cache = tempfile::tempdir().unwrap();
        let source = RustSource::new(
            "fn main() { std::thread::sleep(std::time::Duration::from_secs(30)); }\n".to_string(),
        );
        let build = single_program_job(
            source.clone(),
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            RustCargoMode::Build,
            "debug",
        );
        execute(&build).unwrap();
        let run = single_program_job(
            source,
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            RustCargoMode::Run,
            "debug",
        );
        let mut events = vec![];

        assert!(matches!(
            execute_with_events_and_timeout(&run, Some(Duration::from_secs(5)), |event| {
                events.push(event);
            }),
            Err(RustCargoError::Timeout)
        ));
        assert_eq!(events, [RustCargoEvent::Compiling, RustCargoEvent::Running]);
    }

    #[test]
    fn stable_identity_reuses_crate_dir_across_source_changes() {
        let cache = tempfile::tempdir().unwrap();
        let crate_identity = single_program_crate_identity(&RustCargoCrateIdentityInput {
            seed: "project\0/root\0main.anv",
            semantic_profile: "debug",
            cargo_profile: RustCargoProfile::Dev,
            dependencies: &[],
        });
        let first = single_program_job_configured(
            RustSource::new("fn main() { println!(\"one\"); }\n".to_string()),
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            RustCargoMode::Build,
            "debug",
            vec![],
            RustCargoPackageMetadata::default(),
            Some(crate_identity.clone()),
        );
        let second = single_program_job_configured(
            RustSource::new("fn main() { println!(\"two\"); }\n".to_string()),
            cache.path().to_path_buf(),
            RustCargoProfile::Dev,
            RustCargoMode::Build,
            "debug",
            vec![],
            RustCargoPackageMetadata::default(),
            Some(crate_identity),
        );

        let first_layout =
            RustCargoLayout::new(cache.path().to_path_buf(), first.crate_identity.clone());
        let second_layout =
            RustCargoLayout::new(cache.path().to_path_buf(), second.crate_identity.clone());
        write_single_package(&first, &first_layout).unwrap();
        write_single_package(&second, &second_layout).unwrap();

        assert_eq!(first_layout.crate_dir(), second_layout.crate_dir());
        assert!(
            fs::read_to_string(second_layout.source_path())
                .unwrap()
                .contains("two")
        );
    }
}
