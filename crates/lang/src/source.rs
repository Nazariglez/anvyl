use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    fs, io,
    path::{Component, Path, PathBuf},
};

use anvyx_frontend::{
    pipeline::{
        PackageModuleInput, PackageSourceLoader, Source as FrontendSource, SourceLoad,
        SourceLoadError,
    },
    resolve::{LocalSourceRequest, ModuleId, ModulePath, PackageId, PackageKind, SourceFileId},
};

use crate::CheckError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceText {
    code: String,
    label: String,
    path: Option<PathBuf>,
}

impl SourceText {
    pub fn new(code: impl Into<String>, label: impl Into<String>) -> Result<Self, CheckError> {
        let label = label.into();
        validate_label(&label, "source label")?;

        Ok(Self {
            code: code.into(),
            label,
            path: None,
        })
    }

    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn label(&self) -> &str {
        &self.label
    }

    pub(crate) fn with_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub(crate) fn to_frontend_source(&self) -> FrontendSource {
        FrontendSource {
            code: self.code.clone(),
            label: self.label.clone(),
            path: self.path.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceOverride {
    path: PathBuf,
    code: String,
}

impl SourceOverride {
    pub fn new(path: impl Into<PathBuf>, code: impl Into<String>) -> Result<Self, CheckError> {
        let path = path.into();
        if path.as_os_str().is_empty() {
            return Err(CheckError::InvalidInput(
                "source override path must not be empty".to_string(),
            ));
        }
        Ok(Self {
            path,
            code: code.into(),
        })
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn code(&self) -> &str {
        &self.code
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSource {
    path: ModulePath,
    code: String,
    label: String,
}

impl ModuleSource {
    pub fn new(
        path: Vec<String>,
        code: impl Into<String>,
        label: impl Into<String>,
    ) -> Result<Self, CheckError> {
        let path = module_path(path)?;
        let label = label.into();
        validate_label(&label, "module source label")?;

        Ok(Self {
            path,
            code: code.into(),
            label,
        })
    }

    pub fn path(&self) -> &[String] {
        self.path.segments()
    }

    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn label(&self) -> &str {
        &self.label
    }

    pub(crate) fn to_frontend_source(&self) -> FrontendSource {
        FrontendSource {
            code: self.code.clone(),
            label: self.label.clone(),
            path: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SystemPackageSource {
    root: SourceText,
    modules: Vec<ModuleSource>,
}

impl SystemPackageSource {
    pub fn new(root: SourceText, modules: Vec<ModuleSource>) -> Result<Self, CheckError> {
        let mut paths = HashSet::new();
        for module in &modules {
            insert_unique_path(&mut paths, module.path(), "duplicate system module path")?;
        }
        Ok(Self { root, modules })
    }

    pub fn root(&self) -> &SourceText {
        &self.root
    }

    pub fn module(&self, path: &[String]) -> Option<&ModuleSource> {
        self.modules.iter().find(|module| module.path() == path)
    }

    pub fn modules(&self) -> &[ModuleSource] {
        &self.modules
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SourceBundle {
    core: Option<SystemPackageSource>,
    std: Option<SystemPackageSource>,
}

impl SourceBundle {
    pub fn new(core: Option<SystemPackageSource>, std: Option<SystemPackageSource>) -> Self {
        Self { core, std }
    }

    pub fn core(&self) -> Option<&SystemPackageSource> {
        self.core.as_ref()
    }

    pub fn std(&self) -> Option<&SystemPackageSource> {
        self.std.as_ref()
    }

    pub fn system_module(&self, package: &PackageId, path: &[String]) -> Option<&ModuleSource> {
        if package == &PackageId::core() {
            self.core()?.module(path)
        } else if package == &PackageId::std() {
            self.std()?.module(path)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageSource {
    id: PackageId,
    entry: Option<PathBuf>,
    source_root: Option<PathBuf>,
    dependencies: HashMap<String, PackageId>,
    kind: PackageKind,
}

impl PackageSource {
    pub fn new(
        id: PackageId,
        entry: impl Into<PathBuf>,
        source_root: impl Into<PathBuf>,
        dependencies: HashMap<String, PackageId>,
    ) -> Result<Self, CheckError> {
        let entry = entry.into();
        if entry.as_os_str().is_empty() {
            return invalid_input("package entry path must not be empty");
        }
        let source_root = source_root.into();
        if source_root.as_os_str().is_empty() {
            return invalid_input("package source root must not be empty");
        }
        Ok(Self {
            id,
            entry: Some(entry),
            source_root: Some(source_root),
            dependencies,
            kind: PackageKind::Source,
        })
    }

    pub fn native_only(id: PackageId, dependencies: HashMap<String, PackageId>) -> Self {
        Self {
            id,
            entry: None,
            source_root: None,
            dependencies,
            kind: PackageKind::NativeOnly,
        }
    }

    pub fn id(&self) -> &PackageId {
        &self.id
    }

    pub fn entry(&self) -> Option<&Path> {
        self.entry.as_deref()
    }

    pub fn source_root(&self) -> Option<&Path> {
        self.source_root.as_deref()
    }

    pub fn dependencies(&self) -> &HashMap<String, PackageId> {
        &self.dependencies
    }

    pub fn kind(&self) -> PackageKind {
        self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceOwner {
    Package(PackageId),
    None,
}

impl SourceOwner {
    fn package_context(&self) -> Option<&PackageId> {
        match self {
            Self::Package(package) => Some(package),
            Self::None => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SourceRoot {
    package: PackageId,
    root: PathBuf,
    canonical_root: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct SourceOwnership {
    roots: Vec<SourceRoot>,
}

impl SourceOwnership {
    pub(crate) fn new(packages: &[PackageSource]) -> Result<Self, CheckError> {
        let roots = packages
            .iter()
            .filter_map(|package| {
                package
                    .source_root()
                    .map(|source_root| (package.id(), source_root))
            })
            .map(|(package, source_root)| {
                let canonical_root = fs::canonicalize(source_root).map_err(|error| {
                    CheckError::InvalidInput(format!(
                        "failed to canonicalize package '{package}' source root '{}': {error}",
                        source_root.display()
                    ))
                })?;
                Ok(SourceRoot {
                    package: package.clone(),
                    root: source_root.to_path_buf(),
                    canonical_root,
                })
            })
            .collect::<Result<_, CheckError>>()?;
        Ok(Self { roots })
    }

    fn source_owner(&self, file: &SourceFileId) -> Result<SourceOwner, CheckError> {
        let owners = self
            .roots
            .iter()
            .filter(|root| file.path().starts_with(&root.canonical_root))
            .collect::<Vec<_>>();
        match owners.as_slice() {
            [] => Ok(SourceOwner::None),
            [owner] => Ok(SourceOwner::Package(owner.package.clone())),
            _ => Err(CheckError::InvalidInput(format!(
                "source file '{}' is owned by multiple package source roots: {}",
                file,
                owners
                    .iter()
                    .map(|root| format!("{} ({})", root.package, root.canonical_root.display()))
                    .collect::<Vec<_>>()
                    .join(", ")
            ))),
        }
    }

    pub(crate) fn validate_root_file(
        &self,
        file: &Path,
    ) -> Result<(PackageId, SourceFileId), CheckError> {
        let source_file = source_file_id(file)?;
        match self.source_owner(&source_file)? {
            SourceOwner::Package(package) => Ok((package, source_file)),
            SourceOwner::None => Err(CheckError::InvalidInput(format!(
                "file override '{source_file}' is outside every loaded package source root"
            ))),
        }
    }

    fn source_root(&self, package: &PackageId) -> Option<&Path> {
        self.roots
            .iter()
            .find(|root| &root.package == package)
            .map(|root| root.root.as_path())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct PackageSourceEnvironment<'a> {
    ownership: SourceOwnership,
    sources: &'a SourceBundle,
    source_cache: HashMap<SourceFileId, PackageModuleInput>,
}

impl<'a> PackageSourceEnvironment<'a> {
    pub(crate) fn new(ownership: SourceOwnership, sources: &'a SourceBundle) -> Self {
        Self {
            ownership,
            sources,
            source_cache: HashMap::new(),
        }
    }

    pub(crate) fn cache_sources(&mut self, sources: impl IntoIterator<Item = PackageModuleInput>) {
        for source in sources {
            if let Some(file) = source.module.source_file() {
                self.source_cache.insert(file.clone(), source);
            }
        }
    }

    pub(crate) fn cache_overrides(
        &mut self,
        sources: impl IntoIterator<Item = SourceOverride>,
    ) -> Result<(), CheckError> {
        for source in sources {
            let source_file = source_file_id(source.path())?;
            let owner = self.ownership.source_owner(&source_file)?;
            let source_text = SourceText::new(
                source.code().to_string(),
                source.path().display().to_string(),
            )?;
            self.source_cache.insert(
                source_file.clone(),
                PackageModuleInput {
                    module: ModuleId::source_with_context(
                        owner.package_context().cloned(),
                        source_file.clone(),
                    ),
                    source: source_text
                        .with_path(source_file.path().to_path_buf())
                        .to_frontend_source(),
                },
            );
        }
        Ok(())
    }

    fn load_source(&mut self, module: &ModuleId) -> Result<Option<PackageModuleInput>, CheckError> {
        let Some(path) = module.named_path() else {
            return Ok(None);
        };
        if let Some(source) = self
            .sources
            .system_module(module.package(), path.segments())
        {
            return Ok(Some(PackageModuleInput {
                module: module.clone(),
                source: source.to_frontend_source(),
            }));
        }
        let Some(source_root) = self.ownership.source_root(module.package()) else {
            return Ok(None);
        };
        let file = module_file(source_root, path.segments());
        self.read_module_file(file)
    }

    fn read_module_file(
        &mut self,
        file: PathBuf,
    ) -> Result<Option<PackageModuleInput>, CheckError> {
        let source_file = source_file_id(&file)?;
        if let Some(source) = self.source_cache.get(&source_file) {
            return Ok(Some(source.clone()));
        }
        match fs::read_to_string(&file) {
            Ok(code) => {
                let owner = self.ownership.source_owner(&source_file)?;
                let source = SourceText::new(code, file.display().to_string()).map(|source| {
                    PackageModuleInput {
                        module: ModuleId::source_with_context(
                            owner.package_context().cloned(),
                            source_file.clone(),
                        ),
                        source: source
                            .with_path(source_file.path().to_path_buf())
                            .to_frontend_source(),
                    }
                })?;
                self.source_cache.insert(source_file, source.clone());
                Ok(Some(source))
            }
            Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(None),
            Err(error) => Err(CheckError::ReadModule {
                path: file,
                message: error.to_string(),
            }),
        }
    }
}

impl PackageSourceLoader for PackageSourceEnvironment<'_> {
    type FatalError = CheckError;

    fn load(
        &mut self,
        module: &ModuleId,
    ) -> Result<Option<PackageModuleInput>, SourceLoadError<Self::FatalError>> {
        self.load_source(module).map_err(SourceLoadError::Fatal)
    }

    fn load_local_source(
        &mut self,
        request: &LocalSourceRequest,
    ) -> Result<SourceLoad, SourceLoadError<Self::FatalError>> {
        let file = local_source_file(request).map_err(SourceLoadError::Fatal)?;
        self.read_module_file(file.clone())
            .map(|source| match source {
                Some(source) => SourceLoad::Loaded(source),
                None => SourceLoad::Missing {
                    candidate: Some(file),
                },
            })
            .map_err(SourceLoadError::Fatal)
    }
}

fn local_source_file(request: &LocalSourceRequest) -> Result<PathBuf, CheckError> {
    let Some(mut dir) = request.importer.path().parent().map(Path::to_path_buf) else {
        return Err(CheckError::InvalidInput(format!(
            "source file '{}' has no parent directory",
            request.importer
        )));
    };
    for _ in 0..request.ascend {
        if !dir.pop() {
            return Err(CheckError::InvalidInput(format!(
                "source import '{}' from '{}' ascends above filesystem root",
                display_path(request.path.segments()),
                request.importer
            )));
        }
    }
    for segment in request.path.segments() {
        dir.push(segment);
    }
    dir.set_extension("anv");
    Ok(dir)
}

pub(crate) fn source_file_id(path: &Path) -> Result<SourceFileId, CheckError> {
    let path = match fs::canonicalize(path) {
        Ok(path) => path,
        Err(error) if error.kind() == io::ErrorKind::NotFound => missing_source_path(path)?,
        Err(error) => {
            return Err(CheckError::InvalidInput(format!(
                "failed to canonicalize source file '{}': {error}",
                path.display()
            )));
        }
    };
    SourceFileId::new(path).map_err(|error| CheckError::InvalidInput(error.to_string()))
}

fn missing_source_path(path: &Path) -> Result<PathBuf, CheckError> {
    let absolute = absolute_path(path)?;
    let mut missing = Vec::<OsString>::new();
    let mut cursor = absolute.as_path();

    loop {
        match fs::canonicalize(cursor) {
            Ok(mut existing) => {
                for component in missing.iter().rev() {
                    existing.push(component);
                }
                return Ok(existing);
            }
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                let Some(name) = cursor.file_name() else {
                    return Ok(absolute);
                };
                missing.push(name.to_os_string());
                let Some(parent) = cursor.parent() else {
                    return Ok(absolute);
                };
                cursor = parent;
            }
            Err(error) => {
                return Err(CheckError::InvalidInput(format!(
                    "failed to canonicalize source file '{}': {error}",
                    path.display()
                )));
            }
        }
    }
}

fn absolute_path(path: &Path) -> Result<PathBuf, CheckError> {
    if path.is_absolute() {
        return Ok(normalize_path(path));
    }
    let path = std::env::current_dir()
        .map_err(|error| CheckError::InvalidInput(error.to_string()))?
        .join(path);
    Ok(normalize_path(&path))
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            other => normalized.push(other.as_os_str()),
        }
    }
    normalized
}

fn module_file(root: &Path, module_path: &[String]) -> PathBuf {
    let mut file = root.to_path_buf();
    for segment in module_path {
        file.push(segment);
    }
    file.set_extension("anv");
    file
}

fn module_path(path: Vec<String>) -> Result<ModulePath, CheckError> {
    ModulePath::new(path).map_err(|error| CheckError::InvalidInput(error.to_string()))
}

fn validate_label(label: &str, name: &str) -> Result<(), CheckError> {
    if label.trim().is_empty() {
        return invalid_input(format!("{name} must not be empty"));
    }

    Ok(())
}

fn insert_unique_path(
    paths: &mut HashSet<Vec<String>>,
    path: &[String],
    message: &str,
) -> Result<(), CheckError> {
    if !paths.insert(path.to_vec()) {
        return invalid_input(format!("{message}: {}", display_path(path)));
    }

    Ok(())
}

fn display_path(path: &[String]) -> String {
    if path.is_empty() {
        "<empty>".to_string()
    } else {
        path.join(".")
    }
}

fn invalid_input<T>(message: impl Into<String>) -> Result<T, CheckError> {
    Err(CheckError::InvalidInput(message.into()))
}
