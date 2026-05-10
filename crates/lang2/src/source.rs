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
    providers: Vec<anvyx_externs::ProviderDescriptor>,
}

impl SystemPackageSource {
    pub fn new(root: SourceText, modules: Vec<ModuleSource>) -> Result<Self, CheckError> {
        let mut paths = HashSet::new();
        for module in &modules {
            insert_unique_path(&mut paths, module.path(), "duplicate system module path")?;
        }
        Ok(Self {
            root,
            modules,
            providers: vec![],
        })
    }

    pub fn with_providers(
        root: SourceText,
        modules: Vec<ModuleSource>,
        providers: Vec<anvyx_externs::ProviderDescriptor>,
    ) -> Result<Self, CheckError> {
        let mut source = Self::new(root, modules)?;
        source.providers = providers;
        Ok(source)
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

    pub fn providers(&self) -> &[anvyx_externs::ProviderDescriptor] {
        &self.providers
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
                "--new-frontend file override '{source_file}' is outside every loaded package source root"
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

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    fn path(segments: &[&str]) -> Vec<String> {
        segments
            .iter()
            .map(|segment| (*segment).to_string())
            .collect()
    }

    fn module_path(segments: &[&str]) -> ModulePath {
        ModulePath::new(path(segments)).unwrap()
    }

    fn package_id() -> PackageId {
        PackageId::synthetic_root()
    }

    fn module_id(segments: &[&str]) -> ModuleId {
        ModuleId::named(package_id(), module_path(segments))
    }

    fn std_module_id(segments: &[&str]) -> ModuleId {
        ModuleId::named(PackageId::std(), module_path(segments))
    }

    fn package_env<'a>(root: &Path, bundle: &'a SourceBundle) -> PackageSourceEnvironment<'a> {
        let package = PackageSource::new(
            package_id(),
            root.join("main.anv"),
            root.to_path_buf(),
            HashMap::new(),
        )
        .unwrap();
        let ownership = SourceOwnership::new(&[package]).unwrap();
        PackageSourceEnvironment::new(ownership, bundle)
    }

    fn loaded_source(load: SourceLoad) -> PackageModuleInput {
        let SourceLoad::Loaded(source) = load else {
            panic!("expected loaded source");
        };
        source
    }

    fn core(name: &str) -> ModuleSource {
        ModuleSource::new(path(&[name]), "", format!("<{name}>")).unwrap()
    }

    fn std_module(name: &str) -> ModuleSource {
        ModuleSource::new(path(&[name]), "", format!("<std.{name}>")).unwrap()
    }

    fn std_source(segments: &[&str], code: &str, label: &str) -> ModuleSource {
        ModuleSource::new(path(segments), code, label).unwrap()
    }

    fn root(label: &str) -> SourceText {
        SourceText::new("", label).unwrap()
    }

    fn system(label: &str, modules: Vec<ModuleSource>) -> SystemPackageSource {
        SystemPackageSource::new(root(label), modules).unwrap()
    }

    fn source_bundle(std_modules: Vec<ModuleSource>) -> SourceBundle {
        SourceBundle::new(None, Some(system("<std>", std_modules)))
    }

    fn invalid_message<T>(result: Result<T, CheckError>) -> String {
        match result {
            Err(CheckError::InvalidInput(message)) => message,
            Err(error) => panic!("expected invalid input, got {error:?}"),
            Ok(_) => panic!("expected invalid input, got ok"),
        }
    }

    fn write(dir: &tempfile::TempDir, relative: &str, code: &str) {
        let file = dir.path().join(relative);
        if let Some(parent) = file.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(file, code).unwrap();
    }

    fn mkdir(dir: &tempfile::TempDir, relative: &str) {
        fs::create_dir_all(dir.path().join(relative)).unwrap();
    }

    #[test]
    fn source_text_rejects_empty_label() {
        let message = invalid_message(SourceText::new("code", "  "));
        assert!(message.contains("source label"));
    }

    #[test]
    fn source_text_allows_empty_code() {
        let source = SourceText::new("", "<core>").unwrap();
        assert_eq!(source.code(), "");
        assert_eq!(source.label(), "<core>");
    }

    #[test]
    fn module_source_rejects_empty_path() {
        let message = invalid_message(ModuleSource::new(vec![], "", "<module>"));
        assert!(message.contains("must not be empty"));
    }

    #[test]
    fn module_source_rejects_empty_segment() {
        let message = invalid_message(ModuleSource::new(path(&["foo", "", "bar"]), "", "<module>"));
        assert!(message.contains("empty segments"));
        assert!(message.contains("foo..bar"));
    }

    #[test]
    fn module_source_rejects_empty_label() {
        let message = invalid_message(ModuleSource::new(path(&["foo"]), "", ""));
        assert!(message.contains("module source label"));
    }

    #[test]
    fn module_source_accepts_valid_path_code_and_label() {
        let source =
            ModuleSource::new(path(&["foo", "bar"]), "const X = 1;", "foo/bar.anv").unwrap();

        assert_eq!(source.path(), path(&["foo", "bar"]));
        assert_eq!(source.code(), "const X = 1;");
        assert_eq!(source.label(), "foo/bar.anv");
    }

    #[test]
    fn system_package_rejects_duplicate_paths() {
        let message = invalid_message(SystemPackageSource::new(
            root("<core>"),
            vec![core("int"), core("int")],
        ));
        assert!(message.contains("duplicate system module path"));
        assert!(message.contains("int"));
    }

    #[test]
    fn source_bundle_allows_same_path_in_core_and_std() {
        let bundle = SourceBundle::new(
            Some(system("<core>", vec![core("math")])),
            Some(system("<std>", vec![std_module("math")])),
        );

        assert_eq!(bundle.core().unwrap().modules().len(), 1);
        assert_eq!(bundle.std().unwrap().modules().len(), 1);
    }

    #[test]
    fn source_bundle_accepts_core_and_std_packages() {
        let bundle = SourceBundle::new(
            Some(
                SystemPackageSource::new(
                    SourceText::new("core prelude", "<core>").unwrap(),
                    vec![core("int"), core("string")],
                )
                .unwrap(),
            ),
            Some(system(
                "<std>",
                vec![std_module("math"), std_module("maps")],
            )),
        );

        assert_eq!(bundle.core().unwrap().modules().len(), 2);
        assert_eq!(bundle.std().unwrap().modules().len(), 2);
    }

    #[test]
    fn empty_bundle_has_no_sources() {
        let bundle = SourceBundle::default();

        assert!(bundle.core().is_none());
        assert!(bundle.std().is_none());
    }

    #[test]
    fn std_module_lookup_uses_package_local_path() {
        let bundle = source_bundle(vec![std_module("math")]);
        let module = bundle.std().unwrap().module(&path(&["math"])).unwrap();

        assert_eq!(module.label(), "<std.math>");
        assert!(
            bundle
                .std()
                .unwrap()
                .module(&path(&["std", "math"]))
                .is_none()
        );
    }

    mod ownership {
        use super::*;

        #[test]
        fn finds_owner() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "src/main.anv", "fn main() {}");
            let package = PackageSource::new(
                PackageId::new("game"),
                temp.path().join("src/main.anv"),
                temp.path().join("src"),
                HashMap::new(),
            )
            .unwrap();
            let ownership = SourceOwnership::new(&[package]).unwrap();
            let file = source_file_id(&temp.path().join("src/main.anv")).unwrap();

            assert_eq!(
                ownership.source_owner(&file).unwrap(),
                SourceOwner::Package(PackageId::new("game"))
            );
        }

        #[test]
        fn ignores_native_only_packages() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "src/main.anv", "fn main() {}");
            let package = PackageSource::native_only(PackageId::new("host"), HashMap::new());
            let ownership = SourceOwnership::new(&[package]).unwrap();
            let file = source_file_id(&temp.path().join("src/main.anv")).unwrap();

            assert_eq!(ownership.source_owner(&file).unwrap(), SourceOwner::None);
        }

        #[test]
        fn reports_no_owner() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "src/main.anv", "fn main() {}");
            write(&temp, "outside.anv", "fn outside() {}");
            let package = PackageSource::new(
                PackageId::new("game"),
                temp.path().join("src/main.anv"),
                temp.path().join("src"),
                HashMap::new(),
            )
            .unwrap();
            let ownership = SourceOwnership::new(&[package]).unwrap();
            let file = source_file_id(&temp.path().join("outside.anv")).unwrap();

            assert_eq!(ownership.source_owner(&file).unwrap(), SourceOwner::None);
        }

        #[test]
        fn rejects_ambiguous_roots() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "root/nested/main.anv", "fn main() {}");
            let outer = PackageSource::new(
                PackageId::new("outer"),
                temp.path().join("root/main.anv"),
                temp.path().join("root"),
                HashMap::new(),
            )
            .unwrap();
            let inner = PackageSource::new(
                PackageId::new("inner"),
                temp.path().join("root/nested/main.anv"),
                temp.path().join("root/nested"),
                HashMap::new(),
            )
            .unwrap();
            let ownership = SourceOwnership::new(&[outer, inner]).unwrap();
            let file = source_file_id(&temp.path().join("root/nested/main.anv")).unwrap();
            let message = invalid_message(ownership.source_owner(&file));

            assert!(message.contains("owned by multiple package source roots"));
            assert!(message.contains("outer"));
            assert!(message.contains("inner"));
        }

        #[test]
        fn rejects_outside_override() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "src/main.anv", "fn main() {}");
            write(&temp, "outside.anv", "fn main() {}");
            let package = PackageSource::new(
                PackageId::new("game"),
                temp.path().join("src/main.anv"),
                temp.path().join("src"),
                HashMap::new(),
            )
            .unwrap();
            let ownership = SourceOwnership::new(&[package]).unwrap();
            let message =
                invalid_message(ownership.validate_root_file(&temp.path().join("outside.anv")));

            assert!(message.contains("outside every loaded package source root"));
        }
    }

    mod env {
        use super::*;

        #[test]
        fn loads_sibling() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "helper.anv", "const VALUE = 1;");
            let bundle = SourceBundle::default();
            let mut env = package_env(temp.path(), &bundle);

            let source = env.load_source(&module_id(&["helper"])).unwrap().unwrap();
            let file = temp.path().join("helper.anv");

            assert_eq!(source.module.package(), &package_id());
            assert_eq!(
                source.module.source_file().unwrap().path(),
                fs::canonicalize(&file).unwrap()
            );
            let canonical = fs::canonicalize(&file).unwrap();
            assert_eq!(source.source.code, "const VALUE = 1;");
            assert_eq!(source.source.label, file.display().to_string());
            assert_eq!(source.source.path.as_deref(), Some(canonical.as_path()));
        }

        #[test]
        fn loads_nested() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "foo/bar.anv", "const VALUE = 2;");
            let bundle = SourceBundle::default();
            let mut env = package_env(temp.path(), &bundle);

            let source = env
                .load_source(&module_id(&["foo", "bar"]))
                .unwrap()
                .unwrap();

            let file = temp.path().join("foo/bar.anv");

            assert_eq!(source.module.package(), &package_id());
            assert_eq!(
                source.module.source_file().unwrap().path(),
                fs::canonicalize(&file).unwrap()
            );
            assert_eq!(source.source.code, "const VALUE = 2;");
            assert_eq!(source.source.label, file.display().to_string());
        }

        #[test]
        fn loads_current_relative() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "src/ui/button.anv", "import helper;");
            write(&temp, "src/ui/helper.anv", "const VALUE = 1;");
            let bundle = SourceBundle::default();
            let mut env = package_env(&temp.path().join("src"), &bundle);
            let importer = source_file_id(&temp.path().join("src/ui/button.anv")).unwrap();

            let source = loaded_source(
                PackageSourceLoader::load_local_source(
                    &mut env,
                    &LocalSourceRequest {
                        importer,
                        ascend: 0,
                        path: module_path(&["helper"]),
                    },
                )
                .unwrap(),
            );

            assert_eq!(source.source.code, "const VALUE = 1;");
            assert_eq!(
                source.module.source_file().unwrap().path(),
                fs::canonicalize(temp.path().join("src/ui/helper.anv")).unwrap()
            );
        }

        #[test]
        fn loads_parent_relative() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "src/ui/button.anv", "import ..common;");
            write(&temp, "src/common.anv", "const VALUE = 1;");
            let bundle = SourceBundle::default();
            let mut env = package_env(&temp.path().join("src"), &bundle);
            let importer = source_file_id(&temp.path().join("src/ui/button.anv")).unwrap();

            let source = loaded_source(
                PackageSourceLoader::load_local_source(
                    &mut env,
                    &LocalSourceRequest {
                        importer,
                        ascend: 1,
                        path: module_path(&["common"]),
                    },
                )
                .unwrap(),
            );

            assert_eq!(source.source.code, "const VALUE = 1;");
            assert_eq!(
                source.module.source_file().unwrap().path(),
                fs::canonicalize(temp.path().join("src/common.anv")).unwrap()
            );
        }

        #[test]
        fn caches_equivalent_sources() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "src/ui/button.anv", "import helper; import .helper;");
            write(&temp, "src/ui/helper.anv", "const VALUE = 1;");
            let bundle = SourceBundle::default();
            let mut env = package_env(&temp.path().join("src"), &bundle);
            let importer = source_file_id(&temp.path().join("src/ui/button.anv")).unwrap();
            let request = LocalSourceRequest {
                importer: importer.clone(),
                ascend: 0,
                path: module_path(&["helper"]),
            };

            let first =
                loaded_source(PackageSourceLoader::load_local_source(&mut env, &request).unwrap());
            fs::write(temp.path().join("src/ui/helper.anv"), "const VALUE = 2;").unwrap();
            let second = loaded_source(
                PackageSourceLoader::load_local_source(
                    &mut env,
                    &LocalSourceRequest {
                        importer,
                        ascend: 0,
                        path: module_path(&["helper"]),
                    },
                )
                .unwrap(),
            );

            assert_eq!(first.source.code, "const VALUE = 1;");
            assert_eq!(second.source.code, "const VALUE = 1;");
            assert_eq!(first.module, second.module);
        }

        #[test]
        fn missing_module_returns_none() {
            let temp = tempfile::tempdir().unwrap();
            let bundle = SourceBundle::default();
            let mut env = package_env(temp.path(), &bundle);

            assert!(env.load_source(&module_id(&["missing"])).unwrap().is_none());
        }

        #[test]
        fn std_does_not_use_filesystem() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "std.anv", "const WRONG = 1;");
            write(&temp, "std/math.anv", "const WRONG = 2;");
            let bundle = SourceBundle::default();
            let mut env = package_env(temp.path(), &bundle);

            assert!(env.load_source(&std_module_id(&["std"])).unwrap().is_none());
            assert!(
                env.load_source(&std_module_id(&["math"]))
                    .unwrap()
                    .is_none()
            );
        }

        #[test]
        fn reports_read_errors() {
            let temp = tempfile::tempdir().unwrap();
            mkdir(&temp, "bad.anv");
            let bundle = SourceBundle::default();
            let mut env = package_env(temp.path(), &bundle);

            let error = env
                .load_source(&module_id(&["bad"]))
                .unwrap_err()
                .to_string();

            assert!(error.contains("failed to read module source"));
            assert!(error.contains("bad.anv"));
        }

        #[test]
        fn loads_through_trait() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "helper.anv", "const VALUE = 1;");
            let bundle = SourceBundle::default();
            let mut env = package_env(temp.path(), &bundle);

            let source = PackageSourceLoader::load(&mut env, &module_id(&["helper"]))
                .unwrap()
                .unwrap();

            assert_eq!(source.source.code, "const VALUE = 1;");
            assert_eq!(
                source.source.label,
                temp.path().join("helper.anv").display().to_string()
            );
        }

        #[test]
        fn loads_known_std_module() {
            let temp = tempfile::tempdir().unwrap();
            let bundle = source_bundle(vec![std_source(&["math"], "const PI = 3;", "<std.math>")]);
            let mut env = package_env(temp.path(), &bundle);

            let source = env.load_source(&std_module_id(&["math"])).unwrap().unwrap();

            assert_eq!(source.source.code, "const PI = 3;");
            assert_eq!(source.source.label, "<std.math>");
            assert_eq!(source.source.path, None);
        }

        #[test]
        fn loads_nested_std_module() {
            let temp = tempfile::tempdir().unwrap();
            let bundle = source_bundle(vec![std_source(
                &["collections", "map"],
                "type Map;",
                "<std.collections.map>",
            )]);
            let mut env = package_env(temp.path(), &bundle);

            let source = env
                .load_source(&std_module_id(&["collections", "map"]))
                .unwrap()
                .unwrap();

            assert_eq!(source.source.code, "type Map;");
            assert_eq!(source.source.label, "<std.collections.map>");
        }

        #[test]
        fn unknown_std_returns_none() {
            let temp = tempfile::tempdir().unwrap();
            let bundle = source_bundle(vec![std_module("math")]);
            let mut env = package_env(temp.path(), &bundle);

            assert!(
                env.load_source(&std_module_id(&["unknown"]))
                    .unwrap()
                    .is_none()
            );
        }

        #[test]
        fn std_root_returns_none() {
            let temp = tempfile::tempdir().unwrap();
            let bundle = source_bundle(vec![std_module("math")]);
            let mut env = package_env(temp.path(), &bundle);

            assert!(
                env.load_source(&ModuleId::root(PackageId::std()))
                    .unwrap()
                    .is_none()
            );
        }

        #[test]
        fn known_std_ignores_local_file() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "std/math.anv", "const WRONG = 1;");
            let bundle = source_bundle(vec![std_source(
                &["math"],
                "const RIGHT = 1;",
                "<std.math>",
            )]);
            let mut env = package_env(temp.path(), &bundle);

            let source = env.load_source(&std_module_id(&["math"])).unwrap().unwrap();

            assert_eq!(source.source.code, "const RIGHT = 1;");
            assert_eq!(source.source.label, "<std.math>");
        }

        #[test]
        fn unknown_std_ignores_local_file() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "std/missing.anv", "const WRONG = 1;");
            let bundle = SourceBundle::default();
            let mut env = package_env(temp.path(), &bundle);

            assert!(
                env.load_source(&std_module_id(&["missing"]))
                    .unwrap()
                    .is_none()
            );
        }

        #[test]
        fn loads_std_through_trait() {
            let temp = tempfile::tempdir().unwrap();
            let bundle = source_bundle(vec![std_source(&["math"], "const PI = 3;", "<std.math>")]);
            let mut env = package_env(temp.path(), &bundle);

            let source = PackageSourceLoader::load(&mut env, &std_module_id(&["math"]))
                .unwrap()
                .unwrap();

            assert_eq!(source.source.code, "const PI = 3;");
            assert_eq!(source.source.label, "<std.math>");
        }
    }

    #[test]
    fn getters_return_original_source_text() {
        let prelude = SourceText::new("type int;", "<core>").unwrap();
        let core = ModuleSource::new(path(&["int"]), "extend int {}", "<core.int>").unwrap();
        let std = ModuleSource::new(path(&["math"]), "fn min() {}", "<std.math>").unwrap();
        let bundle = SourceBundle::new(
            Some(SystemPackageSource::new(prelude.clone(), vec![core.clone()]).unwrap()),
            Some(SystemPackageSource::new(root("<std>"), vec![std.clone()]).unwrap()),
        );

        assert_eq!(bundle.core().unwrap().root(), &prelude);
        assert_eq!(bundle.core().unwrap().modules(), &[core]);
        assert_eq!(bundle.std().unwrap().module(&path(&["math"])), Some(&std));
    }
}
