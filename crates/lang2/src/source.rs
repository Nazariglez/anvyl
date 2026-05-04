use std::{
    collections::{HashMap, HashSet},
    fs, io,
    path::{Path, PathBuf},
};

use anvyx_frontend::{
    pipeline::{
        PackageModuleInput, PackageSourceLoader, Source as FrontendSource, SourceLoad,
        SourceLoadError,
    },
    resolve::{LocalSourceRequest, ModuleId, ModulePath, PackageId, SourceFileId},
};

use crate::CheckError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceText {
    code: String,
    label: String,
}

impl SourceText {
    pub fn new(code: impl Into<String>, label: impl Into<String>) -> Result<Self, CheckError> {
        let label = label.into();
        validate_label(&label, "source label")?;

        Ok(Self {
            code: code.into(),
            label,
        })
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
        }
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
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SourceBundle {
    core_prelude: Option<SourceText>,
    core_modules: Vec<ModuleSource>,
    std_modules: HashMap<Vec<String>, ModuleSource>,
    always_active_modules: Vec<Vec<String>>,
}

impl SourceBundle {
    pub fn new(
        core_prelude: Option<SourceText>,
        core_modules: Vec<ModuleSource>,
        std_modules: Vec<ModuleSource>,
        always_active_modules: Vec<Vec<String>>,
    ) -> Result<Self, CheckError> {
        let mut module_paths = HashSet::new();
        for module in core_modules.iter().chain(&std_modules) {
            insert_unique_path(&mut module_paths, module.path(), "duplicate module path")?;
        }

        let mut core_paths = HashSet::new();
        for module in &core_modules {
            if path_starts_with(module.path(), "std") {
                return invalid_input(format!(
                    "core module path must not start with std: {}",
                    display_path(module.path())
                ));
            }
            core_paths.insert(module.path().to_vec());
        }

        let mut std_by_path = HashMap::new();
        for module in std_modules {
            if !path_starts_with(module.path(), "std") {
                return invalid_input(format!(
                    "std module path must start with std: {}",
                    display_path(module.path())
                ));
            }
            if module.path().len() == 1 {
                return invalid_input("std module path must include a module name after std: std");
            }
            std_by_path.insert(module.path().to_vec(), module);
        }

        let mut active_paths = HashSet::new();
        for path in &always_active_modules {
            validate_path(path)?;
            if path_starts_with(path, "std") {
                return invalid_input(format!(
                    "always-active module must not be a std module: {}",
                    display_path(path)
                ));
            }
            insert_unique_path(
                &mut active_paths,
                path,
                "duplicate always-active module path",
            )?;
            if !core_paths.contains(path) {
                return invalid_input(format!(
                    "always-active module must refer to a core module: {}",
                    display_path(path)
                ));
            }
        }

        Ok(Self {
            core_prelude,
            core_modules,
            std_modules: std_by_path,
            always_active_modules,
        })
    }

    pub fn core_prelude(&self) -> Option<&SourceText> {
        self.core_prelude.as_ref()
    }

    pub fn core_modules(&self) -> &[ModuleSource] {
        &self.core_modules
    }

    pub fn std_module(&self, path: &[String]) -> Option<&ModuleSource> {
        self.std_modules.get(path)
    }

    pub fn std_modules(&self) -> impl Iterator<Item = &ModuleSource> {
        self.std_modules.values()
    }

    pub fn always_active_modules(&self) -> &[Vec<String>] {
        &self.always_active_modules
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageSource {
    id: PackageId,
    entry: PathBuf,
    source_root: PathBuf,
    dependencies: HashMap<String, PackageId>,
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
            entry,
            source_root,
            dependencies,
        })
    }

    pub fn id(&self) -> &PackageId {
        &self.id
    }

    pub fn entry(&self) -> &Path {
        &self.entry
    }

    pub fn source_root(&self) -> &Path {
        &self.source_root
    }

    pub fn dependencies(&self) -> &HashMap<String, PackageId> {
        &self.dependencies
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
            .map(|package| {
                let canonical_root = fs::canonicalize(package.source_root()).map_err(|error| {
                    CheckError::InvalidInput(format!(
                        "failed to canonicalize package '{}' source root '{}': {error}",
                        package.id(),
                        package.source_root().display()
                    ))
                })?;
                Ok(SourceRoot {
                    package: package.id().clone(),
                    root: package.source_root().to_path_buf(),
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
        let source_file = canonical_source_file(file)?;
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

    fn load_source(&mut self, module: &ModuleId) -> Result<Option<PackageModuleInput>, CheckError> {
        let Some(path) = module.named_path() else {
            return Ok(None);
        };
        if module.package() == &PackageId::std() {
            return Ok(self.load_std_source(path));
        }
        let Some(source_root) = self.ownership.source_root(module.package()) else {
            return Ok(None);
        };
        let file = module_file(source_root, path.segments());
        self.read_module_file(file)
    }

    fn load_std_source(&self, module_path: &ModulePath) -> Option<PackageModuleInput> {
        let mut path = vec!["std".to_string()];
        path.extend(module_path.segments().iter().cloned());
        self.sources
            .std_module(&path)
            .map(ModuleSource::to_frontend_source)
            .map(|source| PackageModuleInput {
                module: ModuleId::named(PackageId::std(), module_path.clone()),
                source,
            })
    }

    fn read_module_file(
        &mut self,
        file: PathBuf,
    ) -> Result<Option<PackageModuleInput>, CheckError> {
        let source_file = match fs::canonicalize(&file) {
            Ok(canonical) => SourceFileId::new(canonical)
                .map_err(|error| CheckError::InvalidInput(error.to_string()))?,
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(error) => {
                return Err(CheckError::ReadModule {
                    path: file,
                    message: error.to_string(),
                });
            }
        };
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
                        source: source.to_frontend_source(),
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

pub(crate) fn canonical_source_file(path: &Path) -> Result<SourceFileId, CheckError> {
    let canonical = fs::canonicalize(path).map_err(|error| {
        CheckError::InvalidInput(format!(
            "failed to canonicalize source file '{}': {error}",
            path.display()
        ))
    })?;
    SourceFileId::new(canonical).map_err(|error| CheckError::InvalidInput(error.to_string()))
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

fn validate_path(path: &[String]) -> Result<(), CheckError> {
    module_path(path.to_vec()).map(|_| ())
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

fn path_starts_with(path: &[String], segment: &str) -> bool {
    path.first().is_some_and(|first| first == segment)
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
        ModuleSource::new(path(&["std", name]), "", format!("<std.{name}>")).unwrap()
    }

    fn std_source(segments: &[&str], code: &str, label: &str) -> ModuleSource {
        ModuleSource::new(path(segments), code, label).unwrap()
    }

    fn source_bundle(std_modules: Vec<ModuleSource>) -> SourceBundle {
        SourceBundle::new(None, vec![], std_modules, vec![]).unwrap()
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
    fn source_bundle_rejects_duplicate_core_paths() {
        let message = invalid_message(SourceBundle::new(
            None,
            vec![core("core_int"), core("core_int")],
            vec![],
            vec![],
        ));
        assert!(message.contains("duplicate module path"));
        assert!(message.contains("core_int"));
    }

    #[test]
    fn source_bundle_rejects_duplicate_std_paths() {
        let message = invalid_message(SourceBundle::new(
            None,
            vec![],
            vec![std_module("math"), std_module("math")],
            vec![],
        ));
        assert!(message.contains("duplicate module path"));
        assert!(message.contains("std.math"));
    }

    #[test]
    fn source_bundle_rejects_duplicate_paths_across_core_and_std() {
        let core_std = ModuleSource::new(path(&["std", "math"]), "", "<core>").unwrap();
        let message = invalid_message(SourceBundle::new(
            None,
            vec![core_std],
            vec![std_module("math")],
            vec![],
        ));
        assert!(message.contains("duplicate module path"));
        assert!(message.contains("std.math"));
    }

    #[test]
    fn source_bundle_rejects_duplicate_always_active_paths() {
        let message = invalid_message(SourceBundle::new(
            None,
            vec![core("core_int")],
            vec![],
            vec![path(&["core_int"]), path(&["core_int"])],
        ));
        assert!(message.contains("duplicate always-active"));
    }

    #[test]
    fn source_bundle_rejects_std_module_without_std_root() {
        let message = invalid_message(SourceBundle::new(None, vec![], vec![core("math")], vec![]));
        assert!(message.contains("std module path must start with std"));
        assert!(message.contains("math"));
    }

    #[test]
    fn source_bundle_rejects_bare_std_module_path() {
        let std_root = ModuleSource::new(path(&["std"]), "", "<std>").unwrap();
        let message = invalid_message(SourceBundle::new(None, vec![], vec![std_root], vec![]));
        assert!(message.contains("include a module name"));
    }

    #[test]
    fn source_bundle_rejects_core_module_under_std_root() {
        let core_std = ModuleSource::new(path(&["std", "core_int"]), "", "<core>").unwrap();
        let message = invalid_message(SourceBundle::new(None, vec![core_std], vec![], vec![]));
        assert!(message.contains("core module path must not start with std"));
    }

    #[test]
    fn source_bundle_accepts_core_and_std_modules() {
        let bundle = SourceBundle::new(
            Some(SourceText::new("core prelude", "<core>").unwrap()),
            vec![core("core_int"), core("core_string")],
            vec![std_module("math"), std_module("maps")],
            vec![path(&["core_int"])],
        )
        .unwrap();

        assert_eq!(bundle.core_modules().len(), 2);
        assert_eq!(bundle.std_modules().count(), 2);
        assert_eq!(bundle.always_active_modules(), &[path(&["core_int"])]);
    }

    #[test]
    fn source_bundle_rejects_empty_always_active_path() {
        let message = invalid_message(SourceBundle::new(
            None,
            vec![core("core_int")],
            vec![],
            vec![vec![]],
        ));
        assert!(message.contains("must not be empty"));
    }

    #[test]
    fn source_bundle_rejects_empty_always_active_segment() {
        let message = invalid_message(SourceBundle::new(
            None,
            vec![core("core_int")],
            vec![],
            vec![path(&["core_int", ""])],
        ));
        assert!(message.contains("empty segments"));
    }

    #[test]
    fn source_bundle_rejects_always_active_std_module() {
        let message = invalid_message(SourceBundle::new(
            None,
            vec![core("core_int")],
            vec![std_module("math")],
            vec![path(&["std", "math"])],
        ));
        assert!(message.contains("must not be a std module"));
    }

    #[test]
    fn source_bundle_rejects_always_active_missing_from_core_modules() {
        let message = invalid_message(SourceBundle::new(
            None,
            vec![core("core_int")],
            vec![],
            vec![path(&["helpers"])],
        ));
        assert!(message.contains("refer to a core module"));
    }

    #[test]
    fn source_bundle_accepts_always_active_core_module() {
        let bundle = SourceBundle::new(
            None,
            vec![core("core_int")],
            vec![],
            vec![path(&["core_int"])],
        )
        .unwrap();

        assert_eq!(bundle.always_active_modules(), &[path(&["core_int"])]);
    }

    #[test]
    fn empty_bundle_has_no_sources() {
        let bundle = SourceBundle::default();

        assert!(bundle.core_prelude().is_none());
        assert!(bundle.core_modules().is_empty());
        assert_eq!(bundle.std_modules().count(), 0);
        assert!(bundle.always_active_modules().is_empty());
    }

    #[test]
    fn std_module_lookup_uses_logical_path() {
        let bundle = SourceBundle::new(None, vec![], vec![std_module("math")], vec![]).unwrap();
        let module = bundle.std_module(&path(&["std", "math"])).unwrap();

        assert_eq!(module.label(), "<std.math>");
        assert!(bundle.std_module(&path(&["math"])).is_none());
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
            let file = canonical_source_file(&temp.path().join("src/main.anv")).unwrap();

            assert_eq!(
                ownership.source_owner(&file).unwrap(),
                SourceOwner::Package(PackageId::new("game"))
            );
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
            let file = canonical_source_file(&temp.path().join("outside.anv")).unwrap();

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
            let file = canonical_source_file(&temp.path().join("root/nested/main.anv")).unwrap();
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
            assert_eq!(source.source.code, "const VALUE = 1;");
            assert_eq!(source.source.label, file.display().to_string());
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
            let importer = canonical_source_file(&temp.path().join("src/ui/button.anv")).unwrap();

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
            let importer = canonical_source_file(&temp.path().join("src/ui/button.anv")).unwrap();

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
            let importer = canonical_source_file(&temp.path().join("src/ui/button.anv")).unwrap();
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
            let bundle = source_bundle(vec![std_source(
                &["std", "math"],
                "const PI = 3;",
                "<std.math>",
            )]);
            let mut env = package_env(temp.path(), &bundle);

            let source = env.load_source(&std_module_id(&["math"])).unwrap().unwrap();

            assert_eq!(source.source.code, "const PI = 3;");
            assert_eq!(source.source.label, "<std.math>");
        }

        #[test]
        fn loads_nested_std_module() {
            let temp = tempfile::tempdir().unwrap();
            let bundle = source_bundle(vec![std_source(
                &["std", "collections", "map"],
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

            assert!(env.load_source(&std_module_id(&["std"])).unwrap().is_none());
        }

        #[test]
        fn known_std_ignores_local_file() {
            let temp = tempfile::tempdir().unwrap();
            write(&temp, "std/math.anv", "const WRONG = 1;");
            let bundle = source_bundle(vec![std_source(
                &["std", "math"],
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
            let bundle = source_bundle(vec![std_source(
                &["std", "math"],
                "const PI = 3;",
                "<std.math>",
            )]);
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
        let core =
            ModuleSource::new(path(&["core_int"]), "extend int {}", "<core.core_int>").unwrap();
        let std = ModuleSource::new(path(&["std", "math"]), "fn min() {}", "<std.math>").unwrap();
        let bundle = SourceBundle::new(
            Some(prelude.clone()),
            vec![core.clone()],
            vec![std.clone()],
            vec![path(&["core_int"])],
        )
        .unwrap();

        assert_eq!(bundle.core_prelude(), Some(&prelude));
        assert_eq!(bundle.core_modules(), &[core]);
        assert_eq!(bundle.std_module(&path(&["std", "math"])), Some(&std));
    }
}
