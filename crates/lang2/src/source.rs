use std::{
    collections::{HashMap, HashSet},
    fs, io,
    path::PathBuf,
};

use anvyx_frontend::{
    pipeline::{
        ModuleInput as FrontendModuleInput, Source as FrontendSource, SourceLoadError, SourceLoader,
    },
    resolve::ModulePath,
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

    pub(crate) fn to_frontend_module(&self) -> FrontendModuleInput {
        FrontendModuleInput {
            path: self.path.clone(),
            source: self.to_frontend_source(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn empty() -> Self {
        Self {
            core_prelude: None,
            core_modules: vec![],
            std_modules: HashMap::new(),
            always_active_modules: vec![],
        }
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

#[derive(Debug, Clone)]
pub(crate) struct SourceEnvironment<'a> {
    root: PathBuf,
    sources: &'a SourceBundle,
}

impl<'a> SourceEnvironment<'a> {
    pub(crate) fn new(root: impl Into<PathBuf>, sources: &'a SourceBundle) -> Self {
        Self {
            root: root.into(),
            sources,
        }
    }

    fn load_source(&self, module_path: &ModulePath) -> Result<Option<FrontendSource>, CheckError> {
        if module_path.first_segment() == Some("std") {
            return Ok(self.load_std_source(module_path));
        }

        let file = self.local_module_file(module_path.segments());
        match fs::read_to_string(&file) {
            Ok(code) => SourceText::new(code, file.display().to_string())
                .map(|source| Some(source.to_frontend_source())),
            Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(None),
            Err(error) => Err(CheckError::ReadModule {
                path: file,
                message: error.to_string(),
            }),
        }
    }

    fn load_std_source(&self, module_path: &ModulePath) -> Option<FrontendSource> {
        self.sources
            .std_module(module_path.segments())
            .map(ModuleSource::to_frontend_source)
    }

    fn local_module_file(&self, module_path: &[String]) -> PathBuf {
        let mut file = self.root.clone();
        for segment in module_path {
            file.push(segment);
        }
        file.set_extension("anv");
        file
    }
}

impl SourceLoader for SourceEnvironment<'_> {
    type FatalError = CheckError;

    fn load(
        &mut self,
        module_path: &ModulePath,
    ) -> Result<Option<FrontendSource>, SourceLoadError<Self::FatalError>> {
        self.load_source(module_path)
            .map_err(SourceLoadError::Fatal)
    }
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
    use std::{
        path::Path,
        sync::atomic::{AtomicU64, Ordering},
    };

    use super::*;

    static NEXT_TEMP_ID: AtomicU64 = AtomicU64::new(0);

    fn path(segments: &[&str]) -> Vec<String> {
        segments
            .iter()
            .map(|segment| (*segment).to_string())
            .collect()
    }

    fn module_path(segments: &[&str]) -> ModulePath {
        ModulePath::new(path(segments)).unwrap()
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

    struct TempDir {
        path: PathBuf,
    }

    impl TempDir {
        fn new() -> Self {
            let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
            let path = std::env::temp_dir().join(format!(
                "anvyx-lang2-source-env-{}-{id}",
                std::process::id()
            ));
            let _ = fs::remove_dir_all(&path);
            fs::create_dir_all(&path).unwrap();
            Self { path }
        }

        fn path(&self) -> &Path {
            &self.path
        }

        fn write(&self, relative: &str, code: &str) {
            let file = self.path.join(relative);
            if let Some(parent) = file.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(file, code).unwrap();
        }

        fn mkdir(&self, relative: &str) {
            fs::create_dir_all(self.path.join(relative)).unwrap();
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
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
        let bundle = SourceBundle::empty();

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

    #[test]
    fn source_environment_loads_sibling_module() {
        let temp = TempDir::new();
        temp.write("helper.anv", "const VALUE = 1;");
        let bundle = SourceBundle::empty();
        let env = SourceEnvironment::new(temp.path(), &bundle);

        let source = env.load_source(&module_path(&["helper"])).unwrap().unwrap();

        assert_eq!(source.code, "const VALUE = 1;");
        assert_eq!(
            source.label,
            temp.path().join("helper.anv").display().to_string()
        );
    }

    #[test]
    fn source_environment_loads_nested_module() {
        let temp = TempDir::new();
        temp.write("foo/bar.anv", "const VALUE = 2;");
        let bundle = SourceBundle::empty();
        let env = SourceEnvironment::new(temp.path(), &bundle);

        let source = env
            .load_source(&module_path(&["foo", "bar"]))
            .unwrap()
            .unwrap();

        assert_eq!(source.code, "const VALUE = 2;");
        assert_eq!(
            source.label,
            temp.path().join("foo/bar.anv").display().to_string()
        );
    }

    #[test]
    fn source_environment_returns_none_for_missing_module() {
        let temp = TempDir::new();
        let bundle = SourceBundle::empty();
        let env = SourceEnvironment::new(temp.path(), &bundle);

        assert!(
            env.load_source(&module_path(&["missing"]))
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn source_environment_does_not_load_std_from_filesystem() {
        let temp = TempDir::new();
        temp.write("std.anv", "const WRONG = 1;");
        temp.write("std/math.anv", "const WRONG = 2;");
        let bundle = SourceBundle::empty();
        let env = SourceEnvironment::new(temp.path(), &bundle);

        assert!(env.load_source(&module_path(&["std"])).unwrap().is_none());
        assert!(
            env.load_source(&module_path(&["std", "math"]))
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn source_environment_reports_read_errors() {
        let temp = TempDir::new();
        temp.mkdir("bad.anv");
        let bundle = SourceBundle::empty();
        let env = SourceEnvironment::new(temp.path(), &bundle);

        let error = env
            .load_source(&module_path(&["bad"]))
            .unwrap_err()
            .to_string();

        assert!(error.contains("failed to read module source"));
        assert!(error.contains("bad.anv"));
    }

    #[test]
    fn source_environment_loads_through_frontend_loader_trait() {
        let temp = TempDir::new();
        temp.write("helper.anv", "const VALUE = 1;");
        let bundle = SourceBundle::empty();
        let mut env = SourceEnvironment::new(temp.path(), &bundle);

        let source = SourceLoader::load(&mut env, &module_path(&["helper"]))
            .unwrap()
            .unwrap();

        assert_eq!(source.code, "const VALUE = 1;");
        assert_eq!(
            source.label,
            temp.path().join("helper.anv").display().to_string()
        );
    }

    #[test]
    fn source_environment_loads_known_std_module() {
        let temp = TempDir::new();
        let bundle = source_bundle(vec![std_source(
            &["std", "math"],
            "const PI = 3;",
            "<std.math>",
        )]);
        let env = SourceEnvironment::new(temp.path(), &bundle);

        let source = env
            .load_source(&module_path(&["std", "math"]))
            .unwrap()
            .unwrap();

        assert_eq!(source.code, "const PI = 3;");
        assert_eq!(source.label, "<std.math>");
    }

    #[test]
    fn source_environment_loads_nested_std_module() {
        let temp = TempDir::new();
        let bundle = source_bundle(vec![std_source(
            &["std", "collections", "map"],
            "type Map;",
            "<std.collections.map>",
        )]);
        let env = SourceEnvironment::new(temp.path(), &bundle);

        let source = env
            .load_source(&module_path(&["std", "collections", "map"]))
            .unwrap()
            .unwrap();

        assert_eq!(source.code, "type Map;");
        assert_eq!(source.label, "<std.collections.map>");
    }

    #[test]
    fn source_environment_returns_none_for_unknown_std_module() {
        let temp = TempDir::new();
        let bundle = source_bundle(vec![std_module("math")]);
        let env = SourceEnvironment::new(temp.path(), &bundle);

        assert!(
            env.load_source(&module_path(&["std", "unknown"]))
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn source_environment_returns_none_for_std_root() {
        let temp = TempDir::new();
        let bundle = source_bundle(vec![std_module("math")]);
        let env = SourceEnvironment::new(temp.path(), &bundle);

        assert!(env.load_source(&module_path(&["std"])).unwrap().is_none());
    }

    #[test]
    fn source_environment_does_not_read_local_std_file_for_known_std_module() {
        let temp = TempDir::new();
        temp.write("std/math.anv", "const WRONG = 1;");
        let bundle = source_bundle(vec![std_source(
            &["std", "math"],
            "const RIGHT = 1;",
            "<std.math>",
        )]);
        let env = SourceEnvironment::new(temp.path(), &bundle);

        let source = env
            .load_source(&module_path(&["std", "math"]))
            .unwrap()
            .unwrap();

        assert_eq!(source.code, "const RIGHT = 1;");
        assert_eq!(source.label, "<std.math>");
    }

    #[test]
    fn source_environment_does_not_read_local_std_file_for_unknown_std_module() {
        let temp = TempDir::new();
        temp.write("std/missing.anv", "const WRONG = 1;");
        let bundle = SourceBundle::empty();
        let env = SourceEnvironment::new(temp.path(), &bundle);

        assert!(
            env.load_source(&module_path(&["std", "missing"]))
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn source_environment_loads_std_through_frontend_loader_trait() {
        let temp = TempDir::new();
        let bundle = source_bundle(vec![std_source(
            &["std", "math"],
            "const PI = 3;",
            "<std.math>",
        )]);
        let mut env = SourceEnvironment::new(temp.path(), &bundle);

        let source = SourceLoader::load(&mut env, &module_path(&["std", "math"]))
            .unwrap()
            .unwrap();

        assert_eq!(source.code, "const PI = 3;");
        assert_eq!(source.label, "<std.math>");
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
