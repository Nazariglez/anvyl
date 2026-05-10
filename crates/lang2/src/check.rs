use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use anvyx_frontend::{
    externs::{ExternInputs, PackageExternInputs},
    pipeline::{
        self, FrontendConfig, PackageModuleInput, PackageProgramInput, PackageSourceInput,
        Source as FrontendSource,
    },
    resolve::{ModuleId, PackageId, PackageKind, SystemPackages},
};

use crate::{
    CheckError, CheckResult, PackageSource, SourceBundle, SourceOverride, SourceText,
    source::{PackageSourceEnvironment, SourceOwnership, source_file_id},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckFileInput {
    file: PathBuf,
    sources: SourceBundle,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
}

impl CheckFileInput {
    pub fn new(file: impl Into<PathBuf>, sources: SourceBundle) -> Result<Self, CheckError> {
        let file = file.into();
        if file.as_os_str().is_empty() {
            return Err(CheckError::InvalidInput(
                "main source path must not be empty".to_string(),
            ));
        }

        Ok(Self {
            file,
            sources,
            source_overrides: vec![],
            config: FrontendConfig::default(),
        })
    }

    #[must_use]
    pub fn with_config(mut self, config: FrontendConfig) -> Self {
        self.config = config;
        self
    }

    #[must_use]
    pub fn with_source_overrides(mut self, source_overrides: Vec<SourceOverride>) -> Self {
        self.source_overrides = source_overrides;
        self
    }

    pub fn file(&self) -> &Path {
        &self.file
    }

    pub fn sources(&self) -> &SourceBundle {
        &self.sources
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckPackageInput {
    root_package: PackageId,
    root_file: PathBuf,
    packages: Vec<PackageSource>,
    sources: SourceBundle,
    source_overrides: Vec<SourceOverride>,
    config: FrontendConfig,
}

impl CheckPackageInput {
    pub fn new(
        root_package: PackageId,
        root_file: impl Into<PathBuf>,
        packages: Vec<PackageSource>,
        sources: SourceBundle,
    ) -> Result<Self, CheckError> {
        let root_file = root_file.into();
        if root_file.as_os_str().is_empty() {
            return Err(CheckError::InvalidInput(
                "main source path must not be empty".to_string(),
            ));
        }
        let Some(root) = packages
            .iter()
            .find(|package| package.id() == &root_package)
        else {
            return Err(CheckError::InvalidInput(format!(
                "root package '{root_package}' is missing from package input"
            )));
        };
        if root.kind() == PackageKind::NativeOnly {
            return Err(CheckError::InvalidInput(format!(
                "root package '{root_package}' has no source entry to check"
            )));
        }
        Ok(Self {
            root_package,
            root_file,
            packages,
            sources,
            source_overrides: vec![],
            config: FrontendConfig::default(),
        })
    }

    #[must_use]
    pub fn with_config(mut self, config: FrontendConfig) -> Self {
        self.config = config;
        self
    }

    #[must_use]
    pub fn with_source_overrides(mut self, source_overrides: Vec<SourceOverride>) -> Self {
        self.source_overrides = source_overrides;
        self
    }
}

pub fn check_file(input: CheckFileInput) -> CheckResult {
    let CheckFileInput {
        file,
        sources,
        source_overrides,
        config,
    } = input;
    let main_code = read_main(&file, &source_overrides)?;
    let root_file_id = source_file_id(&file)?;
    let main = PackageModuleInput {
        module: ModuleId::source_without_package(root_file_id.clone()),
        source: FrontendSource {
            code: main_code,
            label: file.display().to_string(),
            path: Some(root_file_id.path().to_path_buf()),
        },
    };

    check_prepared(PreparedCheck {
        root_package: PackageId::synthetic_root(),
        main: main.clone(),
        packages: HashMap::new(),
        cached_sources: vec![main],
        ownership: SourceOwnership::new(&[])?,
        source_overrides,
        sources,
        config,
    })
}

pub fn check_package(input: CheckPackageInput) -> CheckResult {
    let CheckPackageInput {
        root_package,
        root_file,
        packages,
        sources,
        source_overrides,
        config,
    } = input;
    let main_code = read_main(&root_file, &source_overrides)?;
    let ownership = SourceOwnership::new(&packages)?;
    let (root_owner, root_file_id) = ownership.validate_root_file(&root_file)?;
    let main = PackageModuleInput {
        module: ModuleId::source(root_owner, root_file_id.clone()),
        source: FrontendSource {
            code: main_code,
            label: root_file.display().to_string(),
            path: Some(root_file_id.path().to_path_buf()),
        },
    };
    let mut cached_sources = vec![main.clone()];
    let package_inputs = packages
        .iter()
        .map(|package| {
            let root = package_root(package, &root_package, &ownership, &source_overrides)?;
            if let Some(root) = &root {
                cached_sources.push(root.clone());
            }
            Ok((
                package.id().clone(),
                PackageSourceInput {
                    root,
                    dependencies: package.dependencies().clone(),
                    kind: package.kind(),
                },
            ))
        })
        .collect::<Result<_, CheckError>>()?;

    check_prepared(PreparedCheck {
        root_package,
        main,
        packages: package_inputs,
        cached_sources,
        ownership,
        source_overrides,
        sources,
        config,
    })
}

struct PreparedCheck {
    root_package: PackageId,
    main: PackageModuleInput,
    packages: HashMap<PackageId, PackageSourceInput>,
    cached_sources: Vec<PackageModuleInput>,
    ownership: SourceOwnership,
    source_overrides: Vec<SourceOverride>,
    sources: SourceBundle,
    config: FrontendConfig,
}

fn check_prepared(input: PreparedCheck) -> CheckResult {
    let PreparedCheck {
        root_package,
        main,
        mut packages,
        cached_sources,
        ownership,
        source_overrides,
        sources,
        mut config,
    } = input;
    if let Some(core) = system_package_input(PackageId::core(), sources.core()) {
        packages.insert(PackageId::core(), core);
    }
    if let Some(std) = system_package_input(PackageId::std(), sources.std()) {
        packages.insert(PackageId::std(), std);
    }
    let mut source_loader = PackageSourceEnvironment::new(ownership, &sources);
    source_loader.cache_overrides(source_overrides)?;
    source_loader.cache_sources(cached_sources);

    let ok = pipeline::check_packages(
        PackageProgramInput {
            root_package,
            main,
            system: SystemPackages {
                core: sources.core().map(|_| PackageId::core()),
                std: sources.std().map(|_| PackageId::std()),
            },
            packages,
            preloaded_modules: preloaded_modules(&sources),
            source_loader: &mut source_loader,
        },
        {
            config.externs = system_externs(&sources);
            config
        },
    )?;

    Ok(ok.into())
}

fn read_main(file: &Path, overrides: &[SourceOverride]) -> Result<String, CheckError> {
    if let Some(source) = source_override(file, overrides)? {
        return Ok(source.code().to_string());
    }
    fs::read_to_string(file).map_err(|error| CheckError::ReadMain {
        path: file.to_path_buf(),
        message: error.to_string(),
    })
}

fn source_override<'a>(
    file: &Path,
    overrides: &'a [SourceOverride],
) -> Result<Option<&'a SourceOverride>, CheckError> {
    let Ok(source_file) = source_file_id(file) else {
        return Ok(None);
    };
    for source in overrides {
        let override_file = source_file_id(source.path())?;
        if override_file == source_file {
            return Ok(Some(source));
        }
    }
    Ok(None)
}

fn package_root(
    package: &PackageSource,
    root_package: &PackageId,
    ownership: &SourceOwnership,
    source_overrides: &[SourceOverride],
) -> Result<Option<PackageModuleInput>, CheckError> {
    if package.id() == root_package || package.kind() == PackageKind::NativeOnly {
        return Ok(None);
    }
    let Some(entry) = package.entry() else {
        return Err(CheckError::InvalidInput(format!(
            "source package '{}' is missing an entry path",
            package.id()
        )));
    };
    let (owner, source_file) = ownership.validate_root_file(entry)?;
    Ok(Some(PackageModuleInput {
        module: ModuleId::source(owner, source_file.clone()),
        source: read_package_root(package, source_file.path(), source_overrides)?
            .to_frontend_source(),
    }))
}

fn read_package_root(
    package: &PackageSource,
    path: &Path,
    source_overrides: &[SourceOverride],
) -> Result<SourceText, CheckError> {
    let Some(entry) = package.entry() else {
        return Err(CheckError::InvalidInput(format!(
            "source package '{}' is missing an entry path",
            package.id()
        )));
    };
    let code = read_main(entry, source_overrides)?;
    SourceText::new(code, entry.display().to_string())
        .map(|source| source.with_path(path.to_path_buf()))
}

fn system_externs(sources: &SourceBundle) -> ExternInputs {
    let mut packages = vec![];
    if let Some(core) = sources.core()
        && !core.providers().is_empty()
    {
        packages.push(PackageExternInputs {
            package: PackageId::core(),
            providers: core.providers().to_vec(),
        });
    }
    if let Some(std) = sources.std()
        && !std.providers().is_empty()
    {
        packages.push(PackageExternInputs {
            package: PackageId::std(),
            providers: std.providers().to_vec(),
        });
    }
    ExternInputs { packages }
}

fn system_package_input(
    package: PackageId,
    source: Option<&crate::SystemPackageSource>,
) -> Option<PackageSourceInput> {
    source.map(|source| PackageSourceInput {
        root: Some(PackageModuleInput {
            module: ModuleId::root(package),
            source: source.root().to_frontend_source(),
        }),
        dependencies: HashMap::new(),
        kind: PackageKind::Source,
    })
}

fn preloaded_modules(sources: &SourceBundle) -> Vec<PackageModuleInput> {
    let mut modules = vec![];
    preload_system_package(&mut modules, &PackageId::core(), sources.core());
    preload_system_package(&mut modules, &PackageId::std(), sources.std());
    modules
}

fn preload_system_package(
    modules: &mut Vec<PackageModuleInput>,
    package: &PackageId,
    source: Option<&crate::SystemPackageSource>,
) {
    let Some(source) = source else {
        return;
    };
    modules.push(PackageModuleInput {
        module: ModuleId::root(package.clone()),
        source: source.root().to_frontend_source(),
    });
    modules.extend(source.modules().iter().map(|module| PackageModuleInput {
        module: ModuleId::named(package.clone(), module_path(module.path().to_vec())),
        source: module.to_frontend_source(),
    }));
}

fn module_path(path: Vec<String>) -> anvyx_frontend::resolve::ModulePath {
    anvyx_frontend::resolve::ModulePath::new(path).expect("SourceBundle validates module paths")
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::pipeline::CheckError as FrontendCheckError;

    use super::*;
    use crate::{ModuleSource, SystemPackageSource};

    fn write(dir: &tempfile::TempDir, relative: &str, code: &str) -> PathBuf {
        let file = dir.path().join(relative);
        if let Some(parent) = file.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(&file, code).unwrap();
        file
    }

    fn path(segments: &[&str]) -> Vec<String> {
        segments
            .iter()
            .map(|segment| (*segment).to_string())
            .collect()
    }

    fn input(file: PathBuf, sources: SourceBundle) -> CheckFileInput {
        CheckFileInput::new(file, sources).unwrap()
    }

    fn empty_input(file: PathBuf) -> CheckFileInput {
        input(file, SourceBundle::default())
    }

    fn core_module(name: &str, code: &str) -> ModuleSource {
        ModuleSource::new(path(&[name]), code, format!("<core.{name}>")).unwrap()
    }

    fn std_module(name: &str, code: &str) -> ModuleSource {
        ModuleSource::new(path(&[name]), code, format!("<std.{name}>")).unwrap()
    }

    fn system(root: SourceText, modules: Vec<ModuleSource>) -> SystemPackageSource {
        SystemPackageSource::new(root, modules).unwrap()
    }

    fn std_root(modules: &[ModuleSource]) -> SourceText {
        let code = modules
            .iter()
            .map(|module| format!("pub import {};", module.path().join(".")))
            .collect::<Vec<_>>()
            .join("\n");
        SourceText::new(code, "<std>").unwrap()
    }

    fn bundle(
        prelude: Option<SourceText>,
        core_modules: Vec<ModuleSource>,
        std_modules: Vec<ModuleSource>,
    ) -> SourceBundle {
        let core = (prelude.is_some() || !core_modules.is_empty()).then(|| {
            system(
                prelude.unwrap_or_else(|| SourceText::new("", "<core>").unwrap()),
                core_modules,
            )
        });
        let std = (!std_modules.is_empty()).then(|| system(std_root(&std_modules), std_modules));
        SourceBundle::new(core, std)
    }

    fn unwrap_error(result: CheckResult) -> CheckError {
        match result {
            Ok(_) => panic!("expected check error"),
            Err(error) => error,
        }
    }

    fn package_source(
        id: PackageId,
        entry: PathBuf,
        source_root: PathBuf,
        dependencies: HashMap<String, PackageId>,
    ) -> PackageSource {
        PackageSource::new(id, entry, source_root, dependencies).unwrap()
    }

    mod input {
        use super::*;

        #[test]
        fn rejects_empty_path() {
            let error = CheckFileInput::new(PathBuf::new(), SourceBundle::default()).unwrap_err();

            assert!(matches!(
                error,
                CheckError::InvalidInput(message)
                    if message.contains("main source path must not be empty")
            ));
        }
    }

    mod package {
        use super::*;

        fn package_id(name: &str) -> PackageId {
            PackageId::new(name)
        }

        #[test]
        fn resolves_dependency_root_import() {
            let temp = tempfile::tempdir().unwrap();
            let game_main = write(&temp, "game/src/main.anv", "import pkg:math; fn main() {}");
            let math_root = write(&temp, "math/src/lib.anv", "pub fn add() -> int { 1 }");
            let canonical_math_root = fs::canonicalize(&math_root).unwrap();
            let game = package_id("game");
            let math = package_id("math");
            let packages = vec![
                package_source(
                    game.clone(),
                    game_main.clone(),
                    temp.path().join("game/src"),
                    HashMap::from([("math".to_string(), math.clone())]),
                ),
                package_source(
                    math,
                    math_root,
                    temp.path().join("math/src"),
                    HashMap::new(),
                ),
            ];
            let input =
                CheckPackageInput::new(game, game_main, packages, SourceBundle::default()).unwrap();

            let ok = check_package(input).unwrap();

            assert!(
                ok.report
                    .sources
                    .iter()
                    .any(|source| source.path() == Some(canonical_math_root.as_path()))
            );
        }

        #[test]
        fn native_only_root_package_is_rejected() {
            let temp = tempfile::tempdir().unwrap();
            let game_main = write(&temp, "game/src/main.anv", "fn main() {}");
            let game = package_id("game");
            let packages = vec![PackageSource::native_only(game.clone(), HashMap::new())];
            let error =
                CheckPackageInput::new(game.clone(), game_main, packages, SourceBundle::default())
                    .unwrap_err();

            assert!(matches!(
                error,
                CheckError::InvalidInput(message)
                    if message.contains("has no source entry") && message.contains("game")
            ));
        }

        #[test]
        fn unknown_dependency_alias_is_resolve_error() {
            let temp = tempfile::tempdir().unwrap();
            let game_main = write(&temp, "game/src/main.anv", "import pkg:math; fn main() {}");
            let game = package_id("game");
            let packages = vec![package_source(
                game.clone(),
                game_main.clone(),
                temp.path().join("game/src"),
                HashMap::new(),
            )];
            let input =
                CheckPackageInput::new(game, game_main, packages, SourceBundle::default()).unwrap();
            let error = unwrap_error(check_package(input));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Resolve { .. })
            ));
        }

        #[test]
        fn outside_file_cannot_use_deps() {
            let temp = tempfile::tempdir().unwrap();
            let game_main = write(&temp, "game/src/main.anv", "import ..outside; fn main() {}");
            write(
                &temp,
                "game/outside.anv",
                "import pkg:math; pub fn value() -> int { 1 }",
            );
            let math_root = write(&temp, "math/src/lib.anv", "pub fn add() -> int { 1 }");
            let game = package_id("game");
            let math = package_id("math");
            let packages = vec![
                package_source(
                    game.clone(),
                    game_main.clone(),
                    temp.path().join("game/src"),
                    HashMap::from([("math".to_string(), math.clone())]),
                ),
                package_source(
                    math,
                    math_root,
                    temp.path().join("math/src"),
                    HashMap::new(),
                ),
            ];
            let input =
                CheckPackageInput::new(game, game_main, packages, SourceBundle::default()).unwrap();
            let error = unwrap_error(check_package(input));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Resolve { report })
                    if report.diagnostics()[0].message().contains("has no package dependency named 'math'")
            ));
        }
    }

    mod file {
        use super::*;

        #[test]
        fn reads_main() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "fn main() {}");
            let canonical = fs::canonicalize(&main).unwrap();

            let ok = check_file(empty_input(main)).unwrap();

            assert!(
                ok.report
                    .sources
                    .iter()
                    .any(|source| source.path() == Some(canonical.as_path()))
            );
        }

        #[test]
        fn reports_read_main_error() {
            let temp = tempfile::tempdir().unwrap();
            let missing = temp.path().join("missing.anv");
            let error = unwrap_error(check_file(empty_input(missing.clone())));

            assert!(matches!(
                error,
                CheckError::ReadMain { path, message } if path == missing && !message.is_empty()
            ));
        }

        #[test]
        fn checks_missing_main_from_source_override() {
            let temp = tempfile::tempdir().unwrap();
            let main = temp.path().join("main.anv");
            let input = empty_input(main.clone()).with_source_overrides(vec![
                SourceOverride::new(main.clone(), "fn main() {}").unwrap(),
            ]);

            let ok = check_file(input).unwrap();
            let expected = source_file_id(&main).unwrap().path().to_path_buf();

            assert!(
                ok.report
                    .sources
                    .iter()
                    .any(|source| source.path() == Some(expected.as_path()))
            );
        }

        #[test]
        fn imports_missing_module_from_source_override() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import helper { value }; fn main() { let x: int = value(); }",
            );
            let helper = temp.path().join("helper.anv");
            let input = empty_input(main).with_source_overrides(vec![
                SourceOverride::new(helper, "pub fn value() -> int { 1 }").unwrap(),
            ]);

            check_file(input).unwrap();
        }

        #[test]
        fn preserves_main_label() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "fn main( {}");
            let error = unwrap_error(check_file(empty_input(main.clone())));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Parse { label, .. })
                    if label == main.display().to_string()
            ));
        }

        #[test]
        fn uses_core_prelude() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "fn main() { let x: int = prelude_value(); }",
            );
            let prelude = SourceText::new("pub fn prelude_value() -> int { 1 }", "<core>").unwrap();
            let sources = bundle(Some(prelude), vec![], vec![]);

            let ok = check_file(input(main, sources)).unwrap();

            assert!(
                ok.report
                    .sources
                    .iter()
                    .filter(|source| source.label().starts_with("<core"))
                    .all(|source| source.path().is_none())
            );
        }

        #[test]
        fn preloads_core_roots() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "fn main() { let x: int = value(); }");
            let prelude = SourceText::new("pub import core_helpers { value };", "<core>").unwrap();
            let sources = bundle(
                Some(prelude),
                vec![core_module("core_helpers", "pub fn value() -> int { 1 }")],
                vec![],
            );

            check_file(input(main, sources)).unwrap();
        }

        #[test]
        fn std_file_is_local_source() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import std { value }; fn main() { let x: int = value(); }",
            );
            write(&temp, "std.anv", "pub fn value() -> int { 1 }");

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn std_directory_is_local_source() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import std.foo { value }; fn main() { let x: int = value(); }",
            );
            write(&temp, "std/foo.anv", "pub fn value() -> int { 1 }");

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn imports_sibling() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import helper { value }; fn main() { let x: int = value(); }",
            );
            let helper = write(&temp, "helper.anv", "pub fn value() -> int { 1 }");
            let helper = fs::canonicalize(helper).unwrap();

            let ok = check_file(empty_input(main)).unwrap();

            assert!(
                ok.report
                    .sources
                    .iter()
                    .any(|source| source.path() == Some(helper.as_path()))
            );
        }

        #[test]
        fn imports_nested() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import foo.bar { value }; fn main() { let x: int = value(); }",
            );
            write(&temp, "foo/bar.anv", "pub fn value() -> int { 2 }");

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn imports_from_nested_folder() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "src/ui/button.anv",
                "import helper { value }; fn main() { let x: int = value(); }",
            );
            write(&temp, "src/ui/helper.anv", "pub fn value() -> int { 1 }");

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn dot_import_deduplicates() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "src/ui/button.anv",
                "import helper { value as a }; import .helper { value as b }; fn main() { let x: int = a() + b(); }",
            );
            write(&temp, "src/ui/helper.anv", "pub fn value() -> int { 1 }");

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn imports_from_parent_folder() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "src/ui/button.anv",
                "import ..common { value }; fn main() { let x: int = value(); }",
            );
            write(&temp, "src/common.anv", "pub fn value() -> int { 1 }");

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn dedups_equivalent_extern_imports() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import helper { tick as a }; import .helper { tick as b }; fn main() { let x: int = a() + b(); }",
            );
            write(&temp, "helper.anv", "pub extern fn tick() -> int;");

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn same_names_in_files_are_distinct() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import a { make }; import b { take }; fn main() { take(make()); }",
            );
            write(
                &temp,
                "a.anv",
                "pub struct Vec2 {} pub fn make() -> Vec2 { Vec2 {} }",
            );
            write(&temp, "b.anv", "pub struct Vec2 {} pub fn take(v: Vec2) {}");
            let error = unwrap_error(check_file(empty_input(main)));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Type { report })
                    if report.diagnostics().iter().any(|diagnostic| {
                        let message = diagnostic.message();
                        message.contains("a.anv") && message.contains("b.anv")
                    })
            ));
        }

        #[test]
        fn missing_import_is_resolve_error() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "import missing; fn main() {}");
            let error = unwrap_error(check_file(empty_input(main)));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Resolve { .. })
            ));
        }

        #[test]
        fn imports_std_module() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import std:math { PI }; fn main() { let x: int = PI; }",
            );
            let sources = bundle(
                None,
                vec![],
                vec![std_module("math", "pub const PI: int = 3;")],
            );

            check_file(input(main, sources)).unwrap();
        }

        #[test]
        fn default_bundle_has_no_std_package() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "import std:math; fn main() {}");
            let error = unwrap_error(check_file(empty_input(main)));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Resolve { .. })
            ));
        }

        #[test]
        fn std_is_not_preluded() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "fn main() { let x: int = PI; }");
            let sources = bundle(
                None,
                vec![],
                vec![std_module("math", "pub const PI: int = 3;")],
            );
            let error = unwrap_error(check_file(input(main, sources)));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Type { .. })
            ));
        }

        #[test]
        fn std_receives_core_prelude() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import std:math { value }; fn main() { let x: int = value(); }",
            );
            let prelude = SourceText::new("pub fn core_value() -> int { 1 }", "<core>").unwrap();
            let sources = bundle(
                Some(prelude),
                vec![],
                vec![std_module("math", "pub fn value() -> int { core_value() }")],
            );

            check_file(input(main, sources)).unwrap();
        }

        #[test]
        fn module_read_error_stays_lang2() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "import bad; fn main() {}");
            let bad = temp.path().join("bad.anv");
            fs::create_dir_all(&bad).unwrap();
            let error = unwrap_error(check_file(empty_input(main)));

            let bad = fs::canonicalize(&bad).unwrap_or(bad);
            assert!(matches!(
                error,
                CheckError::ReadModule { path, message } if path == bad && !message.is_empty()
            ));
        }

        #[test]
        fn loaded_parse_label() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "import broken; fn main() {}");
            let broken = write(&temp, "broken.anv", "fn nope( {}");
            let error = unwrap_error(check_file(empty_input(main)));

            let broken = fs::canonicalize(broken).unwrap();
            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Parse { label, .. })
                    if label == broken.display().to_string()
            ));
        }

        #[test]
        fn loaded_lex_label() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "import broken; fn main() {}");
            let broken = write(&temp, "broken.anv", "fn main() { \"unterminated }");
            let error = unwrap_error(check_file(empty_input(main)));

            let broken = fs::canonicalize(broken).unwrap();
            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Lex { label, .. })
                    if label == broken.display().to_string()
            ));
        }

        #[test]
        fn loaded_declaration_errors_use_loaded_source() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "import helper; fn main() {}");
            let helper = write(&temp, "helper.anv", "pub fn bad(a: int = 1, b: int) {}");
            let helper = fs::canonicalize(helper).unwrap();
            let error = unwrap_error(check_file(empty_input(main)));
            let CheckError::Frontend(FrontendCheckError::Type { report }) = error else {
                panic!("expected type error");
            };
            let label = report.diagnostics()[0].labels()[0].span;
            let file = report.sources.get(label.source()).unwrap();

            assert_eq!(file.path(), Some(helper.as_path()));
        }

        #[test]
        fn helper_directives_are_source() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import helper { value }; fn main() { let x: int = value(); }",
            );
            write(
                &temp,
                "helper.anv",
                "// @mode: run\n// @expect_stdout: ignored by lang2\npub fn value() -> int { 1 }",
            );

            check_file(empty_input(main)).unwrap();
        }

        #[test]
        fn core_root_reexported_extend_visible() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "fn main() { let x: int = 1.plus_one(); }",
            );
            let prelude = SourceText::new("pub import core_int { * };", "<core>").unwrap();
            let sources = bundle(
                Some(prelude),
                vec![core_module(
                    "core_int",
                    "pub extend int { fn plus_one(self) -> int { self + 1 } }",
                )],
                vec![],
            );

            check_file(input(main, sources)).unwrap();
        }
    }
}
