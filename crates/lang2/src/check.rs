use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use anvyx_frontend::{
    pipeline::{
        self, FrontendConfig, PackageModuleInput, PackageProgramInput, PackageSourceInput,
        Source as FrontendSource,
    },
    resolve::{ModuleId, PackageId, SystemPackages},
};

use crate::{
    CheckError, CheckOk, CheckResult, ModuleSource, PackageSource, SourceBundle, SourceText,
    source::{PackageSourceEnvironment, SourceOwnership, canonical_source_file},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckFileInput {
    file: PathBuf,
    sources: SourceBundle,
}

impl CheckFileInput {
    pub fn new(file: impl Into<PathBuf>, sources: SourceBundle) -> Result<Self, CheckError> {
        let file = file.into();
        if file.as_os_str().is_empty() {
            return Err(CheckError::InvalidInput(
                "main source path must not be empty".to_string(),
            ));
        }

        Ok(Self { file, sources })
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
        if !packages.iter().any(|package| package.id() == &root_package) {
            return Err(CheckError::InvalidInput(format!(
                "root package '{root_package}' is missing from package input"
            )));
        }
        Ok(Self {
            root_package,
            root_file,
            packages,
            sources,
        })
    }
}

pub fn check_file(input: CheckFileInput) -> CheckResult {
    let CheckFileInput { file, sources } = input;
    let main_code = read_main(&file)?;
    let root_file_id = canonical_source_file(&file)?;
    let main = PackageModuleInput {
        module: ModuleId::source_without_package(root_file_id),
        source: FrontendSource {
            code: main_code,
            label: file.display().to_string(),
        },
    };

    check_prepared(PreparedCheck {
        root_package: PackageId::synthetic_root(),
        main: main.clone(),
        packages: HashMap::new(),
        cached_sources: vec![main],
        ownership: SourceOwnership::new(&[])?,
        sources,
    })
}

pub fn check_package(input: CheckPackageInput) -> CheckResult {
    let CheckPackageInput {
        root_package,
        root_file,
        packages,
        sources,
    } = input;
    let main_code = read_main(&root_file)?;
    let ownership = SourceOwnership::new(&packages)?;
    let (root_owner, root_file_id) = ownership.validate_root_file(&root_file)?;
    let main = PackageModuleInput {
        module: ModuleId::source(root_owner, root_file_id),
        source: FrontendSource {
            code: main_code,
            label: root_file.display().to_string(),
        },
    };
    let mut cached_sources = vec![main.clone()];
    let package_inputs = packages
        .iter()
        .map(|package| {
            let root = package_root(package, &root_package, &ownership)?;
            if let Some(root) = &root {
                cached_sources.push(root.clone());
            }
            Ok((
                package.id().clone(),
                PackageSourceInput {
                    root,
                    dependencies: package.dependencies().clone(),
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
        sources,
    })
}

struct PreparedCheck {
    root_package: PackageId,
    main: PackageModuleInput,
    packages: HashMap<PackageId, PackageSourceInput>,
    cached_sources: Vec<PackageModuleInput>,
    ownership: SourceOwnership,
    sources: SourceBundle,
}

fn check_prepared(input: PreparedCheck) -> CheckResult {
    let PreparedCheck {
        root_package,
        main,
        mut packages,
        cached_sources,
        ownership,
        sources,
    } = input;
    let std_tree = StdModuleTree::new(&sources);
    if let Some(core) = core_package_input(&sources) {
        packages.insert(PackageId::core(), core);
    }
    packages.insert(PackageId::std(), std_package_input(&std_tree));
    let mut source_loader = PackageSourceEnvironment::new(ownership, &sources);
    source_loader.cache_sources(cached_sources);

    pipeline::check_packages(
        PackageProgramInput {
            root_package,
            main,
            system: SystemPackages {
                core: core_package(&sources),
                std: Some(PackageId::std()),
            },
            packages,
            preloaded_modules: preloaded_modules(&sources, &std_tree),
            always_active_modules: always_active_modules(&sources),
            source_loader: &mut source_loader,
        },
        FrontendConfig::default(),
    )?;

    Ok(CheckOk)
}

fn read_main(file: &Path) -> Result<String, CheckError> {
    fs::read_to_string(file).map_err(|error| CheckError::ReadMain {
        path: file.to_path_buf(),
        message: error.to_string(),
    })
}

fn package_root(
    package: &PackageSource,
    root_package: &PackageId,
    ownership: &SourceOwnership,
) -> Result<Option<PackageModuleInput>, CheckError> {
    if package.id() == root_package {
        return Ok(None);
    }
    let (owner, source_file) = ownership.validate_root_file(package.entry())?;
    Ok(Some(PackageModuleInput {
        module: ModuleId::source(owner, source_file),
        source: read_package_root(package)?.to_frontend_source(),
    }))
}

fn read_package_root(package: &PackageSource) -> Result<SourceText, CheckError> {
    let code = read_main(package.entry())?;
    SourceText::new(code, package.entry().display().to_string())
}

fn core_package(sources: &SourceBundle) -> Option<PackageId> {
    (sources.core_prelude().is_some() || !sources.core_modules().is_empty()).then(PackageId::core)
}

fn core_package_input(sources: &SourceBundle) -> Option<PackageSourceInput> {
    core_package(sources).map(|_| PackageSourceInput {
        root: Some(PackageModuleInput {
            module: ModuleId::root(PackageId::core()),
            source: sources
                .core_prelude()
                .map_or_else(empty_core_source, SourceText::to_frontend_source),
        }),
        dependencies: HashMap::new(),
    })
}

fn std_package_input(tree: &StdModuleTree) -> PackageSourceInput {
    PackageSourceInput {
        root: Some(PackageModuleInput {
            module: ModuleId::root(PackageId::std()),
            source: FrontendSource {
                code: tree.source(&[]),
                label: "<std>".to_string(),
            },
        }),
        dependencies: HashMap::new(),
    }
}

fn empty_core_source() -> FrontendSource {
    FrontendSource {
        code: String::new(),
        label: "<core>".to_string(),
    }
}

fn preloaded_modules(sources: &SourceBundle, std_tree: &StdModuleTree) -> Vec<PackageModuleInput> {
    let mut modules = vec![];
    if let Some(core) = core_package_input(sources).and_then(|package| package.root) {
        modules.push(core);
    }
    modules.extend(
        sources
            .core_modules()
            .iter()
            .map(|module| PackageModuleInput {
                module: ModuleId::named(PackageId::core(), module_path(module.path().to_vec())),
                source: module.to_frontend_source(),
            }),
    );
    modules.extend(std_package_modules(std_tree));
    modules
}

fn std_package_modules(tree: &StdModuleTree<'_>) -> Vec<PackageModuleInput> {
    let mut modules = vec![];
    for module in &tree.modules {
        modules.push(PackageModuleInput {
            module: ModuleId::named(PackageId::std(), module_path(module.path()[1..].to_vec())),
            source: module.to_frontend_source(),
        });
    }
    for prefix in tree.prefixes() {
        modules.push(PackageModuleInput {
            module: ModuleId::named(PackageId::std(), module_path(prefix.clone())),
            source: FrontendSource {
                code: tree.source(&prefix),
                label: format!("<std.{}>", prefix.join(".")),
            },
        });
    }
    modules
}

struct StdModuleTree<'a> {
    modules: Vec<&'a ModuleSource>,
    existing: HashSet<Vec<String>>,
    children: BTreeMap<Vec<String>, BTreeSet<String>>,
}

impl<'a> StdModuleTree<'a> {
    fn new(sources: &'a SourceBundle) -> Self {
        let mut modules = vec![];
        let mut existing = HashSet::new();
        let mut children = BTreeMap::<Vec<String>, BTreeSet<String>>::new();
        for module in sources.std_modules() {
            modules.push(module);
            let path = &module.path()[1..];
            existing.insert(path.to_vec());
            for len in 0..path.len() {
                children
                    .entry(path[..len].to_vec())
                    .or_default()
                    .insert(path[len].clone());
            }
        }
        Self {
            modules,
            existing,
            children,
        }
    }

    fn prefixes(&self) -> Vec<Vec<String>> {
        self.children
            .keys()
            .filter(|path| !path.is_empty() && !self.existing.contains(*path))
            .cloned()
            .collect()
    }

    fn source(&self, prefix: &[String]) -> String {
        self.children
            .get(prefix)
            .into_iter()
            .flatten()
            .map(|child| {
                let mut path = prefix.to_vec();
                path.push(child.clone());
                format!("pub import {};", path.join("."))
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}

fn always_active_modules(sources: &SourceBundle) -> Vec<ModuleId> {
    sources
        .always_active_modules()
        .iter()
        .map(|path| ModuleId::named(PackageId::core(), module_path(path.clone())))
        .collect()
}

fn module_path(path: Vec<String>) -> anvyx_frontend::resolve::ModulePath {
    anvyx_frontend::resolve::ModulePath::new(path).expect("SourceBundle validates module paths")
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::pipeline::CheckError as FrontendCheckError;

    use super::*;
    use crate::ModuleSource;

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
        ModuleSource::new(path(&["std", name]), code, format!("<std.{name}>")).unwrap()
    }

    fn bundle(
        prelude: Option<SourceText>,
        core_modules: Vec<ModuleSource>,
        std_modules: Vec<ModuleSource>,
        always_active_modules: Vec<Vec<String>>,
    ) -> SourceBundle {
        SourceBundle::new(prelude, core_modules, std_modules, always_active_modules).unwrap()
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

            check_package(input).unwrap();
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
                CheckError::Frontend(FrontendCheckError::Resolve { diagnostics })
                    if diagnostics[0].message().contains("has no package dependency named 'math'")
            ));
        }
    }

    mod file {
        use super::*;

        #[test]
        fn reads_main() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "fn main() {}");

            check_file(empty_input(main)).unwrap();
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
            let sources = bundle(Some(prelude), vec![], vec![], vec![]);

            check_file(input(main, sources)).unwrap();
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
            write(&temp, "helper.anv", "pub fn value() -> int { 1 }");

            check_file(empty_input(main)).unwrap();
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
            write(&temp, "helper.anv", "extern fn tick() -> int;");

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
                CheckError::Frontend(FrontendCheckError::Type { diagnostics })
                    if diagnostics.iter().any(|diagnostic| {
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
                vec![],
            );

            check_file(input(main, sources)).unwrap();
        }

        #[test]
        fn std_is_not_preluded() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "fn main() { let x: int = PI; }");
            let sources = bundle(
                None,
                vec![],
                vec![std_module("math", "pub const PI: int = 3;")],
                vec![],
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
                vec![],
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
        fn always_active_extend_visible() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "fn main() { let x: int = 1.plus_one(); }",
            );
            let sources = bundle(
                None,
                vec![core_module(
                    "core_int",
                    "pub extend int { fn plus_one(self) -> int { self + 1 } }",
                )],
                vec![],
                vec![path(&["core_int"])],
            );

            check_file(input(main, sources)).unwrap();
        }
    }
}
