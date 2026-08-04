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

    pub fn source_overrides(&self) -> &[SourceOverride] {
        &self.source_overrides
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckPackageInput {
    root_package: PackageId,
    root_file: PathBuf,
    packages: Vec<PackageSource>,
    sources: SourceBundle,
    externs: Vec<PackageExternInputs>,
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
            externs: vec![],
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

    #[must_use]
    pub fn with_package_externs(
        mut self,
        externs: Vec<(PackageId, Vec<anvyx_externs::ProviderDescriptor>)>,
    ) -> Self {
        self.externs = externs
            .into_iter()
            .map(|(package, providers)| PackageExternInputs { package, providers })
            .collect();
        self
    }

    pub fn root_package(&self) -> &PackageId {
        &self.root_package
    }

    pub fn root_file(&self) -> &Path {
        &self.root_file
    }

    pub fn packages(&self) -> &[PackageSource] {
        &self.packages
    }

    pub fn sources(&self) -> &SourceBundle {
        &self.sources
    }

    pub fn source_overrides(&self) -> &[SourceOverride] {
        &self.source_overrides
    }
}

pub fn build_air_file(
    input: CheckFileInput,
) -> Result<pipeline::AirBuildOutput, pipeline::AirBuildError<CheckError>> {
    build_air_prepared(prepare_file(input).map_err(pipeline::AirBuildError::Fatal)?)
}

pub fn check_file(input: CheckFileInput) -> CheckResult {
    check_prepared(prepare_file(input)?)
}

pub fn build_air_package(
    input: CheckPackageInput,
) -> Result<pipeline::AirBuildOutput, pipeline::AirBuildError<CheckError>> {
    build_air_prepared(prepare_package(input).map_err(pipeline::AirBuildError::Fatal)?)
}

pub fn check_package(input: CheckPackageInput) -> CheckResult {
    check_prepared(prepare_package(input)?)
}

fn prepare_file(input: CheckFileInput) -> Result<PreparedCheck, CheckError> {
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

    Ok(PreparedCheck {
        root_package: PackageId::synthetic_root(),
        main: main.clone(),
        packages: HashMap::new(),
        cached_sources: vec![main],
        ownership: SourceOwnership::new(&[])?,
        externs: vec![],
        source_overrides,
        sources,
        config,
    })
}

fn prepare_package(input: CheckPackageInput) -> Result<PreparedCheck, CheckError> {
    let CheckPackageInput {
        root_package,
        root_file,
        packages,
        sources,
        externs,
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

    Ok(PreparedCheck {
        root_package,
        main,
        packages: package_inputs,
        cached_sources,
        ownership,
        externs,
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
    externs: Vec<PackageExternInputs>,
    source_overrides: Vec<SourceOverride>,
    sources: SourceBundle,
    config: FrontendConfig,
}

fn build_air_prepared(
    input: PreparedCheck,
) -> Result<pipeline::AirBuildOutput, pipeline::AirBuildError<CheckError>> {
    run_prepared(input, |input, config| {
        pipeline::build_air_packages(input, config, &pipeline::AirRootConfig::entry_main())
    })
    .map_err(pipeline::AirBuildError::Fatal)?
}

fn check_prepared(input: PreparedCheck) -> CheckResult {
    run_prepared(input, run_check_packages)?
}

fn run_check_packages(
    input: PackageProgramInput<'_, PackageSourceEnvironment<'_>>,
    config: FrontendConfig,
) -> CheckResult {
    pipeline::check_packages(input, config)
}

fn run_prepared<T>(
    input: PreparedCheck,
    run: impl for<'a, 'b> FnOnce(
        PackageProgramInput<'a, PackageSourceEnvironment<'b>>,
        FrontendConfig,
    ) -> T,
) -> Result<T, CheckError> {
    let PreparedCheck {
        root_package,
        main,
        mut packages,
        cached_sources,
        ownership,
        externs,
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
    let input = PackageProgramInput {
        root_package,
        main,
        system: SystemPackages {
            core: sources.core().map(|_| PackageId::core()),
            std: sources.std().map(|_| PackageId::std()),
        },
        packages,
        preloaded_modules: preloaded_modules(&sources),
        source_loader: &mut source_loader,
    };
    config.externs = system_externs(&sources, externs);
    Ok(run(input, config))
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

fn system_externs(sources: &SourceBundle, mut packages: Vec<PackageExternInputs>) -> ExternInputs {
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
