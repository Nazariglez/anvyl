use std::{
    fs,
    path::{Path, PathBuf},
};

use anvyx_frontend::pipeline::{self, FrontendConfig, ProgramInput, Source as FrontendSource};

use crate::{
    CheckError, CheckOk, CheckResult, ModuleSource, SourceBundle, SourceText,
    source::SourceEnvironment,
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

pub fn check_file(input: CheckFileInput) -> CheckResult {
    let CheckFileInput { file, sources } = input;
    let code = read_main(&file)?;
    let root = source_root(&file);
    let mut source_loader = SourceEnvironment::new(root, &sources);

    pipeline::check(
        ProgramInput {
            main: FrontendSource {
                code,
                label: file.display().to_string(),
            },
            prelude: sources.core_prelude().map(SourceText::to_frontend_source),
            preloaded_modules: sources
                .core_modules()
                .iter()
                .map(ModuleSource::to_frontend_module)
                .collect(),
            always_active_modules: sources
                .always_active_modules()
                .iter()
                .map(|path| {
                    anvyx_frontend::resolve::ModulePath::new(path.clone())
                        .expect("SourceBundle validates module paths")
                })
                .collect(),
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

fn source_root(file: &Path) -> PathBuf {
    file.parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf()
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::pipeline::CheckError as FrontendCheckError;

    use super::*;

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
        input(file, SourceBundle::empty())
    }

    fn std_module(segments: &[&str], code: &str, label: &str) -> ModuleSource {
        ModuleSource::new(path(segments), code, label).unwrap()
    }

    fn core_module(name: &str, code: &str) -> ModuleSource {
        ModuleSource::new(path(&[name]), code, format!("<core.{name}>")).unwrap()
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

    mod input {
        use super::*;

        #[test]
        fn rejects_empty_path() {
            let error = CheckFileInput::new(PathBuf::new(), SourceBundle::empty()).unwrap_err();

            assert!(matches!(
                error,
                CheckError::InvalidInput(message)
                    if message.contains("main source path must not be empty")
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
            let prelude = SourceText::new("fn prelude_value() -> int { 1 }", "<core>").unwrap();
            let sources = bundle(Some(prelude), vec![], vec![], vec![]);

            check_file(input(main, sources)).unwrap();
        }

        #[test]
        fn preloads_core_roots() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import core_helpers { value }; fn main() { let x: int = value(); }",
            );
            let sources = bundle(
                None,
                vec![core_module("core_helpers", "pub fn value() -> int { 1 }")],
                vec![],
                vec![],
            );

            check_file(input(main, sources)).unwrap();
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
        fn imports_std_bundle() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import std.math { sqrt }; fn main() { let x: float = sqrt(4.0); }",
            );
            let sources = bundle(
                None,
                vec![],
                vec![std_module(
                    &["std", "math"],
                    "pub fn sqrt(x: float) -> float { x }",
                    "<std.math>",
                )],
                vec![],
            );

            check_file(input(main, sources)).unwrap();
        }

        #[test]
        fn std_bundle_wins() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import std.math { sqrt }; fn main() { let x: float = sqrt(4.0); }",
            );
            write(&temp, "std/math.anv", "fn nope( {}");
            let sources = bundle(
                None,
                vec![],
                vec![std_module(
                    &["std", "math"],
                    "pub fn sqrt(x: float) -> float { x }",
                    "<std.math>",
                )],
                vec![],
            );

            check_file(input(main, sources)).unwrap();
        }

        #[test]
        fn unknown_std_ignores_local_file() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(
                &temp,
                "main.anv",
                "import std.missing { value }; fn main() { let x: int = value(); }",
            );
            write(&temp, "std/missing.anv", "pub fn value() -> int { 1 }");
            let error = unwrap_error(check_file(empty_input(main)));

            assert!(matches!(
                error,
                CheckError::Frontend(FrontendCheckError::Resolve { .. })
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
        fn module_read_error_stays_lang2() {
            let temp = tempfile::tempdir().unwrap();
            let main = write(&temp, "main.anv", "import bad; fn main() {}");
            let bad = temp.path().join("bad.anv");
            fs::create_dir_all(&bad).unwrap();
            let error = unwrap_error(check_file(empty_input(main)));

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
