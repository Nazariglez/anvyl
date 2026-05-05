use std::{collections::HashMap, fs, path::Path};

use anvyx_lang::{CompilationContext, LintConfig, LintLevel};
use anvyx_lang2::{
    CheckFileInput, CheckPackageInput, PackageId as FrontendPackageId, PackageSource, SourceBundle,
};

use crate::{
    manifest::{Manifest, PackageGraph, PackageId},
    std_support::{collect_core, collect_std},
};

pub fn cmd(
    file: &Path,
    extern_meta: &HashMap<String, String>,
    lint: LintConfig,
    ctx: &CompilationContext,
) -> Result<(), String> {
    let program = fs::read_to_string(file).map_err(|e| format!("Failed to read file: {e}"))?;
    let file_path = file.to_string_lossy().to_string();
    let (std_sources, _) = collect_std();
    let (core_prelude, core_sources, _) = collect_core();

    let _ast = anvyx_lang::generate_ast_with_std(
        &program,
        &file_path,
        &core_prelude,
        extern_meta,
        &std_sources,
        &core_sources,
        lint,
        ctx,
    )?;
    Ok(())
}

pub fn new_frontend_cmd(file: &Path) -> Result<(), String> {
    let sources = new_frontend_source_bundle()?;
    let manifest_path = Path::new("anvyx.toml");
    if manifest_path.exists() {
        let graph = crate::manifest::load_package_graph(manifest_path)?;
        let input = package_check_input(&graph, file, sources)?;
        anvyx_lang2::check_package(input).map_err(|error| error.to_string())?;
    } else {
        let input =
            CheckFileInput::new(file.to_path_buf(), sources).map_err(|error| error.to_string())?;
        anvyx_lang2::check_file(input).map_err(|error| error.to_string())?;
    }
    Ok(())
}

fn package_check_input(
    graph: &PackageGraph,
    file: &Path,
    sources: SourceBundle,
) -> Result<CheckPackageInput, String> {
    let root = graph.root();
    let root_id = frontend_package_id(&root.id);
    let packages = graph
        .packages()
        .iter()
        .map(|package| {
            let dependencies = package
                .dependencies
                .iter()
                .map(|(alias, id)| (alias.clone(), frontend_package_id(id)))
                .collect();
            match (&package.entry, &package.source_root) {
                (Some(entry), Some(source_root)) => PackageSource::new(
                    frontend_package_id(&package.id),
                    entry.clone(),
                    source_root.clone(),
                    dependencies,
                )
                .map_err(|error| error.to_string()),
                (None, None) => Ok(PackageSource::native_only(
                    frontend_package_id(&package.id),
                    dependencies,
                )),
                _ => Err(format!(
                    "package {} has inconsistent source entry/source root state",
                    package.id
                )),
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    CheckPackageInput::new(root_id, file.to_path_buf(), packages, sources)
        .map_err(|error| error.to_string())
}

fn frontend_package_id(id: &PackageId) -> FrontendPackageId {
    FrontendPackageId::new(id.manifest_path().display().to_string())
}

pub fn reject_new_frontend_inputs(
    manifest: Option<&Manifest>,
    lint_overrides: &[String],
    features: &[String],
    cfgs: &[String],
) -> Result<(), String> {
    if !features.is_empty() {
        return Err("--new-frontend does not support --feature yet".to_string());
    }
    if !cfgs.is_empty() {
        return Err("--new-frontend does not support --cfg yet".to_string());
    }
    if !lint_overrides.is_empty() {
        return Err("--new-frontend does not support --lint yet".to_string());
    }

    let Some(manifest) = manifest else {
        return Ok(());
    };

    if manifest.lint.internal_access != LintLevel::Warn {
        return Err("--new-frontend does not support lint configuration yet".to_string());
    }
    if manifest.has_externs() {
        return Err("--new-frontend does not support extern providers yet".to_string());
    }

    Ok(())
}

fn new_frontend_source_bundle() -> Result<SourceBundle, String> {
    crate::frontend_sources::source_bundle()
}

#[cfg(test)]
mod tests {
    use anvyx_lang::{LintConfig, StdModuleSource};

    use super::*;
    use crate::manifest::{DependencyEntry, ExternEntry, Project};

    fn source(code: &str) -> StdModuleSource {
        StdModuleSource {
            anv_source: code.to_string(),
        }
    }

    fn sources(items: &[(&str, &str)]) -> HashMap<String, StdModuleSource> {
        items
            .iter()
            .map(|(name, code)| ((*name).to_string(), source(code)))
            .collect()
    }

    fn path(segments: &[&str]) -> Vec<String> {
        segments
            .iter()
            .map(|segment| (*segment).to_string())
            .collect()
    }

    fn plain_manifest() -> Manifest {
        Manifest {
            project: Project {
                name: None,
                entry: Some("main.anv".to_string()),
            },
            dependencies: HashMap::new(),
            externs: HashMap::new(),
            lint: LintConfig::default(),
        }
    }

    fn unsupported_error(
        manifest: Option<&Manifest>,
        lint_overrides: &[String],
        features: &[String],
        cfgs: &[String],
    ) -> String {
        reject_new_frontend_inputs(manifest, lint_overrides, features, cfgs)
            .expect_err("input should be unsupported")
    }

    mod frontend {
        use super::*;

        fn write(dir: &tempfile::TempDir, relative: &str, code: &str) -> std::path::PathBuf {
            let file = dir.path().join(relative);
            if let Some(parent) = file.parent() {
                fs::create_dir_all(parent).unwrap();
            }
            fs::write(&file, code).unwrap();
            file
        }

        fn sorted_std_paths(bundle: &SourceBundle) -> Vec<Vec<String>> {
            let mut paths = bundle
                .std()
                .unwrap()
                .modules()
                .iter()
                .map(|module| module.path().to_vec())
                .collect::<Vec<_>>();
            paths.sort();
            paths
        }

        mod bundle {
            use anvyx_lang2::{ModuleSource, SourceText, SystemPackageSource};

            use super::*;

            fn bundle_from_sources(
                core_prelude: String,
                core_sources: HashMap<String, StdModuleSource>,
                std_sources: HashMap<String, StdModuleSource>,
            ) -> Result<SourceBundle, String> {
                let prelude =
                    SourceText::new(core_prelude, "<core>").map_err(|error| error.to_string())?;

                let core_modules = sorted_modules(core_sources)
                    .into_iter()
                    .map(|(name, source)| {
                        let path = vec![name.clone()];
                        let label = format!("<core.{name}>");
                        ModuleSource::new(path, source.anv_source, label)
                            .map_err(|error| error.to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let std_modules = sorted_modules(std_sources)
                    .into_iter()
                    .map(|(name, source)| {
                        let path = vec![name.clone()];
                        let label = format!("<std.{name}>");
                        ModuleSource::new(path, source.anv_source, label)
                            .map_err(|error| error.to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let std_root = std_modules
                    .iter()
                    .map(|module| format!("pub import {};", module.path().join(".")))
                    .collect::<Vec<_>>()
                    .join("\n");
                let core = SystemPackageSource::new(prelude, core_modules)
                    .map_err(|error| error.to_string())?;
                let std = SystemPackageSource::new(
                    SourceText::new(std_root, "<std>").map_err(|error| error.to_string())?,
                    std_modules,
                )
                .map_err(|error| error.to_string())?;

                Ok(SourceBundle::new(Some(core), Some(std)))
            }

            fn sorted_modules(
                sources: HashMap<String, StdModuleSource>,
            ) -> Vec<(String, StdModuleSource)> {
                let mut sources = sources.into_iter().collect::<Vec<_>>();
                sources.sort_by(|left, right| left.0.cmp(&right.0));
                sources
            }

            #[test]
            fn prelude() {
                let bundle =
                    bundle_from_sources("fn p() {}".to_string(), HashMap::new(), HashMap::new())
                        .unwrap();
                let prelude = bundle.core().unwrap().root();

                assert_eq!(prelude.label(), "<core>");
                assert_eq!(prelude.code(), "fn p() {}");
            }

            #[test]
            fn core_modules() {
                let bundle = bundle_from_sources(
                    String::new(),
                    sources(&[("core_int", "extend int {}")]),
                    HashMap::new(),
                )
                .unwrap();
                let modules = bundle.core().unwrap().modules();

                assert_eq!(modules.len(), 1);
                assert_eq!(modules[0].path(), path(&["core_int"]));
                assert_eq!(modules[0].label(), "<core.core_int>");
                assert_eq!(modules[0].code(), "extend int {}");
            }

            #[test]
            fn std_root() {
                let bundle = bundle_from_sources(
                    String::new(),
                    HashMap::new(),
                    sources(&[("math", "extern fn sin(x: float) -> float;")]),
                )
                .unwrap();
                let module = bundle.std().unwrap().module(&path(&["math"])).unwrap();

                assert_eq!(module.path(), path(&["math"]));
                assert_eq!(module.label(), "<std.math>");
                assert_eq!(module.code(), "extern fn sin(x: float) -> float;");
            }

            #[test]
            fn orders_modules() {
                let bundle = bundle_from_sources(
                    String::new(),
                    sources(&[
                        ("core_string", "extend string {}"),
                        ("core_int", "extend int {}"),
                    ]),
                    sources(&[("maps", ""), ("math", "")]),
                )
                .unwrap();
                let core_paths = bundle
                    .core()
                    .unwrap()
                    .modules()
                    .iter()
                    .map(|module| module.path().to_vec())
                    .collect::<Vec<_>>();

                assert_eq!(core_paths, [path(&["core_int"]), path(&["core_string"])]);
            }

            #[test]
            fn rejects_bad_builtin_name() {
                let error = bundle_from_sources(
                    String::new(),
                    sources(&[("", "extend int {}")]),
                    HashMap::new(),
                )
                .expect_err("empty built-in module name should be invalid");

                assert!(error.contains("module path must not contain empty segments"));
            }

            #[test]
            fn clean_frontend_core_surface_is_explicit() {
                let bundle = new_frontend_source_bundle().unwrap();
                let core = bundle.core().unwrap();
                let core_paths = core
                    .modules()
                    .iter()
                    .map(|module| module.path().to_vec())
                    .collect::<Vec<_>>();
                let expected = [
                    path(&["option"]),
                    path(&["result"]),
                    path(&["range"]),
                    path(&["core_int"]),
                    path(&["core_float"]),
                    path(&["core_string"]),
                ];

                assert_eq!(core.root().label(), "crates/core2/src/lib.anv");
                assert_eq!(core_paths, expected);
                assert!(core.root().code().contains("pub import core_int { * };"));
                assert!(core.root().code().contains("pub import core_float { * };"));
                assert!(core.root().code().contains("pub import core_string { * };"));
                assert!(core.root().code().contains("pub import option { * };"));
                assert!(core.root().code().contains("pub import result { * };"));
                assert!(core.root().code().contains("pub import range { * };"));
            }

            #[test]
            fn clean_frontend_std_surface_is_explicit() {
                let bundle = new_frontend_source_bundle().unwrap();

                assert_eq!(sorted_std_paths(&bundle), [path(&["mem"])]);
            }

            #[test]
            fn clean_frontend_bundle_has_no_legacy_type_spellings() {
                let bundle = new_frontend_source_bundle().unwrap();
                let mut code = String::new();
                code.push_str(bundle.core().unwrap().root().code());
                for module in bundle.core().unwrap().modules() {
                    code.push_str(module.code());
                }
                for module in bundle.std().unwrap().modules() {
                    code.push_str(module.code());
                }

                assert!(!code.contains("double"));
                assert!(!code.contains("PI_D"));
                assert!(!code.contains("EPSILON_D"));
                assert!(!code.contains("Option<string>"));
                assert!(!code.contains("Option<int>"));
                assert!(!code.contains("Option<float>"));
                assert!(!code.contains("Option<bool>"));
                assert!(!code.contains("Option<any>"));
                assert!(!code.contains("import ext:int"));
                assert!(!code.contains("import ext:float"));
                assert!(!code.contains("import ext:string"));
            }

            #[test]
            fn target_system_package_shape() {
                let bundle = new_frontend_source_bundle().unwrap();

                assert_eq!(
                    bundle.core().unwrap().root().label(),
                    "crates/core2/src/lib.anv"
                );
                assert_eq!(
                    bundle
                        .std()
                        .unwrap()
                        .module(&path(&["mem"]))
                        .unwrap()
                        .label(),
                    "crates/stdlib2/src/mem.anv"
                );
            }

            #[test]
            fn target_adapter_paths_removed() {
                let bundle = new_frontend_source_bundle().unwrap();
                let core_paths = bundle
                    .core()
                    .unwrap()
                    .modules()
                    .iter()
                    .map(|module| module.path().to_vec())
                    .collect::<Vec<_>>();

                assert_eq!(
                    core_paths,
                    [
                        path(&["option"]),
                        path(&["result"]),
                        path(&["range"]),
                        path(&["core_int"]),
                        path(&["core_float"]),
                        path(&["core_string"]),
                    ]
                );
                assert_eq!(sorted_std_paths(&bundle), [path(&["mem"])]);
            }
        }

        mod integration {
            use std::fmt::Write as _;

            use super::*;

            fn write_manifest(
                dir: &tempfile::TempDir,
                package: &str,
                entry: &str,
                deps: &[(&str, &str)],
            ) {
                let mut manifest = format!("[project]\nentry = \"{entry}\"\n");
                if !deps.is_empty() {
                    manifest.push_str("\n[dependencies]\n");
                    for (alias, path) in deps {
                        writeln!(manifest, "{alias} = {{ path = \"{path}\" }}").unwrap();
                    }
                }
                write(dir, &format!("{package}/anvyx.toml"), &manifest);
            }

            fn check_manifest_file(dir: &tempfile::TempDir, file: &Path) -> Result<(), String> {
                let graph =
                    crate::manifest::load_package_graph(&dir.path().join("game/anvyx.toml"))?;
                let sources = new_frontend_source_bundle()?;
                let input = package_check_input(&graph, file, sources)?;
                anvyx_lang2::check_package(input)
                    .map(|_| ())
                    .map_err(|error| error.to_string())
            }

            #[test]
            fn core_prelude_nominals_are_visible_without_import() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "fn takes_option(x: Option<int>) {} fn takes_ranges(a: Range<int>, b: RangeFrom<int>) {}",
                );

                new_frontend_cmd(&main).unwrap();
            }

            #[test]
            fn core_primitive_extensions_are_forwarded_by_core_root() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "fn main() { let a: int = (-1).abs(); let b: float = 4.0.sqrt(); let c: int = \"abc\".len(); }",
                );

                new_frontend_cmd(&main).unwrap();
            }

            #[test]
            fn core_helper_externs_are_not_visible_to_user_modules() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "fn main() { int_abs(1); }");
                let error = new_frontend_cmd(&main).unwrap_err();

                assert!(error.contains("int_abs"));
            }

            #[test]
            fn core_primitive_wrapper_modules_are_not_preluded() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "fn main() { core_int.abs(1); }");
                let error = new_frontend_cmd(&main).unwrap_err();

                assert!(error.contains("core_int"));
            }

            #[test]
            fn std_import_resolves_implicit_package() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "import std:mem; fn main() { mem.collect_cycles(); }",
                );

                new_frontend_cmd(&main).unwrap();
            }

            #[test]
            fn std_selective_import_resolves_implicit_package() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "import std:mem { collect_cycles }; fn main() { collect_cycles(); }",
                );

                new_frontend_cmd(&main).unwrap();
            }

            #[test]
            fn std_declarations_are_not_preluded() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "fn main() { collect_cycles(); }");
                let error = new_frontend_cmd(&main).unwrap_err();

                assert!(error.contains("collect_cycles"));
            }

            #[test]
            fn old_std_dot_import_is_local_source_syntax() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "import std.mem; fn main() {}");
                let error = new_frontend_cmd(&main).unwrap_err();

                assert!(error.contains("std/mem.anv") || error.contains("std\\mem.anv"));
            }

            #[test]
            fn local_std_path_does_not_affect_std_colon_import() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "main.anv",
                    "import std.mem { local }; import std:mem { collect_cycles }; fn main() { local(); collect_cycles(); }",
                );
                write(&temp, "std/mem.anv", "pub fn local() {}");

                new_frontend_cmd(&main).unwrap();
            }

            #[test]
            fn script_relative_imports_work() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(
                    &temp,
                    "src/ui/main.anv",
                    "import helper { value as a }; import .helper { value as b }; import ..common { value as c }; fn main() { let x: int = a() + b() + c(); }",
                );
                write(&temp, "src/ui/helper.anv", "pub fn value() -> int { 1 }");
                write(&temp, "src/common.anv", "pub fn value() -> int { 1 }");

                new_frontend_cmd(&main).unwrap();
            }

            #[test]
            fn pkg_std_and_relative_imports() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[("math", "../math")]);
                write_manifest(&temp, "math", "src/lib.anv", &[]);
                let main = write(
                    &temp,
                    "game/src/main.anv",
                    "import helper { local }; import .helper { local as local2 }; import ..outside { escaped }; import pkg:math { add }; import std:mem; fn main() { let x: int = local() + local2() + escaped() + add(); }",
                );
                write(&temp, "game/src/helper.anv", "pub fn local() -> int { 1 }");
                write(&temp, "game/outside.anv", "pub fn escaped() -> int { 1 }");
                write(&temp, "math/src/lib.anv", "pub fn add() -> int { 1 }");

                check_manifest_file(&temp, &main).unwrap();
            }

            #[test]
            fn package_root_reexport_forwards_extend() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[("helpers", "../helpers")]);
                write_manifest(&temp, "helpers", "src/lib.anv", &[]);
                let main = write(
                    &temp,
                    "game/src/main.anv",
                    "import pkg:helpers; fn main() { let x: string = \"hi\".shout(); }",
                );
                write(&temp, "helpers/src/lib.anv", "pub import strings;");
                write(
                    &temp,
                    "helpers/src/strings.anv",
                    "pub extend string { fn shout(self) -> string { self } }",
                );

                check_manifest_file(&temp, &main).unwrap();
            }

            #[test]
            fn outside_override_rejected() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[]);
                write(&temp, "game/src/main.anv", "fn main() {}");
                let outside = write(&temp, "game/outside.anv", "fn main() {}");
                let error = check_manifest_file(&temp, &outside).unwrap_err();

                assert!(error.contains("outside every loaded package source root"));
            }

            #[test]
            fn old_dep_root_fails() {
                let temp = tempfile::tempdir().unwrap();
                write_manifest(&temp, "game", "src/main.anv", &[]);
                let main = write(&temp, "game/src/main.anv", "import dep:math; fn main() {}");
                let error = check_manifest_file(&temp, &main).unwrap_err();

                assert!(
                    error.contains("Unexpected token ':'") || error.contains("import declaration")
                );
            }
        }

        mod unsupported {
            use super::*;

            #[test]
            fn feature_flags() {
                let error = unsupported_error(None, &[], &["demo".to_string()], &[]);

                assert_eq!(error, "--new-frontend does not support --feature yet");
            }

            #[test]
            fn cfg_flags() {
                let error = unsupported_error(None, &[], &[], &["os=wasm".to_string()]);

                assert_eq!(error, "--new-frontend does not support --cfg yet");
            }

            #[test]
            fn lint_flags() {
                let error =
                    unsupported_error(None, &["internal_access=allow".to_string()], &[], &[]);

                assert_eq!(error, "--new-frontend does not support --lint yet");
            }

            #[test]
            fn manifest_lint() {
                let mut manifest = plain_manifest();
                manifest.lint.internal_access = LintLevel::Error;

                let error = unsupported_error(Some(&manifest), &[], &[], &[]);

                assert_eq!(
                    error,
                    "--new-frontend does not support lint configuration yet"
                );
            }

            #[test]
            fn manifest_externs() {
                let mut manifest = plain_manifest();
                manifest.externs.insert(
                    "engine".to_string(),
                    ExternEntry {
                        path: "externs/engine".to_string(),
                    },
                );

                let error = unsupported_error(Some(&manifest), &[], &[], &[]);

                assert_eq!(
                    error,
                    "--new-frontend does not support extern providers yet"
                );
            }

            #[test]
            fn accepts_plain_manifest() {
                let manifest = plain_manifest();

                reject_new_frontend_inputs(Some(&manifest), &[], &[], &[]).unwrap();
            }

            #[test]
            fn accepts_manifest_dependencies() {
                let mut manifest = plain_manifest();
                manifest.dependencies.insert(
                    "math".to_string(),
                    DependencyEntry {
                        path: "../math".to_string(),
                    },
                );

                reject_new_frontend_inputs(Some(&manifest), &[], &[], &[]).unwrap();
            }
        }
    }
}
