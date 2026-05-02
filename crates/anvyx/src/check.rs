use std::{collections::HashMap, fs, path::Path};

use anvyx_lang::{CompilationContext, LintConfig, LintLevel};
use anvyx_lang2::{PackageId as FrontendPackageId, SourceBundle};

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
        let input = anvyx_lang2::CheckFileInput::new(file.to_path_buf(), sources)
            .map_err(|error| error.to_string())?;
        anvyx_lang2::check_file(input).map_err(|error| error.to_string())?;
    }
    Ok(())
}

fn package_check_input(
    graph: &PackageGraph,
    file: &Path,
    sources: SourceBundle,
) -> Result<anvyx_lang2::CheckPackageInput, String> {
    let root = graph.root();
    reject_outside_source_root(file, &root.source_root)?;
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
            anvyx_lang2::PackageSource::new(
                frontend_package_id(&package.id),
                package.entry.clone(),
                package.source_root.clone(),
                dependencies,
            )
            .map_err(|error| error.to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;

    anvyx_lang2::CheckPackageInput::new(root_id, file.to_path_buf(), packages, sources)
        .map_err(|error| error.to_string())
}

fn reject_outside_source_root(file: &Path, source_root: &Path) -> Result<(), String> {
    let file = file
        .canonicalize()
        .map_err(|error| format!("failed to canonicalize '{}': {error}", file.display()))?;
    let source_root = source_root.canonicalize().map_err(|error| {
        format!(
            "failed to canonicalize source root '{}': {error}",
            source_root.display()
        )
    })?;
    if file.starts_with(&source_root) {
        Ok(())
    } else {
        Err(format!(
            "--new-frontend file override '{}' is outside package source root '{}'",
            file.display(),
            source_root.display()
        ))
    }
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
                entry: "main.anv".to_string(),
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

    mod new_frontend {
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
                .std_modules()
                .map(|module| module.path().to_vec())
                .collect::<Vec<_>>();
            paths.sort();
            paths
        }

        mod bundle {
            use anvyx_lang2::{ModuleSource, SourceText};

            use super::*;

            fn bundle_from_sources(
                core_prelude: String,
                core_sources: HashMap<String, StdModuleSource>,
                std_sources: HashMap<String, StdModuleSource>,
            ) -> Result<SourceBundle, String> {
                let prelude =
                    SourceText::new(core_prelude, "<core>").map_err(|error| error.to_string())?;

                let mut always_active_modules = vec![];
                let core_modules = sorted_modules(core_sources)
                    .into_iter()
                    .map(|(name, source)| {
                        let path = vec![name.clone()];
                        let label = format!("<core.{name}>");
                        always_active_modules.push(path.clone());
                        ModuleSource::new(path, source.anv_source, label)
                            .map_err(|error| error.to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let std_modules = sorted_modules(std_sources)
                    .into_iter()
                    .map(|(name, source)| {
                        let path = vec!["std".to_string(), name.clone()];
                        let label = format!("<std.{name}>");
                        ModuleSource::new(path, source.anv_source, label)
                            .map_err(|error| error.to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                SourceBundle::new(
                    Some(prelude),
                    core_modules,
                    std_modules,
                    always_active_modules,
                )
                .map_err(|error| error.to_string())
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
                let prelude = bundle.core_prelude().unwrap();

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
                let modules = bundle.core_modules();

                assert_eq!(modules.len(), 1);
                assert_eq!(modules[0].path(), path(&["core_int"]));
                assert_eq!(modules[0].label(), "<core.core_int>");
                assert_eq!(modules[0].code(), "extend int {}");
                assert_eq!(bundle.always_active_modules(), &[path(&["core_int"])]);
            }

            #[test]
            fn std_root() {
                let bundle = bundle_from_sources(
                    String::new(),
                    HashMap::new(),
                    sources(&[("math", "extern fn sin(x: float) -> float;")]),
                )
                .unwrap();
                let module = bundle.std_module(&path(&["std", "math"])).unwrap();

                assert_eq!(module.path(), path(&["std", "math"]));
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
                    .core_modules()
                    .iter()
                    .map(|module| module.path().to_vec())
                    .collect::<Vec<_>>();

                assert_eq!(core_paths, [path(&["core_int"]), path(&["core_string"])]);
                assert_eq!(bundle.always_active_modules(), core_paths);
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
                let core_paths = bundle
                    .core_modules()
                    .iter()
                    .map(|module| module.path().to_vec())
                    .collect::<Vec<_>>();
                let expected = [
                    path(&["core_float"]),
                    path(&["core_int"]),
                    path(&["core_string"]),
                ];

                assert_eq!(core_paths, expected);
                assert_eq!(bundle.always_active_modules(), expected);
                let prelude = bundle.core_prelude().unwrap().code();
                assert!(prelude.contains("enum Option<T>"));
                assert!(prelude.contains("struct Range<T>"));
            }

            #[test]
            fn clean_frontend_std_surface_is_explicit() {
                let bundle = new_frontend_source_bundle().unwrap();

                assert_eq!(sorted_std_paths(&bundle), [path(&["std", "mem"])]);
            }

            #[test]
            fn clean_frontend_bundle_has_no_legacy_type_spellings() {
                let bundle = new_frontend_source_bundle().unwrap();
                let mut code = String::new();
                code.push_str(bundle.core_prelude().unwrap().code());
                for module in bundle.core_modules() {
                    code.push_str(module.code());
                }
                for module in bundle.std_modules() {
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
            }
        }

        mod integration {
            use super::*;

            #[test]
            fn std_import_resolves_implicit_package() {
                let temp = tempfile::tempdir().unwrap();
                let main = write(&temp, "main.anv", "import std.mem; fn main() {}");

                new_frontend_cmd(&main).unwrap();
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
