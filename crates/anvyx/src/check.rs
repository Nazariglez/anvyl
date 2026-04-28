use std::{collections::HashMap, fs, path::Path};

use anvyx_lang::{CompilationContext, LintConfig, LintLevel, StdModuleSource};
use anvyx_lang2::{ModuleSource, SourceBundle, SourceText};

use crate::{
    manifest::Manifest,
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
    let input = anvyx_lang2::CheckFileInput::new(file.to_path_buf(), sources)
        .map_err(|error| error.to_string())?;
    anvyx_lang2::check_file(input).map_err(|error| error.to_string())?;
    Ok(())
}

pub fn reject_unsupported_new_frontend_inputs(
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
    let (std_sources, _) = collect_std();
    let (core_prelude, core_sources, _) = collect_core();

    source_bundle_from_collected_sources(core_prelude, core_sources, std_sources)
}

fn source_bundle_from_collected_sources(
    core_prelude: String,
    core_sources: HashMap<String, StdModuleSource>,
    std_sources: HashMap<String, StdModuleSource>,
) -> Result<SourceBundle, String> {
    let prelude = SourceText::new(core_prelude, "<core>").map_err(|error| error.to_string())?;

    let mut always_active_modules = vec![];
    let core_modules = sorted_modules(core_sources)
        .into_iter()
        .map(|(name, source)| {
            let path = vec![name.clone()];
            let label = format!("<core.{name}>");
            always_active_modules.push(path.clone());
            ModuleSource::new(path, source.anv_source, label).map_err(|error| error.to_string())
        })
        .collect::<Result<Vec<_>, _>>()?;

    let std_modules = sorted_modules(std_sources)
        .into_iter()
        .map(|(name, source)| {
            let path = vec!["std".to_string(), name.clone()];
            let label = format!("<std.{name}>");
            ModuleSource::new(path, source.anv_source, label).map_err(|error| error.to_string())
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

fn sorted_modules(sources: HashMap<String, StdModuleSource>) -> Vec<(String, StdModuleSource)> {
    let mut sources = sources.into_iter().collect::<Vec<_>>();
    sources.sort_by(|left, right| left.0.cmp(&right.0));
    sources
}

#[cfg(test)]
mod tests {
    use anvyx_lang::LintConfig;

    use super::*;
    use crate::manifest::{ExternEntry, Project};

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
        reject_unsupported_new_frontend_inputs(manifest, lint_overrides, features, cfgs)
            .expect_err("input should be unsupported")
    }

    #[test]
    fn new_frontend_bundle_maps_core_prelude() {
        let bundle = source_bundle_from_collected_sources(
            "fn p() {}".to_string(),
            HashMap::new(),
            HashMap::new(),
        )
        .unwrap();
        let prelude = bundle.core_prelude().unwrap();

        assert_eq!(prelude.label(), "<core>");
        assert_eq!(prelude.code(), "fn p() {}");
    }

    #[test]
    fn new_frontend_bundle_maps_core_modules() {
        let bundle = source_bundle_from_collected_sources(
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
    fn new_frontend_bundle_maps_std_modules_under_std_root() {
        let bundle = source_bundle_from_collected_sources(
            String::new(),
            HashMap::new(),
            sources(&[("math", "extern fn sin(x: double) -> double;")]),
        )
        .unwrap();
        let module = bundle.std_module(&path(&["std", "math"])).unwrap();

        assert_eq!(module.path(), path(&["std", "math"]));
        assert_eq!(module.label(), "<std.math>");
        assert_eq!(module.code(), "extern fn sin(x: double) -> double;");
    }

    #[test]
    fn new_frontend_bundle_uses_deterministic_module_order() {
        let bundle = source_bundle_from_collected_sources(
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
    fn new_frontend_bundle_rejects_invalid_builtin_module_name() {
        let error = source_bundle_from_collected_sources(
            String::new(),
            sources(&[("", "extend int {}")]),
            HashMap::new(),
        )
        .expect_err("empty built-in module name should be invalid");

        assert!(error.contains("module path must not contain empty segments"));
    }

    #[test]
    fn new_frontend_rejects_feature_flags() {
        let error = unsupported_error(None, &[], &["demo".to_string()], &[]);

        assert_eq!(error, "--new-frontend does not support --feature yet");
    }

    #[test]
    fn new_frontend_rejects_cfg_flags() {
        let error = unsupported_error(None, &[], &[], &["os=wasm".to_string()]);

        assert_eq!(error, "--new-frontend does not support --cfg yet");
    }

    #[test]
    fn new_frontend_rejects_lint_flags() {
        let error = unsupported_error(None, &["internal_access=allow".to_string()], &[], &[]);

        assert_eq!(error, "--new-frontend does not support --lint yet");
    }

    #[test]
    fn new_frontend_rejects_manifest_lint_config() {
        let mut manifest = plain_manifest();
        manifest.lint.internal_access = LintLevel::Error;

        let error = unsupported_error(Some(&manifest), &[], &[], &[]);

        assert_eq!(
            error,
            "--new-frontend does not support lint configuration yet"
        );
    }

    #[test]
    fn new_frontend_rejects_manifest_externs() {
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
    fn new_frontend_accepts_plain_manifest_entry() {
        let manifest = plain_manifest();

        reject_unsupported_new_frontend_inputs(Some(&manifest), &[], &[], &[]).unwrap();
    }
}
