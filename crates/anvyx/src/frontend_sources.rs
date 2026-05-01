use anvyx_lang::StdModule;
use anvyx_lang2::{ModuleSource, SourceBundle, SourceText};

// FIXME: remove this transitional adapter as soon as core/std expose clean-frontend-native sources or descriptors.
// The clean frontend should not filter or canonicalize old `StdModule::full_anv_source()` strings.
const CLEAN_CORE_PRELUDE_MODULES: &[&str] = &["core_option", "core_range"];
const CLEAN_CORE_MODULES: &[&str] = &["core_float", "core_int", "core_string"];
const CLEAN_STD_MODULES: &[&str] = &["mem"];

pub(crate) fn source_bundle() -> Result<SourceBundle, String> {
    let (prelude, core_modules) = core_sources()?;

    SourceBundle::new(
        Some(prelude),
        core_modules,
        std_sources()?,
        active_core_paths(),
    )
    .map_err(|error| error.to_string())
}

fn core_sources() -> Result<(SourceText, Vec<ModuleSource>), String> {
    let core = anvyx_core::core_modules();
    let mut prelude = String::new();
    for name in CLEAN_CORE_PRELUDE_MODULES {
        prelude.push_str(&module_named(&core, name)?.full_clean_anv_source());
        prelude.push('\n');
    }

    let modules = CLEAN_CORE_MODULES
        .iter()
        .map(|name| core_module_source(module_named(&core, name)?))
        .collect::<Result<Vec<_>, _>>()?;

    Ok((text(prelude, "<core>")?, modules))
}

fn std_sources() -> Result<Vec<ModuleSource>, String> {
    let std = anvyx_std::std_modules();
    CLEAN_STD_MODULES
        .iter()
        .map(|name| std_module_source(module_named(&std, name)?))
        .collect()
}

fn active_core_paths() -> Vec<Vec<String>> {
    CLEAN_CORE_MODULES
        .iter()
        .map(|name| vec![(*name).to_string()])
        .collect()
}

fn core_module_source(module: &StdModule) -> Result<ModuleSource, String> {
    module_source(
        vec![module.name.to_string()],
        module.full_clean_anv_source(),
        format!("<core.{}>", module.name),
    )
}

fn std_module_source(module: &StdModule) -> Result<ModuleSource, String> {
    module_source(
        vec!["std".to_string(), module.name.to_string()],
        module.full_clean_anv_source(),
        format!("<std.{}>", module.name),
    )
}

fn module_named<'a>(modules: &'a [StdModule], name: &str) -> Result<&'a StdModule, String> {
    let mut matches = modules.iter().filter(|module| module.name == name);
    let Some(module) = matches.next() else {
        return Err(format!("clean frontend source module '{name}' is missing"));
    };
    if matches.next().is_some() {
        return Err(format!(
            "clean frontend source module '{name}' is duplicated"
        ));
    }
    Ok(module)
}

fn text(code: String, label: &str) -> Result<SourceText, String> {
    SourceText::new(code, label).map_err(|error| error.to_string())
}

fn module_source(path: Vec<String>, code: String, label: String) -> Result<ModuleSource, String> {
    ModuleSource::new(path, code, label).map_err(|error| error.to_string())
}
