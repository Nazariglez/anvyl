use std::collections::HashSet;

use super::support::{
    InMemoryLoader, ignored_roots, module_path, preloaded, resolve, resolve_errors,
    resolve_with_external, resolve_with_ignored, resolve_with_preloaded,
    resolve_with_preloaded_and_external,
};
use crate::resolve::{ModuleId, ModulePath, PackageModulePath, ResolveError, ResolveResult};

fn group_keys(result: &ResolveResult) -> Vec<Vec<ModuleId>> {
    result
        .module_groups
        .iter()
        .map(|group| group.iter().map(|m| m.key.clone()).collect())
        .collect()
}

fn is_named(key: &ModuleId, path: &[&str]) -> bool {
    matches!(key.path(), PackageModulePath::Named(p) if p.segments() == path)
}

fn is_root(key: &ModuleId) -> bool {
    matches!(key.path(), PackageModulePath::Root)
}

fn has_key(result: &ResolveResult, path: &[&str]) -> bool {
    group_keys(result)
        .into_iter()
        .flatten()
        .any(|key| is_named(&key, path))
}

fn has_error(errors: &[ResolveError], path: &[&str]) -> bool {
    errors.iter().any(|e| match e {
        ResolveError::ModuleNotFound { module, .. }
        | ResolveError::LoadFailed { module, .. }
        | ResolveError::DuplicatePreloadedModule { module } => {
            module.named_path().is_some_and(|p| p.segments() == path)
        }
        ResolveError::UnknownDependency { .. }
        | ResolveError::PackageImportUnavailable { .. }
        | ResolveError::UnsupportedImportRoot { .. }
        | ResolveError::NativeProviderUnavailable { .. }
        | ResolveError::UnknownNativeProviderModule { .. }
        | ResolveError::UnknownNativeDepProviderModule { .. }
        | ResolveError::NativeOnlyPkgRootImport { .. }
        | ResolveError::SourceImportNotFound { .. } => false,
    })
}

fn assert_module_path(src: &str, dep_src: &str, path: &[&str]) {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(path.to_vec()), dep_src);
    let result = resolve(src, &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2);
    assert!(is_named(&keys[0][0], path));
}

#[test]
fn module_path_rejects_empty_path() {
    let error = ModulePath::new(vec![]).unwrap_err();
    assert_eq!(error.message(), "module path must not be empty");
}

#[test]
fn module_path_rejects_empty_segment() {
    let error = ModulePath::new(vec!["foo".into(), String::new(), "bar".into()]).unwrap_err();
    assert_eq!(
        error.message(),
        "module path must not contain empty segments: foo..bar"
    );
}

#[test]
fn single_import() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["foo"]), "");
    let result = resolve("import foo;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "should have 2 groups: foo, root");
    assert!(is_named(&keys[0][0], &["foo"]));
    assert!(is_root(&keys[1][0]));
}

#[test]
fn chain() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["foo"]), "import bar;");
    loader.add_source(module_path(["bar"]), "");
    let result = resolve("import foo;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 3, "bar, foo, root");
    assert!(is_named(&keys[0][0], &["bar"]));
    assert!(is_named(&keys[1][0], &["foo"]));
    assert!(is_root(&keys[2][0]));
}

#[test]
fn shared_dep() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["a"]), "import c;");
    loader.add_source(module_path(["b"]), "import c;");
    loader.add_source(module_path(["c"]), "");
    let result = resolve("import a; import b;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 4, "c, a, b, root");
    let c_count = keys.iter().filter(|g| is_named(&g[0], &["c"])).count();
    assert_eq!(c_count, 1, "c appears once despite two imports");
    assert!(is_root(&keys[3][0]));
}

#[test]
fn two_module_cycle() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["a"]), "import b;");
    loader.add_source(module_path(["b"]), "import a;");
    let result = resolve("import a;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "SCC of a,b, then root");
    assert_eq!(keys[0].len(), 2, "a and b in same cycle");
    assert!(is_root(&keys[1][0]));
}

#[test]
fn self_cycle() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["a"]), "import a;");
    let result = resolve("import a;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "a (SCC), then root");
    assert_eq!(keys[0].len(), 1);
    assert!(is_named(&keys[0][0], &["a"]));
    assert!(is_root(&keys[1][0]));
}

#[test]
fn ignored_root() {
    let mut loader = InMemoryLoader::default();
    let ignored = ignored_roots(&["godot"]);
    let result = resolve_with_ignored("import godot.math;", &mut loader, &ignored).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 1, "only root");
    assert!(is_root(&keys[0][0]));
}

#[test]
fn mixed_real_and_ignored() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["foo"]), "");
    let ignored = ignored_roots(&["godot"]);
    let result =
        resolve_with_ignored("import foo; import godot.math;", &mut loader, &ignored).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "foo, root");
}

#[test]
fn missing_module() {
    let mut loader = InMemoryLoader::default();
    loader.add_missing(module_path(["missing"]));
    let errors = resolve_errors(resolve("import missing;", &mut loader));
    assert!(has_error(&errors, &["missing"]));
}

#[test]
fn load_failure() {
    let mut loader = InMemoryLoader::default();
    loader.add_failure(module_path(["bad"]), "disk error");
    let errors = resolve_errors(resolve("import bad;", &mut loader));
    assert!(has_error(&errors, &["bad"]));
}

#[test]
fn external_module_does_not_make_local_import_resolve() {
    let mut loader = InMemoryLoader::default();
    let path = module_path(["host"]);
    loader.add_missing(path.clone());
    let external_modules = HashSet::from([path.clone()]);

    let errors = resolve_errors(resolve_with_external(
        "import host;",
        &mut loader,
        &external_modules,
    ));

    assert!(has_error(&errors, &["host"]));
    assert_eq!(loader.load_count(&path), 1);
}

#[test]
fn ext_provider_module_resolves_without_source_load() {
    let mut loader = InMemoryLoader::default();
    let path = module_path(["host"]);
    let external_modules = HashSet::from([path.clone()]);

    let result = resolve_with_external("import ext:host;", &mut loader, &external_modules).unwrap();

    assert!(!has_key(&result, &["host"]));
    assert_eq!(loader.load_count(&path), 0);
}

#[test]
fn unknown_ext_provider_module_fails() {
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_with_external(
        "import ext:missing;",
        &mut loader,
        &HashSet::new(),
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::UnknownNativeProviderModule { module, .. }]
            if module.segments() == ["missing"]
    ));
}

#[test]
fn missing_non_external_module_still_fails() {
    let mut loader = InMemoryLoader::default();
    loader.add_missing(module_path(["missing"]));
    let errors = resolve_errors(resolve_with_external(
        "import missing;",
        &mut loader,
        &HashSet::new(),
    ));

    assert!(has_error(&errors, &["missing"]));
}

#[test]
fn preloaded_module_wins_over_missing_source() {
    let mut loader = InMemoryLoader::default();
    let path = module_path(["host"]);
    let external_modules = HashSet::from([path.clone()]);

    let result = resolve_with_preloaded_and_external(
        "import host;",
        vec![preloaded(&["host"], "pub fn f() {}")],
        &mut loader,
        &external_modules,
    )
    .unwrap();

    assert!(has_key(&result, &["host"]));
    assert_eq!(loader.load_count(&path), 0);
}

#[test]
fn load_failure_is_reported_with_external_module() {
    let mut loader = InMemoryLoader::default();
    let path = module_path(["host"]);
    loader.add_failure(path.clone(), "disk error");
    let external_modules = HashSet::from([path]);

    let errors = resolve_errors(resolve_with_external(
        "import host;",
        &mut loader,
        &external_modules,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::LoadFailed { module, message, .. }]
            if module.named_path().is_some_and(|path| path.segments() == ["host"])
                && message == "disk error"
    ));
}

#[test]
fn duplicate_imports() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["foo"]), "");
    let result = resolve("import foo; import foo;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "foo, root");
    assert_eq!(keys[0].len(), 1, "foo appears once");
}

#[test]
fn nested_path() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["foo", "bar", "baz"]), "");
    let result = resolve("import foo.bar.baz;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert!(is_named(&keys[0][0], &["foo", "bar", "baz"]));
}

#[test]
fn selective_import_is_module_path() {
    assert_module_path("import foo { bar, baz };", "", &["foo"]);
}

#[test]
fn wildcard_import_is_module_path() {
    assert_module_path("import foo { * };", "", &["foo"]);
}

#[test]
fn root_in_result() {
    let mut loader = InMemoryLoader::default();
    let result = resolve("", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 1);
    assert!(is_root(&keys[0][0]));
}

#[test]
fn root_with_decl() {
    let mut loader = InMemoryLoader::default();
    let result = resolve("fn main() {}", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 1);
    assert!(is_root(&keys[0][0]));
}

#[test]
fn module_as_alias() {
    assert_module_path("import foo as f;", "", &["foo"]);
}

#[test]
fn selective_self() {
    assert_module_path("import foo { self };", "", &["foo"]);
}

#[test]
fn selective_self_alias() {
    assert_module_path("import foo { self as f };", "", &["foo"]);
}

#[test]
fn selective_self_mixed() {
    assert_module_path("import foo { self, bar };", "pub fn bar() {}", &["foo"]);
}

#[test]
fn preloaded_module_resolves_without_import() {
    let mut loader = InMemoryLoader::default();
    let result =
        resolve_with_preloaded("", vec![preloaded(&["core_int"], "")], &mut loader).unwrap();

    assert!(has_key(&result, &["core_int"]));
}

#[test]
fn duplicate_preloaded_module_is_resolve_error() {
    let path = module_path(["core_int"]);
    let module = ModuleId::named(crate::resolve::PackageId::synthetic_root(), path.clone());
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_with_preloaded(
        "import core_int;",
        vec![preloaded(&["core_int"], ""), preloaded(&["core_int"], "")],
        &mut loader,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::DuplicatePreloadedModule { module: found }] if found == &module
    ));
    assert_eq!(loader.load_count(&path), 0);
}

#[test]
fn preloaded_module_imports_are_resolved() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(["dep"]), "");
    let result = resolve_with_preloaded(
        "",
        vec![preloaded(&["core_int"], "import dep;")],
        &mut loader,
    )
    .unwrap();

    assert!(has_key(&result, &["core_int"]));
    assert!(has_key(&result, &["dep"]));
}

#[test]
fn root_import_of_preloaded_module_dedupes() {
    let path = module_path(["core_int"]);
    let mut loader = InMemoryLoader::default();
    loader.add_source(path.clone(), "fn should_not_load() {}");
    let result = resolve_with_preloaded(
        "import core_int;",
        vec![preloaded(&["core_int"], "")],
        &mut loader,
    )
    .unwrap();

    assert!(has_key(&result, &["core_int"]));
    assert_eq!(loader.load_count(&path), 0);
}

#[test]
fn duplicate_preloaded_and_loaded_imports_appear_once() {
    let mut loader = InMemoryLoader::default();
    let result = resolve_with_preloaded(
        "import core_int; import core_int;",
        vec![preloaded(&["core_int"], "")],
        &mut loader,
    )
    .unwrap();
    let count = group_keys(&result)
        .into_iter()
        .flatten()
        .filter(|key| is_named(key, &["core_int"]))
        .count();

    assert_eq!(count, 1);
}

#[test]
fn preloaded_cycle_is_one_scc() {
    let mut loader = InMemoryLoader::default();
    let result = resolve_with_preloaded(
        "",
        vec![
            preloaded(&["a"], "import b;"),
            preloaded(&["b"], "import a;"),
        ],
        &mut loader,
    )
    .unwrap();
    let keys = group_keys(&result);

    assert!(keys.iter().any(|group| {
        group.len() == 2
            && group.iter().any(|key| is_named(key, &["a"]))
            && group.iter().any(|key| is_named(key, &["b"]))
    }));
}
