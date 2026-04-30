use std::collections::HashSet;

use super::support::{
    InMemoryLoader, ignored_roots, module_path, preloaded, resolve, resolve_errors,
    resolve_with_external, resolve_with_ignored, resolve_with_preloaded,
    resolve_with_preloaded_and_external,
};
use crate::resolve::{ModuleKey, ModulePath, ResolveError, ResolveResult};

fn group_keys(result: &ResolveResult) -> Vec<Vec<ModuleKey>> {
    result
        .module_groups
        .iter()
        .map(|group| group.iter().map(|m| m.key.clone()).collect())
        .collect()
}

fn has_key(result: &ResolveResult, path: &[&str]) -> bool {
    group_keys(result)
        .into_iter()
        .flatten()
        .any(|key| matches!(key, ModuleKey::Named(p) if p.segments() == path))
}

fn has_error(errors: &[ResolveError], path: &[&str]) -> bool {
    errors.iter().any(|e| match e {
        ResolveError::ModuleNotFound { path: p, .. }
        | ResolveError::LoadFailed { path: p, .. }
        | ResolveError::DuplicatePreloadedModule { path: p } => p.segments() == path,
    })
}

fn assert_module_path(src: &str, dep_src: &str, path: &[&str]) {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(path.to_vec()), dep_src);
    let result = resolve(src, &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2);
    assert!(matches!(&keys[0][0], ModuleKey::Named(p) if p.segments() == path));
}

#[test]
fn module_path_rejects_empty_path() {
    let error = ModulePath::new(vec![]).unwrap_err();
    assert_eq!(error.message(), "module path must not be empty");
}

#[test]
fn module_path_rejects_empty_segment() {
    let error = ModulePath::new(vec!["foo".into(), "".into(), "bar".into()]).unwrap_err();
    assert_eq!(
        error.message(),
        "module path must not contain empty segments: foo..bar"
    );
}

#[test]
fn single_import() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["foo"]), "");
    let result = resolve("import foo;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "should have 2 groups: foo, root");
    assert!(matches!(&keys[0][0], ModuleKey::Named(p) if p.segments() == ["foo"]));
    assert!(matches!(&keys[1][0], ModuleKey::Root));
}

#[test]
fn chain() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["foo"]), "import bar;");
    loader.add_source(module_path(vec!["bar"]), "");
    let result = resolve("import foo;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 3, "bar, foo, root");
    assert!(matches!(&keys[0][0], ModuleKey::Named(p) if p.segments() == ["bar"]));
    assert!(matches!(&keys[1][0], ModuleKey::Named(p) if p.segments() == ["foo"]));
    assert!(matches!(&keys[2][0], ModuleKey::Root));
}

#[test]
fn shared_dep() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["a"]), "import c;");
    loader.add_source(module_path(vec!["b"]), "import c;");
    loader.add_source(module_path(vec!["c"]), "");
    let result = resolve("import a; import b;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 4, "c, a, b, root");
    let c_count = keys
        .iter()
        .filter(|g| matches!(&g[0], ModuleKey::Named(p) if p.segments() == ["c"]))
        .count();
    assert_eq!(c_count, 1, "c appears once despite two imports");
    assert!(matches!(&keys[3][0], ModuleKey::Root));
}

#[test]
fn two_module_cycle() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["a"]), "import b;");
    loader.add_source(module_path(vec!["b"]), "import a;");
    let result = resolve("import a;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "SCC of a,b, then root");
    assert_eq!(keys[0].len(), 2, "a and b in same cycle");
    assert!(matches!(&keys[1][0], ModuleKey::Root));
}

#[test]
fn self_cycle() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["a"]), "import a;");
    let result = resolve("import a;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "a (SCC), then root");
    assert_eq!(keys[0].len(), 1);
    assert!(matches!(&keys[0][0], ModuleKey::Named(p) if p.segments() == ["a"]));
    assert!(matches!(&keys[1][0], ModuleKey::Root));
}

#[test]
fn ignored_root() {
    let mut loader = InMemoryLoader::default();
    let ignored = ignored_roots(&["godot"]);
    let result = resolve_with_ignored("import godot.math;", &mut loader, &ignored).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 1, "only root");
    assert!(matches!(&keys[0][0], ModuleKey::Root));
}

#[test]
fn mixed_real_and_ignored() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["foo"]), "");
    let ignored = ignored_roots(&["godot"]);
    let result =
        resolve_with_ignored("import foo; import godot.math;", &mut loader, &ignored).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "foo, root");
}

#[test]
fn missing_module() {
    let mut loader = InMemoryLoader::default();
    loader.add_missing(module_path(vec!["missing"]));
    let errors = resolve_errors(resolve("import missing;", &mut loader));
    assert!(has_error(&errors, &["missing"]));
}

#[test]
fn load_failure() {
    let mut loader = InMemoryLoader::default();
    loader.add_failure(module_path(vec!["bad"]), "disk error");
    let errors = resolve_errors(resolve("import bad;", &mut loader));
    assert!(has_error(&errors, &["bad"]));
}

#[test]
fn external_module_import_resolves_without_source_module() {
    let mut loader = InMemoryLoader::default();
    let path = module_path(vec!["host"]);
    loader.add_missing(path.clone());
    let external_modules = HashSet::from([path.clone()]);

    let result = resolve_with_external("import host;", &mut loader, &external_modules).unwrap();

    assert!(!has_key(&result, &["host"]));
    assert_eq!(loader.load_count(&path), 1);
}

#[test]
fn missing_non_external_module_still_fails() {
    let mut loader = InMemoryLoader::default();
    loader.add_missing(module_path(vec!["missing"]));
    let errors = resolve_errors(resolve_with_external(
        "import missing;",
        &mut loader,
        &HashSet::new(),
    ));

    assert!(has_error(&errors, &["missing"]));
}

#[test]
fn preloaded_module_wins_over_external_fallback() {
    let mut loader = InMemoryLoader::default();
    let path = module_path(vec!["host"]);
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
fn load_failure_beats_external_fallback() {
    let mut loader = InMemoryLoader::default();
    let path = module_path(vec!["host"]);
    loader.add_failure(path.clone(), "disk error");
    let external_modules = HashSet::from([path]);

    let errors = resolve_errors(resolve_with_external(
        "import host;",
        &mut loader,
        &external_modules,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::LoadFailed { path, message, .. }]
            if path.segments() == ["host"] && message == "disk error"
    ));
}

#[test]
fn duplicate_imports() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["foo"]), "");
    let result = resolve("import foo; import foo;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 2, "foo, root");
    assert_eq!(keys[0].len(), 1, "foo appears once");
}

#[test]
fn nested_path() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["foo", "bar", "baz"]), "");
    let result = resolve("import foo.bar.baz;", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert!(matches!(&keys[0][0], ModuleKey::Named(p) if p.segments() == ["foo", "bar", "baz"]));
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
    assert!(matches!(&keys[0][0], ModuleKey::Root));
}

#[test]
fn root_with_decl() {
    let mut loader = InMemoryLoader::default();
    let result = resolve("fn main() {}", &mut loader).unwrap();
    let keys = group_keys(&result);
    assert_eq!(keys.len(), 1);
    assert!(matches!(&keys[0][0], ModuleKey::Root));
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
    let path = module_path(vec!["core_int"]);
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_with_preloaded(
        "import core_int;",
        vec![preloaded(&["core_int"], ""), preloaded(&["core_int"], "")],
        &mut loader,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::DuplicatePreloadedModule { path: p }] if p == &path
    ));
    assert_eq!(loader.load_count(&path), 0);
}

#[test]
fn preloaded_module_imports_are_resolved() {
    let mut loader = InMemoryLoader::default();
    loader.add_source(module_path(vec!["dep"]), "");
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
    let path = module_path(vec!["core_int"]);
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
        .filter(|key| matches!(key, ModuleKey::Named(p) if p.segments() == ["core_int"]))
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
            && group
                .iter()
                .any(|key| matches!(key, ModuleKey::Named(p) if p.segments() == ["a"]))
            && group
                .iter()
                .any(|key| matches!(key, ModuleKey::Named(p) if p.segments() == ["b"]))
    }));
}
