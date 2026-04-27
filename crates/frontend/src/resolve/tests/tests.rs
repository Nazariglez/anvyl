use super::support::{InMemoryLoader, ignored_roots, module_path, resolve, resolve_with_ignored};
use crate::resolve::{ModuleKey, ResolveError, ResolveResult};

fn group_keys(result: &ResolveResult) -> Vec<Vec<ModuleKey>> {
    result
        .module_groups
        .iter()
        .map(|group| group.iter().map(|m| m.key.clone()).collect())
        .collect()
}

fn has_error(errors: &[ResolveError], path: &[&str]) -> bool {
    errors.iter().any(|e| match e {
        ResolveError::ModuleNotFound { path: p, .. } => p.segments() == path,
        ResolveError::LoadFailed { path: p, .. } => p.segments() == path,
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
    let errors = resolve("import missing;", &mut loader).unwrap_err();
    assert!(has_error(&errors, &["missing"]));
}

#[test]
fn load_failure() {
    let mut loader = InMemoryLoader::default();
    loader.add_failure(module_path(vec!["bad"]), "disk error");
    let errors = resolve("import bad;", &mut loader).unwrap_err();
    assert!(has_error(&errors, &["bad"]));
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
