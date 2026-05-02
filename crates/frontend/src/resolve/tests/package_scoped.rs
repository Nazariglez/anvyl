use std::collections::HashMap;

use super::support::{
    InMemoryLoader, module_id, module_path, package, package_input, package_root, resolve_errors,
    resolve_package, root_id,
};
use crate::resolve::{PackageId, PackageInput, PackageModulePath, ResolveError, ResolveResult};

fn packages<const N: usize>(
    entries: [(PackageId, PackageInput); N],
) -> HashMap<PackageId, PackageInput> {
    HashMap::from(entries)
}

fn has_module(result: &ResolveResult, package: &PackageId, path: &[&str]) -> bool {
    result.module_groups.iter().flatten().any(|module| {
        module.key.package() == package
            && matches!(module.key.path(), PackageModulePath::Named(found) if found.segments() == path)
    })
}

fn has_root(result: &ResolveResult, package: &PackageId) -> bool {
    result.module_groups.iter().flatten().any(|module| {
        module.key.package() == package && matches!(module.key.path(), PackageModulePath::Root)
    })
}

#[test]
fn bare_import_is_local_even_when_dependency_alias_matches() {
    let game = package("game");
    let math = package("math");
    let mut loader = InMemoryLoader::default();
    loader.add_package_source(&game, module_path(["math"]), "");
    let result = resolve_package(
        game.clone(),
        "import math;",
        packages([
            (game.clone(), package_input(&[("math", math.clone())])),
            (math.clone(), package_root("", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_module(&result, &game, &["math"]));
    assert!(!has_root(&result, &math));
}

#[test]
fn dependency_import_uses_direct_alias() {
    let game = package("game");
    let math = package("math");
    let mut loader = InMemoryLoader::default();
    let result = resolve_package(
        game.clone(),
        "import dep:math;",
        packages([
            (game, package_input(&[("math", math.clone())])),
            (math.clone(), package_root("", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_root(&result, &math));
}

#[test]
fn unknown_dependency_alias_is_rejected() {
    let game = package("game");
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_package(
        game.clone(),
        "import dep:math;",
        packages([(game.clone(), PackageInput::default())]),
        &mut loader,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::UnknownDependency { package, alias, .. }]
            if package == &game && alias == "math"
    ));
}

#[test]
fn same_module_path_in_different_packages_does_not_collide() {
    let game = package("game");
    let audio = package("audio");
    let render = package("render");
    let mut loader = InMemoryLoader::default();
    loader.add_package_source(&audio, module_path(["math"]), "");
    loader.add_package_source(&render, module_path(["math"]), "");
    let result = resolve_package(
        game.clone(),
        "import dep:audio.math; import dep:render.math;",
        packages([
            (
                game,
                package_input(&[("audio", audio.clone()), ("render", render.clone())]),
            ),
            (audio.clone(), package_root("pub import math;", &[])),
            (render.clone(), package_root("pub import math;", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_module(&result, &audio, &["math"]));
    assert!(has_module(&result, &render, &["math"]));
}

#[test]
fn duplicate_local_imports_load_once_per_package() {
    let game = package("game");
    let math = package("math");
    let mut loader = InMemoryLoader::default();
    loader.add_package_source(&game, module_path(["util"]), "");
    loader.add_package_source(&math, module_path(["util"]), "");
    let result = resolve_package(
        game.clone(),
        "import util; import util; import dep:math.util; import dep:math.util;",
        packages([
            (game.clone(), package_input(&[("math", math.clone())])),
            (math.clone(), package_root("pub import util;", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_module(&result, &game, &["util"]));
    assert!(has_module(&result, &math, &["util"]));
    assert_eq!(loader.load_count_module(&module_id(&game, &["util"])), 1);
    assert_eq!(loader.load_count_module(&module_id(&math, &["util"])), 1);
}

#[test]
fn no_manifest_synthetic_package_cannot_use_dep() {
    let game = PackageId::synthetic_root();
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_package(
        game.clone(),
        "import dep:math;",
        packages([(game.clone(), PackageInput::default())]),
        &mut loader,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::UnknownDependency { package, alias, .. }]
            if package == &game && alias == "math"
    ));
}

#[test]
fn local_import_chain_stays_package_local() {
    let game = package("game");
    let math = package("math");
    let mut loader = InMemoryLoader::default();
    loader.add_package_source(&game, module_path(["a"]), "import b;");
    loader.add_package_source(&game, module_path(["b"]), "");
    loader.add_package_source(&math, module_path(["b"]), "");
    let result = resolve_package(
        game.clone(),
        "import a;",
        packages([
            (game.clone(), package_input(&[("b", math.clone())])),
            (math.clone(), package_root("", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_module(&result, &game, &["a"]));
    assert!(has_module(&result, &game, &["b"]));
    assert!(!has_module(&result, &math, &["b"]));
}

#[test]
fn missing_local_module_reports_current_package() {
    let game = package("game");
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_package(
        game.clone(),
        "import missing;",
        packages([(game.clone(), PackageInput::default())]),
        &mut loader,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::ModuleNotFound { module, .. }]
            if module.package() == &game
                && module.named_path().is_some_and(|path| path.segments() == ["missing"])
    ));
}

#[test]
fn dependency_root_import_resolves_package_root_imports() {
    let game = package("game");
    let math = package("math");
    let mut loader = InMemoryLoader::default();
    loader.add_package_source(&math, module_path(["helpers"]), "");
    let result = resolve_package(
        game.clone(),
        "import dep:math;",
        packages([
            (game, package_input(&[("math", math.clone())])),
            (math.clone(), package_root("import helpers;", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_root(&result, &math));
    assert!(has_module(&result, &math, &["helpers"]));
}

#[test]
fn package_root_ids_can_be_asserted() {
    let game = package("game");

    assert_eq!(root_id(&game).package(), &game);
}
