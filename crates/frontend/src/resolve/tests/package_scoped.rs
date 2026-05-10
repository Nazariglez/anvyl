use std::collections::{HashMap, HashSet};

use super::support::{
    InMemoryLoader, loaded_module, module_id, module_path, native_package_input, package,
    package_input, package_root, parse_program, provider_id, resolve_errors, resolve_package,
    root_id, source_id,
};
use crate::{
    resolve::{
        LoadedModule, ModuleId, PackageId, PackageInput, PackageModulePath, ResolveError,
        ResolveResult, SourceFileId,
    },
    span::Span,
};

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

fn has_source(result: &ResolveResult, module_id: &ModuleId) -> bool {
    result
        .module_groups
        .iter()
        .flatten()
        .any(|module| &module.key == module_id)
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
            (math.clone(), package_root(&math, "", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_module(&result, &game, &["math"]));
    assert!(!has_root(&result, &math));
}

#[test]
fn pkg_import_uses_direct_alias() {
    let game = package("game");
    let math = package("math");
    let mut loader = InMemoryLoader::default();
    let result = resolve_package(
        game.clone(),
        "import pkg:math;",
        packages([
            (game, package_input(&[("math", math.clone())])),
            (math.clone(), package_root(&math, "", &[])),
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
        "import pkg:math;",
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
        "import pkg:audio.math; import pkg:render.math;",
        packages([
            (
                game,
                package_input(&[("audio", audio.clone()), ("render", render.clone())]),
            ),
            (audio.clone(), package_root(&audio, "pub import math;", &[])),
            (
                render.clone(),
                package_root(&render, "pub import math;", &[]),
            ),
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
        "import util; import util; import pkg:math.util; import pkg:math.util;",
        packages([
            (game.clone(), package_input(&[("math", math.clone())])),
            (math.clone(), package_root(&math, "pub import util;", &[])),
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
fn duplicate_spans_keep_edges() {
    let game = package("game");
    let mut loader = InMemoryLoader::default();
    loader.add_package_source(&game, module_path(["a"]), "");
    loader.add_package_source(&game, module_path(["b"]), "");
    let mut program = parse_program("import a; import b;");
    for stmt in &mut program.stmts {
        if let crate::ast::Stmt::Import(import) = &mut stmt.node {
            import.span = Span::new(0, 0);
        }
    }
    let result = crate::resolve::resolve_package_modules(
        LoadedModule {
            module: root_id(&game),
            source: loaded_module(root_id(&game), "import a; import b;").source,
            program,
        },
        &packages([(game.clone(), PackageInput::default())]),
        vec![],
        &mut loader,
        &HashSet::new(),
        &HashSet::new(),
        crate::resolve::SystemPackages::default(),
    )
    .unwrap();
    let edges = result.import_edges.get(&root_id(&game)).unwrap();

    assert_eq!(edges.len(), 2);
    assert_eq!(edges[0].base, module_id(&game, &["a"]));
    assert_eq!(edges[1].base, module_id(&game, &["b"]));
}

#[test]
fn pkg_needs_context() {
    let game = PackageId::synthetic_root();
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_package(
        game.clone(),
        "import pkg:math;",
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
fn source_without_package_rejects_pkg() {
    let file = SourceFileId::new("/tmp/no-package/main.anv").unwrap();
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(crate::resolve::resolve_package_modules(
        loaded_module(
            ModuleId::source_without_package(file.clone()),
            "import pkg:math;",
        ),
        &HashMap::new(),
        vec![],
        &mut loader,
        &HashSet::new(),
        &HashSet::new(),
        crate::resolve::SystemPackages::default(),
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::PackageImportUnavailable { file: found, alias, .. }]
            if found == &file && alias == "math"
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
            (math.clone(), package_root(&math, "", &[])),
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
fn native_only_dependency_provider_import_resolves() {
    let game = package("game");
    let host = package("host");
    let mut loader = InMemoryLoader::default();
    let provider_modules = HashSet::from([provider_id(&host, &["audio"])]);
    let result = crate::resolve::resolve_package_modules(
        loaded_module(root_id(&game), "import pkg:host.audio { play };"),
        &packages([
            (game.clone(), package_input(&[("host", host.clone())])),
            (host.clone(), native_package_input(&[])),
        ]),
        vec![],
        &mut loader,
        &HashSet::new(),
        &provider_modules,
        crate::resolve::SystemPackages::default(),
    )
    .unwrap();

    let target = result.import_target(&root_id(&game), 0).unwrap();
    assert_eq!(target.base, provider_id(&host, &["audio"]));
    assert_eq!(loader.load_count_module(&module_id(&host, &["audio"])), 0);
}

#[test]
fn native_only_dependency_root_import_fails() {
    let game = package("game");
    let host = package("host");
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_package(
        game.clone(),
        "import pkg:host;",
        packages([
            (game, package_input(&[("host", host.clone())])),
            (host.clone(), native_package_input(&[])),
        ]),
        &mut loader,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::NativeOnlyPkgRootImport { package, alias, .. }]
            if package == &host && alias == "host"
    ));
}

#[test]
fn unknown_native_only_dependency_provider_module_fails() {
    let game = package("game");
    let host = package("host");
    let mut loader = InMemoryLoader::default();
    let errors = resolve_errors(resolve_package(
        game.clone(),
        "import pkg:host.audio;",
        packages([
            (game, package_input(&[("host", host.clone())])),
            (host.clone(), native_package_input(&[])),
        ]),
        &mut loader,
    ));

    assert!(matches!(
        errors.as_slice(),
        [ResolveError::UnknownNativeDepProviderModule { package, alias, module, .. }]
            if package == &host && alias == "host" && module.segments() == ["audio"]
    ));
}

#[test]
fn root_imports_resolve() {
    let game = package("game");
    let math = package("math");
    let mut loader = InMemoryLoader::default();
    loader.add_package_source(&math, module_path(["helpers"]), "");
    let result = resolve_package(
        game.clone(),
        "import pkg:math;",
        packages([
            (game, package_input(&[("math", math.clone())])),
            (math.clone(), package_root(&math, "import helpers;", &[])),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_root(&result, &math));
    assert!(has_module(&result, &math, &["helpers"]));
}

#[test]
fn root_aliases_source_entry() {
    let game = package("game");
    let math = package("math");
    let math_source = source_id(&math, "/canonical/math/src/lib.anv");
    let mut loader = InMemoryLoader::default();
    let result = resolve_package(
        game.clone(),
        "import pkg:math;",
        packages([
            (game, package_input(&[("math", math.clone())])),
            (
                math.clone(),
                PackageInput {
                    root: Some(loaded_module(math_source.clone(), "")),
                    dependencies: HashMap::new(),
                    kind: crate::resolve::PackageKind::Source,
                },
            ),
        ]),
        &mut loader,
    )
    .unwrap();

    assert!(has_source(&result, &math_source));
    assert_eq!(result.canonical_module(&root_id(&math)), &math_source);
}

#[test]
fn package_root_ids_can_be_asserted() {
    let game = package("game");

    assert_eq!(root_id(&game).package(), &game);
}
