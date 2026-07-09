use std::collections::{HashMap, HashSet};

use anvyx_externs::ProviderDescriptor;

use crate::{
    ast::{Ident, ImportItemKind, ImportKind, ModuleOrigin, NominalKind, Program, Stmt, Type},
    externs,
    externs::{ExternInputs, PackageExternInputs},
    lexer, parser,
    resolve::{
        self, ModuleId, ModulePath, PackageId, ResolveResult, ResolvedImportTarget, ResolvedModule,
    },
    source::{SourceId, SourceKind, SourceTable},
    typecheck::{self, ModuleScope, NominalKey, TypecheckConfig},
};

pub(crate) fn test_source_id() -> SourceId {
    SourceTable::default().add(SourceKind::Virtual, "test", None, "")
}

pub(crate) fn ident(name: &str) -> Ident {
    Ident::new(name)
}

pub(crate) fn parse_program(source: &str) -> Program {
    let mut sources = SourceTable::default();
    let source_id = sources.add(SourceKind::Virtual, "test", None, source);
    let tokens = lexer::tokenize(source_id, source).expect("lexer error");
    parser::parse_ast(&tokens).expect("parse error")
}

pub(crate) fn root_id() -> ModuleId {
    ModuleId::root(PackageId::synthetic_root())
}

pub(crate) fn module_path(path: &str) -> ModulePath {
    ModulePath::new(path.split('.').map(str::to_string).collect()).unwrap()
}

pub(crate) fn module_path_segments(path: &[&str]) -> ModulePath {
    ModulePath::new(path.iter().map(ToString::to_string).collect()).unwrap()
}

pub(crate) fn core_option_key() -> NominalKey {
    NominalKey {
        module: ModuleScope::Package(ModuleId::named(PackageId::core(), module_path("option"))),
        kind: NominalKind::Enum,
        name: ident(Type::OPTION_ENUM_NAME),
    }
}

pub(crate) fn core_option_origin() -> ModuleOrigin {
    ModuleOrigin::Package {
        package: PackageId::core().as_str().to_string(),
        path: Some(module_path("option").to_ast_path()),
    }
}

pub(crate) fn core_option_type(inner: Type) -> Type {
    Type::nominal_with_origin(
        NominalKind::Enum,
        ident(Type::OPTION_ENUM_NAME),
        vec![inner],
        vec![],
        Some(core_option_origin()),
    )
}

fn assert_core_provider_names_match_source_imports(
    modules: &[ResolvedModule],
    providers: &[ProviderDescriptor],
) {
    let mut provider_names = HashMap::<Vec<String>, HashSet<String>>::new();
    for provider in providers {
        for module in &provider.modules {
            provider_names.insert(
                module.path.segments.clone(),
                module
                    .functions
                    .iter()
                    .map(|function| function.name.clone())
                    .collect(),
            );
        }
    }

    for module in modules {
        for stmt in &module.program.stmts {
            let Stmt::Import(import) = &stmt.node else {
                continue;
            };
            if import.node.target.root != crate::ast::ImportRoot::NativeProvider {
                continue;
            }
            let crate::ast::PackageModulePath::Named(path) = &import.node.target.path else {
                panic!("core extern imports must name provider modules");
            };
            let provider_path = path.iter().map(ToString::to_string).collect::<Vec<_>>();
            let ImportKind::Selective(items) = &import.node.kind else {
                panic!("core extern imports must list provider functions explicitly");
            };
            let imported = items
                .iter()
                .map(|item| match &item.kind {
                    ImportItemKind::Name(name) => name.to_string(),
                    ImportItemKind::SelfModule => panic!("core extern imports cannot import self"),
                })
                .collect::<HashSet<_>>();
            let declared = provider_names
                .get(&provider_path)
                .unwrap_or_else(|| panic!("missing provider descriptor for {provider_path:?}"));
            assert_eq!(declared, &imported, "provider/source extern import drift");
        }
    }
}

pub(crate) fn checked_with_full_core_shape(
    source: &str,
) -> (Program, ResolveResult, typecheck::SemanticCheckOutput) {
    let root = parse_program(source);
    let core_root = parse_program(anvyx_core::ROOT.code);
    let mut modules = anvyx_core::MODULES
        .iter()
        .map(|source| ResolvedModule {
            key: ModuleId::named(
                PackageId::core(),
                ModulePath::new(
                    source
                        .path
                        .iter()
                        .map(|segment| (*segment).to_string())
                        .collect(),
                )
                .expect("valid core module path"),
            ),
            source: test_source_id(),
            program: parse_program(source.code),
        })
        .collect::<Vec<_>>();
    modules.insert(
        0,
        ResolvedModule {
            key: ModuleId::root(PackageId::core()),
            source: test_source_id(),
            program: core_root,
        },
    );
    let providers = anvyx_core::provider_descriptors();
    assert_core_provider_names_match_source_imports(&modules, &providers);
    checked_with_core_modules(root, modules, providers)
}

fn checked_with_core_modules(
    root: Program,
    core_modules: Vec<ResolvedModule>,
    providers: Vec<ProviderDescriptor>,
) -> (Program, ResolveResult, typecheck::SemanticCheckOutput) {
    let provider_raw = externs::ingest_providers(ExternInputs {
        packages: vec![PackageExternInputs {
            package: PackageId::core(),
            providers,
        }],
    })
    .expect("valid providers");
    let external_modules = externs::raw_extern_module_ids(&provider_raw);
    let mut resolved = resolved_modules_with_external(&root, &[], &external_modules);
    for module in &external_modules {
        resolved.import_edges.entry(module.clone()).or_default();
    }
    for module in &core_modules {
        resolved.import_edges.insert(
            module.key.clone(),
            import_targets(
                &module.key,
                &module.program,
                module.source,
                &external_modules,
            ),
        );
    }
    resolved.module_groups.push(core_modules);
    resolved.system.core = Some(PackageId::core());
    let raw = externs::prepare_raw_externs(provider_raw, &root, &resolved).unwrap();
    let semantic =
        typecheck::check_semantic_with_modules(&root, &resolved, raw, TypecheckConfig::default())
            .expect("typecheck failed");
    (root, resolved, semantic)
}

pub(crate) fn empty_resolved() -> ResolveResult {
    ResolveResult {
        root: root_id(),
        root_source: test_source_id(),
        module_groups: vec![],
        dependencies: HashMap::new(),
        system: resolve::SystemPackages::default(),
        module_aliases: HashMap::new(),
        import_edges: HashMap::new(),
    }
}

pub(crate) fn resolved_with_core_option(root: &Program) -> ResolveResult {
    resolved_modules_with_core_option(root, &[])
}

pub(crate) fn resolved_modules_with_core_option(
    root: &Program,
    modules: &[(&str, &str)],
) -> ResolveResult {
    resolved_modules_with_core_option_external(root, modules, &HashSet::new())
}

pub(crate) fn resolved_modules_with_core_option_external(
    root: &Program,
    modules: &[(&str, &str)],
    external_modules: &HashSet<ModuleId>,
) -> ResolveResult {
    let mut resolved = resolved_modules_with_external(root, modules, external_modules);
    let core_package = PackageId::core();
    let core_root = ModuleId::root(core_package.clone());
    let core_option = ModuleId::named(core_package.clone(), module_path("option"));
    let core_root_source = test_source_id();
    let core_option_source = test_source_id();
    let core_root_program = parse_program("pub import option { Option };");
    let core_option_program = parse_program("pub enum Option<T> { None, Some(T) }");

    resolved.import_edges.insert(
        core_root.clone(),
        import_targets(
            &core_root,
            &core_root_program,
            core_root_source,
            &HashSet::new(),
        ),
    );
    resolved.import_edges.insert(
        core_option.clone(),
        import_targets(
            &core_option,
            &core_option_program,
            core_option_source,
            &HashSet::new(),
        ),
    );
    resolved.module_groups.push(vec![
        ResolvedModule {
            key: core_root,
            source: core_root_source,
            program: core_root_program,
        },
        ResolvedModule {
            key: core_option,
            source: core_option_source,
            program: core_option_program,
        },
    ]);
    resolved.system.core = Some(core_package);
    resolved
}

pub(crate) fn resolved_modules(root: &Program, modules: &[(&str, &str)]) -> ResolveResult {
    resolved_modules_with_external(root, modules, &HashSet::new())
}

pub(crate) fn resolved_modules_with_external(
    root: &Program,
    modules: &[(&str, &str)],
    external_modules: &HashSet<ModuleId>,
) -> ResolveResult {
    let root_source = test_source_id();
    let mut import_edges = HashMap::new();
    import_edges.insert(
        root_id(),
        import_targets(&root_id(), root, root_source, external_modules),
    );

    let module_groups = vec![
        modules
            .iter()
            .map(|(name, source)| {
                let key = ModuleId::named(PackageId::synthetic_root(), module_path(name));
                let program = parse_program(source);
                let source = test_source_id();
                import_edges.insert(
                    key.clone(),
                    import_targets(&key, &program, source, external_modules),
                );
                ResolvedModule {
                    key,
                    source,
                    program,
                }
            })
            .collect(),
    ];

    ResolveResult {
        root: root_id(),
        root_source,
        module_groups,
        dependencies: HashMap::new(),
        system: resolve::SystemPackages::default(),
        module_aliases: HashMap::new(),
        import_edges,
    }
}

fn import_targets(
    module: &ModuleId,
    program: &Program,
    source: SourceId,
    external_modules: &HashSet<ModuleId>,
) -> Vec<ResolvedImportTarget> {
    let mut errors = vec![];
    program
        .stmts
        .iter()
        .filter_map(|stmt| match &stmt.node {
            Stmt::Import(import) => Some(import),
            _ => None,
        })
        .filter_map(|import| {
            resolve::resolve_import_target(
                module,
                import,
                crate::span::SourceSpan::new(source, import.span.start, import.span.end),
                &HashMap::new(),
                &HashMap::new(),
                None,
                external_modules,
                &mut errors,
            )
        })
        .collect()
}
