use std::{
    collections::{HashMap, HashSet},
    sync::{Mutex, OnceLock},
};

use crate::{
    ast::{Program, Stmt},
    lexer, parser,
    resolve::{
        self, ModuleId, ModulePath, PackageId, ResolveResult, ResolvedImportTarget, ResolvedModule,
    },
    source::{SourceId, SourceKind, SourceTable},
};

pub(crate) fn test_source_id() -> SourceId {
    static SOURCES: OnceLock<Mutex<SourceTable>> = OnceLock::new();
    SOURCES
        .get_or_init(|| Mutex::new(SourceTable::default()))
        .lock()
        .expect("test source table lock poisoned")
        .add(SourceKind::Virtual, "test", None, "")
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
