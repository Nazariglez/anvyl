use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Program, Stmt},
    lexer, parser,
    resolve::{
        self, ModuleId, ModulePath, PackageId, ResolveResult, ResolvedImportTarget, ResolvedModule,
    },
    source::{SourceId, SourceKind, SourceTable},
};

pub(crate) fn test_source_id() -> SourceId {
    SourceTable::default().add(SourceKind::Virtual, "test", None, "")
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
