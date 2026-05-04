pub(crate) mod catalog;
mod identity;
mod providers;
mod raw;
mod shape;
mod source;

use std::collections::HashSet;

pub(crate) use identity::validate_raw_identities;
pub(crate) use providers::ingest_providers;
#[cfg(test)]
use providers::normalize_type;
pub use raw::ExternInputs;
pub(crate) use raw::{
    ExternInputError, ExternProvenance, RawExternDecl, RawExternFunctionKey, RawExternIdentityKey,
    RawExternMemberKey, RawExternModule, RawExternScope, RawExternSite, RawExternTypeKey,
    RawExterns, UnsupportedSourceKind, UnsupportedSourceParamReason,
};
pub(crate) use shape::validate_raw_shapes;
pub(crate) use source::collect_source_externs;

use crate::{resolve::ModulePath as ResolveModulePath, typecheck::ModuleScope};

pub(crate) fn extern_module_path(path: &anvyx_externs::ModulePath) -> ResolveModulePath {
    ResolveModulePath::new(path.segments.clone()).expect("raw extern module paths are validated")
}

pub(crate) fn extern_module_scope(path: &anvyx_externs::ModulePath) -> ModuleScope {
    ModuleScope::Named(extern_module_path(path))
}

pub(crate) fn raw_named_module_path(scope: &RawExternScope) -> Option<ResolveModulePath> {
    match scope {
        RawExternScope::Module(_) => None,
        RawExternScope::Named(path) => Some(extern_module_path(path)),
    }
}

pub(crate) fn raw_module_scope(scope: &RawExternScope) -> ModuleScope {
    match scope {
        RawExternScope::Named(path) => extern_module_scope(path),
        RawExternScope::Module(module) => ModuleScope::from_module_id(module),
    }
}

pub(crate) fn raw_extern_module_paths(raw: &RawExterns) -> HashSet<ResolveModulePath> {
    raw.groups
        .iter()
        .flat_map(|group| &group.modules)
        .filter_map(|module| raw_named_module_path(&module.scope))
        .collect()
}
#[cfg(test)]
mod tests {
    use anvyx_externs::{
        BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackSignature,
        ExternDescriptorError, ExternEffects, ExternFieldDescriptor, ExternFunctionDescriptor,
        ExternInitDescriptor, ExternMemberSelector, ExternMethodDescriptor, ExternModuleDescriptor,
        ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature,
        ExternStaticDescriptor, ExternTypeDescriptor, ExternTypeExpr, FieldAccess, ModulePath,
        OperatorReturn, ParamFlow, ProviderDescriptor, ProviderId, ReceiverMode, TypeContext,
    };

    use super::*;
    use crate::{
        ast::Program,
        externs::raw::{RawExternGroup, RawExternModule, RawExternType},
        resolve::{
            ModuleId, ModulePath as ResolveModulePath, PackageId, ResolveResult, ResolvedModule,
        },
        test_support::{empty_resolved, parse_program, resolved_modules, root_id},
    };

    fn provider(name: &str, modules: Vec<ExternModuleDescriptor>) -> ProviderDescriptor {
        ProviderDescriptor {
            provider: ProviderId {
                name: name.to_string(),
            },
            modules,
        }
    }

    fn module(path: &[&str]) -> ModulePath {
        ModulePath {
            segments: path.iter().map(|segment| (*segment).to_string()).collect(),
        }
    }

    fn param(name: &str, ty: ExternTypeExpr) -> ExternParam {
        ExternParam {
            name: Some(name.to_string()),
            ty,
            flow: ParamFlow::Value,
        }
    }

    fn signature(params: Vec<ExternParam>, ret: ExternTypeExpr) -> ExternSignature {
        ExternSignature { params, ret }
    }

    fn parse(source: &str) -> Program {
        parse_program(source)
    }

    fn resolve_path(path: &[&str]) -> ResolveModulePath {
        ResolveModulePath::new(path.iter().map(|segment| (*segment).to_string()).collect()).unwrap()
    }

    fn raw_root_scope() -> RawExternScope {
        RawExternScope::Module(root_id())
    }

    fn root_resolved(root: ModuleId) -> ResolveResult {
        let mut resolved = empty_resolved();
        resolved.root = root;
        resolved
    }

    fn collect_root_type(source: &str) -> RawExternType {
        let mut raw = collect_source_externs(&parse(source), &empty_resolved()).unwrap();
        raw.groups.remove(0).modules.remove(0).types.remove(0)
    }

    fn collect_modules(modules: &[(&str, &str)]) -> RawExterns {
        let root = parse("fn main() {}");
        collect_source_externs(&root, &resolved_modules(&root, modules)).unwrap()
    }

    fn named(name: &str) -> ExternTypeExpr {
        ExternTypeExpr::Named {
            module: None,
            name: name.to_string(),
            args: vec![],
        }
    }

    fn function(name: &str) -> ExternFunctionDescriptor {
        ExternFunctionDescriptor {
            name: name.to_string(),
            doc: None,
            signature: signature(vec![], ExternTypeExpr::Void),
            effects: ExternEffects::default(),
        }
    }

    fn extern_type(name: &str) -> ExternTypeDescriptor {
        ExternTypeDescriptor {
            name: name.to_string(),
            doc: None,
            rep: ExternRep::Shared,
            fields: vec![],
            init: None,
            methods: vec![],
            statics: vec![],
            operators: vec![],
        }
    }

    fn raw_provider_types(types: Vec<ExternTypeDescriptor>) -> RawExterns {
        RawExterns {
            groups: vec![RawExternGroup {
                provenance: ExternProvenance::Provider {
                    provider: ProviderId {
                        name: "p".to_string(),
                    },
                },
                modules: vec![RawExternModule {
                    scope: RawExternScope::Named(module(&["math"])),
                    types: types.into_iter().map(normalize_type).collect(),
                    functions: vec![],
                }],
            }],
        }
    }

    fn has_duplicate(
        errors: &[ExternInputError],
        matches_key: impl Fn(&RawExternIdentityKey) -> bool,
    ) -> bool {
        errors.iter().any(|error| match error {
            ExternInputError::DuplicateRawIdentity { key, .. } => matches_key(key),
            _ => false,
        })
    }

    mod provider {
        use super::*;

        #[test]
        fn ingests_valid() {
            let providers = ExternInputs {
                providers: vec![provider(
                    "math",
                    vec![ExternModuleDescriptor {
                        path: module(&["math"]),
                        types: vec![ExternTypeDescriptor {
                            name: "Vec2".to_string(),
                            doc: Some("vector".to_string()),
                            rep: ExternRep::Inline,
                            fields: vec![],
                            init: None,
                            methods: vec![],
                            statics: vec![],
                            operators: vec![],
                        }],
                        functions: vec![ExternFunctionDescriptor {
                            name: "dot".to_string(),
                            doc: Some("dot product".to_string()),
                            signature: signature(vec![], ExternTypeExpr::Float),
                            effects: ExternEffects::default(),
                        }],
                    }],
                )],
            };

            let raw = ingest_providers(providers).unwrap();

            assert_eq!(raw.groups.len(), 1);
            assert_eq!(
                raw.groups[0].provenance,
                ExternProvenance::Provider {
                    provider: ProviderId {
                        name: "math".to_string()
                    }
                }
            );
            assert_eq!(
                raw.groups[0].modules[0].scope,
                RawExternScope::Named(module(&["math"]))
            );
            assert_eq!(raw.groups[0].modules[0].types[0].name, "Vec2");
            assert_eq!(raw.groups[0].modules[0].functions[0].decl.name, "dot");
            assert_eq!(raw.groups[0].modules[0].functions[0].site.span, None);
        }

        #[test]
        fn rejects_invalid() {
            let providers = ExternInputs {
                providers: vec![provider(
                    "math",
                    vec![ExternModuleDescriptor {
                        path: module(&["math"]),
                        types: vec![],
                        functions: vec![
                            ExternFunctionDescriptor {
                                name: "dot".to_string(),
                                doc: None,
                                signature: signature(vec![], ExternTypeExpr::Float),
                                effects: ExternEffects::default(),
                            },
                            ExternFunctionDescriptor {
                                name: "dot".to_string(),
                                doc: None,
                                signature: signature(vec![], ExternTypeExpr::Float),
                                effects: ExternEffects::default(),
                            },
                        ],
                    }],
                )],
            };

            let errors = ingest_providers(providers).unwrap_err();

            assert_eq!(errors.len(), 1);
            assert_eq!(
                errors[0],
                ExternInputError::InvalidProviderDescriptor {
                    provider: ProviderId {
                        name: "math".to_string()
                    },
                    error: ExternDescriptorError::DuplicateFunction {
                        module: module(&["math"]),
                        name: "dot".to_string(),
                    }
                }
            );
        }
    }

    mod source {
        use super::*;

        #[test]
        fn collects_root_function() {
            let root = parse("/// Ticks.\nextern fn tick(dt: float) -> void;");
            let raw = collect_source_externs(&root, &empty_resolved()).unwrap();

            assert_eq!(raw.groups.len(), 1);
            assert_eq!(
                raw.groups[0].provenance,
                ExternProvenance::Source {
                    module: raw_root_scope()
                }
            );
            let module = &raw.groups[0].modules[0];
            assert_eq!(module.scope, raw_root_scope());
            let function = &module.functions[0].decl;
            assert_eq!(function.name, "tick");
            assert_eq!(function.doc.as_deref(), Some("Ticks."));
            assert_eq!(
                function.signature.params[0],
                param("dt", ExternTypeExpr::Float)
            );
            assert_eq!(function.signature.ret, ExternTypeExpr::Void);
            assert!(module.functions[0].site.span.is_some());
        }

        #[test]
        fn collects_named_function() {
            let root = parse("fn main() {}");
            let resolved = resolved_modules(&root, &[("math", "extern fn dot() -> float;")]);

            let raw = collect_source_externs(&root, &resolved).unwrap();

            assert_eq!(raw.groups.len(), 1);
            assert_eq!(
                raw.groups[0].provenance,
                ExternProvenance::Source {
                    module: RawExternScope::Named(module(&["math"]))
                }
            );
            assert_eq!(
                raw.groups[0].modules[0].scope,
                RawExternScope::Named(module(&["math"]))
            );
            assert_eq!(raw.groups[0].modules[0].functions[0].decl.name, "dot");
        }

        #[test]
        fn collects_package_root_function() {
            let package = PackageId::new("dep");
            let root = parse("extern fn tick() -> void;");
            let resolved = root_resolved(ModuleId::root(package.clone()));

            let raw = collect_source_externs(&root, &resolved).unwrap();

            let scope = RawExternScope::Module(ModuleId::root(package));
            assert_eq!(
                raw.groups[0].provenance,
                ExternProvenance::Source {
                    module: scope.clone()
                }
            );
            assert_eq!(raw.groups[0].modules[0].scope, scope);
            assert_eq!(raw.groups[0].modules[0].functions[0].decl.name, "tick");
        }

        #[test]
        fn same_source_extern_path_in_different_packages_does_not_collide() {
            let left = PackageId::new("left");
            let right = PackageId::new("right");
            let root = parse("fn main() {}");
            let mut resolved = empty_resolved();
            resolved.module_groups = vec![vec![
                ResolvedModule {
                    key: ModuleId::named(left, resolve_path(&["math"])),
                    program: parse("extern fn dot() -> void;"),
                },
                ResolvedModule {
                    key: ModuleId::named(right, resolve_path(&["math"])),
                    program: parse("extern fn dot() -> void;"),
                },
            ]];

            let raw = collect_source_externs(&root, &resolved).unwrap();

            validate_raw_identities(&raw).unwrap();
        }

        #[test]
        fn normalizes_members() {
            let root = parse(
                r"
                /// A point.
                extern type Point {
                    init;
                    /// x coordinate.
                    x: float;
                    /// move point.
                    fn move_by(var self, dx: float) -> void;
                    /// origin.
                    fn origin() -> Self;
                    op Self + float -> Self;
                    op float + Self -> Self;
                    op - Self -> Self;
                }
                ",
            );

            let raw = collect_source_externs(&root, &empty_resolved()).unwrap();
            let ty = &raw.groups[0].modules[0].types[0];

            assert_eq!(ty.name, "Point");
            assert_eq!(ty.doc.as_deref(), Some("A point."));
            assert_eq!(ty.rep, ExternRep::Shared);
            assert!(ty.site.span.is_some());
            assert!(ty.init.is_some());
            assert_eq!(ty.fields[0].decl.doc.as_deref(), Some("x coordinate."));
            assert_eq!(
                ty.fields[0].decl.access,
                FieldAccess::ReadWrite { computed: false }
            );
            assert_eq!(ty.methods[0].decl.receiver, ReceiverMode::Mutable);
            assert_eq!(
                ty.methods[0].decl.signature.params[0],
                param("dx", ExternTypeExpr::Float)
            );
            assert_eq!(ty.statics[0].decl.name, "origin");
            assert_eq!(
                ty.statics[0].decl.signature.ret,
                ExternTypeExpr::Named {
                    module: None,
                    name: "Point".to_string(),
                    args: vec![],
                }
            );
            assert_eq!(
                ty.operators[0].decl.op,
                ExternOperator::Binary {
                    op: BinaryOp::Add,
                    self_on_right: false,
                }
            );
            assert_eq!(
                ty.operators[1].decl.op,
                ExternOperator::Binary {
                    op: BinaryOp::Add,
                    self_on_right: true,
                }
            );
            assert_eq!(
                ty.operators[2].decl.op,
                ExternOperator::Unary(anvyx_externs::UnaryOp::Neg)
            );
        }

        #[test]
        fn normalizes_rep() {
            assert_eq!(collect_root_type("extern type T;").rep, ExternRep::Shared);
            assert_eq!(
                collect_root_type("extern type T rep shared;").rep,
                ExternRep::Shared
            );
            assert_eq!(
                collect_root_type("extern type T rep inline;").rep,
                ExternRep::Inline
            );
        }

        #[test]
        fn normalizes_source_field_access() {
            let ty = collect_root_type(
                r"
                extern type T {
                    plain: int;
                    var mutable: int;
                    let readonly: int;
                    computed cached: int;
                    computed var live: int;
                }
                ",
            );

            let fields = ty
                .fields
                .iter()
                .map(|field| (field.decl.name.as_str(), field.decl.access))
                .collect::<Vec<_>>();

            assert_eq!(
                fields,
                [
                    ("plain", FieldAccess::ReadWrite { computed: false }),
                    ("mutable", FieldAccess::ReadWrite { computed: false }),
                    ("readonly", FieldAccess::ReadOnly { computed: false }),
                    ("cached", FieldAccess::ReadOnly { computed: true }),
                    ("live", FieldAccess::ReadWrite { computed: true }),
                ]
            );
        }

        #[test]
        fn normalizes_empty_init() {
            for source in ["extern type T { init; }", "extern type T { init(); }"] {
                let ty = collect_root_type(source);
                let init = ty.init.unwrap();
                assert_eq!(init.decl.params, []);
                assert_eq!(init.decl.field_init, Vec::<String>::new());
                assert!(init.site.span.is_some());
            }
        }

        #[test]
        fn rejects_source_init_params() {
            let raw = collect_source_externs(
                &parse("extern type Point { init(x: float, y: float); }"),
                &empty_resolved(),
            )
            .unwrap();

            let errors = validate_raw_shapes(&raw).unwrap_err();
            assert!(matches!(
                errors[0],
                ExternInputError::InvalidRawDescriptor {
                    error: ExternDescriptorError::UnsupportedInitParams { count: 2, .. },
                    ..
                }
            ));
        }

        #[test]
        fn normalizes_source_receiver_modes() {
            let ty = collect_root_type(
                r"
                extern type T {
                    fn value(self) -> void;
                    fn shared(shared self) -> void;
                    fn mutable(var self) -> void;
                }
                ",
            );

            let receivers = ty
                .methods
                .iter()
                .map(|method| method.decl.receiver)
                .collect::<Vec<_>>();
            assert_eq!(
                receivers,
                [
                    ReceiverMode::Value,
                    ReceiverMode::Shared,
                    ReceiverMode::Mutable,
                ]
            );
        }

        #[test]
        fn normalizes_source_comparison_operators() {
            let ty = collect_root_type(
                r"
                extern type T {
                    op Self != Self -> bool;
                    op Self < Self -> bool;
                    op Self > Self -> bool;
                    op Self <= Self -> bool;
                    op Self >= Self -> bool;
                }
                ",
            );

            let ops = ty
                .operators
                .iter()
                .map(|operator| operator.decl.op)
                .collect::<Vec<_>>();
            assert_eq!(
                ops,
                [
                    ExternOperator::Binary {
                        op: BinaryOp::NotEq,
                        self_on_right: false,
                    },
                    ExternOperator::Binary {
                        op: BinaryOp::LessThan,
                        self_on_right: false,
                    },
                    ExternOperator::Binary {
                        op: BinaryOp::GreaterThan,
                        self_on_right: false,
                    },
                    ExternOperator::Binary {
                        op: BinaryOp::LessThanEq,
                        self_on_right: false,
                    },
                    ExternOperator::Binary {
                        op: BinaryOp::GreaterThanEq,
                        self_on_right: false,
                    },
                ]
            );
        }

        #[test]
        fn rejects_non_bool_comparison_operator_return() {
            for (source, op) in [
                ("extern type T { op Self == Self -> int; }", BinaryOp::Eq),
                ("extern type T { op Self != Self -> int; }", BinaryOp::NotEq),
                (
                    "extern type T { op Self < Self -> int; }",
                    BinaryOp::LessThan,
                ),
                (
                    "extern type T { op Self > Self -> int; }",
                    BinaryOp::GreaterThan,
                ),
                (
                    "extern type T { op Self <= Self -> int; }",
                    BinaryOp::LessThanEq,
                ),
                (
                    "extern type T { op Self >= Self -> int; }",
                    BinaryOp::GreaterThanEq,
                ),
            ] {
                let raw = collect_source_externs(&parse(source), &empty_resolved()).unwrap();
                let errors = validate_raw_shapes(&raw).unwrap_err();
                assert!(matches!(
                    &errors[0],
                    ExternInputError::InvalidRawDescriptor {
                        error: ExternDescriptorError::InvalidOperatorReturn {
                            op: ExternOperator::Binary { op: actual, .. },
                            expected: OperatorReturn::Bool,
                            actual: ExternTypeExpr::Int,
                            ..
                        },
                        ..
                    } if *actual == op
                ));
            }
        }

        #[test]
        fn normalizes_source_self_in_member_types() {
            let ty = collect_root_type(
                r"
                extern type Owner {
                    field: Self;
                    init;
                    fn method(self, owner: Self) -> Self;
                    fn make() -> Self;
                    op Self + Self -> Self;
                }
                ",
            );

            assert_eq!(ty.fields[0].decl.ty, named("Owner"));
            assert!(ty.init.unwrap().decl.params.is_empty());
            assert_eq!(ty.methods[0].decl.signature.params[0].ty, named("Owner"));
            assert_eq!(ty.methods[0].decl.signature.ret, named("Owner"));
            assert_eq!(ty.statics[0].decl.signature.ret, named("Owner"));
            assert_eq!(ty.operators[0].decl.signature.params[0].ty, named("Owner"));
            assert_eq!(ty.operators[0].decl.signature.ret, named("Owner"));
        }

        #[test]
        fn normalizes_source_callback_type() {
            let root = parse("extern fn each(callback: fn(int) -> string) -> void;");
            let raw = collect_source_externs(&root, &empty_resolved()).unwrap();

            let ExternTypeExpr::Callback(callback) =
                &raw.groups[0].modules[0].functions[0].decl.signature.params[0].ty
            else {
                panic!("expected callback type");
            };
            assert_eq!(callback.params, [ExternTypeExpr::Int]);
            assert_eq!(*callback.ret, ExternTypeExpr::String);
            assert_eq!(
                callback.policy,
                CallbackPolicy {
                    escape: CallbackEscape::NonEscaping,
                    thread: CallbackThread::SameThread,
                }
            );
        }

        #[test]
        fn rejects_mutable_source_callback_params() {
            let errors = collect_source_externs(
                &parse("extern fn each(callback: fn(var int) -> void);"),
                &empty_resolved(),
            )
            .unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::UnsupportedSource {
                    kind: UnsupportedSourceKind::CallbackParam {
                        reason: UnsupportedSourceParamReason::Mutable
                    },
                    ..
                }
            ));
        }

        #[test]
        fn rejects_unsupported_source_extern_types() {
            let tuple = parse("extern fn f(x: (int, int)) -> void;");
            let array = parse("extern fn f(x: [int; 2]) -> void;");
            let slice = parse("extern fn f(x: [int; _]) -> void;");
            let const_generic = parse("extern fn f(x: Vec<4>) -> void;");

            for program in [tuple, array, slice, const_generic] {
                assert!(matches!(
                    collect_source_externs(&program, &empty_resolved()).unwrap_err()[0],
                    ExternInputError::UnsupportedSource {
                        kind: UnsupportedSourceKind::Type(_),
                        ..
                    }
                ));
            }
        }

        #[test]
        fn rejects_param_decorations() {
            for source in [
                "extern fn f(var x: int) -> void;",
                "extern fn f(x: as int) -> void;",
                "extern fn f(x: int = 1) -> void;",
            ] {
                assert!(matches!(
                    collect_source_externs(&parse(source), &empty_resolved()).unwrap_err()[0],
                    ExternInputError::UnsupportedSource {
                        kind: UnsupportedSourceKind::Param { .. },
                        ..
                    }
                ));
            }
        }

        #[test]
        fn rejects_invalid_source_extern_shapes() {
            let raw =
                collect_source_externs(&parse("extern fn f(x: void) -> void;"), &empty_resolved())
                    .unwrap();

            let errors = validate_raw_shapes(&raw).unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::InvalidRawDescriptor {
                    error: ExternDescriptorError::VoidType {
                        context: TypeContext::Param
                    },
                    ..
                }
            ));
        }
    }

    mod identity {
        use super::*;

        #[test]
        fn rejects_duplicate_provider_functions() {
            let mut raw = ingest_providers(ExternInputs {
                providers: vec![
                    provider(
                        "a",
                        vec![ExternModuleDescriptor {
                            path: module(&["math"]),
                            types: vec![],
                            functions: vec![function("dot")],
                        }],
                    ),
                    provider(
                        "b",
                        vec![ExternModuleDescriptor {
                            path: module(&["math"]),
                            types: vec![],
                            functions: vec![function("dot")],
                        }],
                    ),
                ],
            })
            .unwrap();

            let errors = validate_raw_identities(&raw).unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::DuplicateRawIdentity {
                    key: RawExternIdentityKey::Function(_),
                    ..
                }
            ));
            raw.groups[1].modules[0].functions[0].decl.name = "cross".to_string();
            validate_raw_identities(&raw).unwrap();
        }

        #[test]
        fn rejects_duplicate_provider_types() {
            let raw = ingest_providers(ExternInputs {
                providers: vec![
                    provider(
                        "a",
                        vec![ExternModuleDescriptor {
                            path: module(&["math"]),
                            types: vec![extern_type("Vec2")],
                            functions: vec![],
                        }],
                    ),
                    provider(
                        "b",
                        vec![ExternModuleDescriptor {
                            path: module(&["math"]),
                            types: vec![extern_type("Vec2")],
                            functions: vec![],
                        }],
                    ),
                ],
            })
            .unwrap();

            let errors = validate_raw_identities(&raw).unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::DuplicateRawIdentity {
                    key: RawExternIdentityKey::Type(_),
                    ..
                }
            ));
        }

        #[test]
        fn rejects_function_conflict() {
            let mut raw = ingest_providers(ExternInputs {
                providers: vec![provider(
                    "math_provider",
                    vec![ExternModuleDescriptor {
                        path: module(&["math"]),
                        types: vec![],
                        functions: vec![function("dot")],
                    }],
                )],
            })
            .unwrap();
            let source = collect_modules(&[("math", "extern fn dot() -> void;")]);
            raw.append(source);

            let errors = validate_raw_identities(&raw).unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::DuplicateRawIdentity {
                    key: RawExternIdentityKey::Function(_),
                    ..
                }
            ));
        }

        #[test]
        fn rejects_type_conflict() {
            let mut raw = ingest_providers(ExternInputs {
                providers: vec![provider(
                    "math_provider",
                    vec![ExternModuleDescriptor {
                        path: module(&["math"]),
                        types: vec![extern_type("Vec2")],
                        functions: vec![],
                    }],
                )],
            })
            .unwrap();
            let source = collect_modules(&[("math", "extern type Vec2;")]);
            raw.append(source);

            let errors = validate_raw_identities(&raw).unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::DuplicateRawIdentity {
                    key: RawExternIdentityKey::Type(_),
                    ..
                }
            ));
        }

        #[test]
        fn rejects_root_duplicates() {
            let raw = collect_source_externs(
                &parse(
                    "extern fn f() -> void; extern fn f() -> void; extern type T; extern type T;",
                ),
                &empty_resolved(),
            )
            .unwrap();

            let errors = validate_raw_identities(&raw).unwrap_err();

            assert!(errors.iter().any(|error| matches!(
                error,
                ExternInputError::DuplicateRawIdentity {
                    key: RawExternIdentityKey::Function(_),
                    ..
                }
            )));
            assert!(errors.iter().any(|error| matches!(
                error,
                ExternInputError::DuplicateRawIdentity {
                    key: RawExternIdentityKey::Type(_),
                    ..
                }
            )));
        }

        #[test]
        fn rejects_duplicate_source_members() {
            let raw = collect_source_externs(
                &parse(
                    r"
                    extern type T {
                        x: int;
                        x: int;
                        fn move(self) -> void;
                        fn move(self) -> void;
                        fn create() -> Self;
                        fn create() -> Self;
                        op Self + int -> Self;
                        op Self + int -> Self;
                    }
                    ",
                ),
                &empty_resolved(),
            )
            .unwrap();

            let errors = validate_raw_identities(&raw).unwrap_err();

            assert!(has_duplicate(&errors, |key| matches!(
                key,
                RawExternIdentityKey::Member(RawExternMemberKey {
                    selector: ExternMemberSelector::Field(_),
                    ..
                })
            )));
            assert!(has_duplicate(&errors, |key| matches!(
                key,
                RawExternIdentityKey::Member(RawExternMemberKey {
                    selector: ExternMemberSelector::Method(_),
                    ..
                })
            )));
            assert!(has_duplicate(&errors, |key| matches!(
                key,
                RawExternIdentityKey::Member(RawExternMemberKey {
                    selector: ExternMemberSelector::Static(_),
                    ..
                })
            )));
            assert!(has_duplicate(&errors, |key| matches!(
                key,
                RawExternIdentityKey::Member(RawExternMemberKey {
                    selector: ExternMemberSelector::Operator(_),
                    ..
                })
            )));
        }

        #[test]
        fn rejects_duplicate_raw_init() {
            let mut first = extern_type("T");
            first.init = Some(ExternInitDescriptor {
                params: vec![],
                field_init: vec![],
            });
            let mut duplicate = extern_type("T");
            duplicate.init = first.init.clone();
            let raw = raw_provider_types(vec![first, duplicate]);

            let errors = validate_raw_identities(&raw).unwrap_err();

            assert!(has_duplicate(&errors, |key| matches!(
                key,
                RawExternIdentityKey::Member(RawExternMemberKey {
                    selector: ExternMemberSelector::Init,
                    ..
                })
            )));
        }

        #[test]
        fn rejects_duplicate_init_fields() {
            let mut ty = extern_type("T");
            ty.init = Some(ExternInitDescriptor {
                params: vec![],
                field_init: vec!["x".to_string(), "x".to_string()],
            });
            let raw = raw_provider_types(vec![ty]);

            let errors = validate_raw_shapes(&raw).unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::InvalidRawDescriptor {
                    error: ExternDescriptorError::DuplicateFieldInit { .. },
                    ..
                }
            ));
        }
    }

    mod shape {
        use super::*;

        #[test]
        fn rejects_provider_init_params() {
            let mut ty = extern_type("T");
            ty.init = Some(ExternInitDescriptor {
                params: vec![param("x", ExternTypeExpr::Int)],
                field_init: vec![],
            });
            let raw = raw_provider_types(vec![ty]);

            let errors = validate_raw_shapes(&raw).unwrap_err();

            assert!(matches!(
                errors[0],
                ExternInputError::InvalidRawDescriptor {
                    error: ExternDescriptorError::UnsupportedInitParams { count: 1, .. },
                    ..
                }
            ));
        }

        #[test]
        fn accepts_distinct_raw_namespaces() {
            let raw = collect_source_externs(
                &parse(
                    r"
                    extern type T {
                        fn size(self) -> int;
                        fn size() -> int;
                    }
                    ",
                ),
                &empty_resolved(),
            )
            .unwrap();

            validate_raw_identities(&raw).unwrap();
        }

        #[test]
        fn accepts_same_names_in_different_scopes() {
            let mut raw = ingest_providers(ExternInputs {
                providers: vec![
                    provider(
                        "a",
                        vec![ExternModuleDescriptor {
                            path: module(&["a"]),
                            types: vec![extern_type("T")],
                            functions: vec![function("f")],
                        }],
                    ),
                    provider(
                        "b",
                        vec![ExternModuleDescriptor {
                            path: module(&["b"]),
                            types: vec![extern_type("T")],
                            functions: vec![function("f")],
                        }],
                    ),
                ],
            })
            .unwrap();
            raw.append(
                collect_source_externs(
                    &parse("extern fn f() -> void; extern type T;"),
                    &empty_resolved(),
                )
                .unwrap(),
            );

            validate_raw_identities(&raw).unwrap();
        }

        #[test]
        fn preserves_descriptor_child_facts() {
            let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
                params: vec![ExternTypeExpr::Int],
                ret: Box::new(ExternTypeExpr::Float),
                policy: CallbackPolicy {
                    escape: CallbackEscape::Escaping,
                    thread: CallbackThread::SameThread,
                },
            });
            let providers = ExternInputs {
                providers: vec![provider(
                    "gfx",
                    vec![ExternModuleDescriptor {
                        path: module(&["gfx"]),
                        functions: vec![],
                        types: vec![ExternTypeDescriptor {
                            name: "Sprite".to_string(),
                            doc: Some("sprite".to_string()),
                            rep: ExternRep::Shared,
                            fields: vec![ExternFieldDescriptor {
                                name: "x".to_string(),
                                ty: ExternTypeExpr::Float,
                                access: FieldAccess::ReadWrite { computed: true },
                                doc: Some("x pos".to_string()),
                            }],
                            init: Some(ExternInitDescriptor {
                                params: vec![],
                                field_init: vec!["x".to_string()],
                            }),
                            methods: vec![ExternMethodDescriptor {
                                name: "move".to_string(),
                                doc: Some("move sprite".to_string()),
                                receiver: ReceiverMode::Mutable,
                                signature: signature(
                                    vec![ExternParam {
                                        name: Some("dx".to_string()),
                                        ty: ExternTypeExpr::Float,
                                        flow: ParamFlow::Borrow,
                                    }],
                                    ExternTypeExpr::Void,
                                ),
                                effects: ExternEffects { fallible: true },
                            }],
                            statics: vec![ExternStaticDescriptor {
                                name: "load".to_string(),
                                doc: Some("load sprite".to_string()),
                                signature: signature(
                                    vec![param("path", ExternTypeExpr::String)],
                                    callback,
                                ),
                                effects: ExternEffects { fallible: true },
                            }],
                            operators: vec![ExternOperatorDescriptor {
                                op: ExternOperator::Binary {
                                    op: BinaryOp::Add,
                                    self_on_right: true,
                                },
                                signature: signature(
                                    vec![param("other", ExternTypeExpr::Int)],
                                    ExternTypeExpr::Int,
                                ),
                                effects: ExternEffects::default(),
                            }],
                        }],
                    }],
                )],
            };

            let raw = ingest_providers(providers).unwrap();
            let ty = &raw.groups[0].modules[0].types[0];

            assert_eq!(ty.doc.as_deref(), Some("sprite"));
            assert_eq!(ty.rep, ExternRep::Shared);
            assert_eq!(
                ty.fields[0].decl.access,
                FieldAccess::ReadWrite { computed: true }
            );
            assert_eq!(ty.init.as_ref().unwrap().decl.field_init, ["x"]);
            assert_eq!(ty.methods[0].decl.receiver, ReceiverMode::Mutable);
            assert_eq!(
                ty.methods[0].decl.signature.params[0].flow,
                ParamFlow::Borrow
            );
            assert!(ty.methods[0].decl.effects.fallible);
            assert!(ty.statics[0].decl.effects.fallible);
            assert_eq!(
                ty.operators[0].decl.op,
                ExternOperator::Binary {
                    op: BinaryOp::Add,
                    self_on_right: true,
                }
            );
            assert!(matches!(
                ty.statics[0].decl.signature.ret,
                ExternTypeExpr::Callback(_)
            ));
        }
    }
}
