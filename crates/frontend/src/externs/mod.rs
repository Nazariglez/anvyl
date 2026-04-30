mod identity;
mod providers;
mod raw;
mod shape;
mod source;

pub(crate) use identity::validate_raw_identities;
pub(crate) use providers::ingest_providers;
#[cfg(test)]
use providers::normalize_type;
pub use raw::ExternInputs;
pub(crate) use raw::{
    ExternInputError, ExternProvenance, RawExternDecl, RawExternFunctionKey, RawExternIdentityKey,
    RawExternMemberKey, RawExternScope, RawExternTypeKey, RawExterns, UnsupportedSourceKind,
    UnsupportedSourceParamReason,
};
pub(crate) use shape::validate_raw_shapes;
pub(crate) use source::collect_source_externs;

#[cfg(test)]
mod tests {
    use anvyx_externs::{
        BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackSignature,
        ExternDescriptorError, ExternEffects, ExternFieldDescriptor, ExternFunctionDescriptor,
        ExternInitDescriptor, ExternMemberSelector, ExternMethodDescriptor, ExternModuleDescriptor,
        ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature,
        ExternStaticDescriptor, ExternTypeDescriptor, ExternTypeExpr, FieldAccess, ModulePath,
        ParamFlow, ProviderDescriptor, ProviderId, ReceiverMode, TypeContext,
    };

    use super::*;
    use crate::{
        ast::Program,
        externs::raw::{RawExternGroup, RawExternModule},
        lexer::tokenize,
        parser,
        resolve::{ModuleKey, ModulePath as ResolveModulePath, ResolveResult, ResolvedModule},
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
        let tokens = tokenize(source).expect("tokenize failed");
        parser::parse_ast(&tokens).expect("parse failed")
    }

    fn resolve_path(path: &[&str]) -> ResolveModulePath {
        ResolveModulePath::new(path.iter().map(|segment| (*segment).to_string()).collect()).unwrap()
    }

    fn empty_resolved() -> ResolveResult {
        ResolveResult {
            module_groups: vec![],
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

    #[test]
    fn ingests_valid_provider_with_provenance() {
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
    fn rejects_invalid_provider_descriptor() {
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

    #[test]
    fn collects_root_source_extern_function() {
        let root = parse("/// Ticks.\nextern fn tick(dt: float) -> void;");
        let raw = collect_source_externs(&root, &empty_resolved()).unwrap();

        assert_eq!(raw.groups.len(), 1);
        assert_eq!(
            raw.groups[0].provenance,
            ExternProvenance::Source {
                module: RawExternScope::Root
            }
        );
        let module = &raw.groups[0].modules[0];
        assert_eq!(module.scope, RawExternScope::Root);
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
    fn collects_named_module_source_extern_function() {
        let root = parse("fn main() {}");
        let resolved = ResolveResult {
            module_groups: vec![vec![ResolvedModule {
                key: ModuleKey::Named(resolve_path(&["math"])),
                program: parse("extern fn dot() -> float;"),
            }]],
        };

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
    fn normalizes_source_extern_type_members() {
        let root = parse(
            r#"
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
            "#,
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
    fn rejects_provider_and_source_named_function_duplicates() {
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
        let source = collect_source_externs(
            &parse("fn main() {}"),
            &ResolveResult {
                module_groups: vec![vec![ResolvedModule {
                    key: ModuleKey::Named(resolve_path(&["math"])),
                    program: parse("extern fn dot() -> void;"),
                }]],
            },
        )
        .unwrap();
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
    fn rejects_provider_and_source_named_type_duplicates() {
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
        let source = collect_source_externs(
            &parse("fn main() {}"),
            &ResolveResult {
                module_groups: vec![vec![ResolvedModule {
                    key: ModuleKey::Named(resolve_path(&["math"])),
                    program: parse("extern type Vec2;"),
                }]],
            },
        )
        .unwrap();
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
    fn rejects_duplicate_source_root_functions_and_types() {
        let raw = collect_source_externs(
            &parse("extern fn f() -> void; extern fn f() -> void; extern type T; extern type T;"),
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
                r#"
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
                "#,
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

    #[test]
    fn accepts_distinct_raw_namespaces() {
        let raw = collect_source_externs(
            &parse(
                r#"
                extern type T {
                    fn size(self) -> int;
                    fn size() -> int;
                }
                "#,
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
                            params: vec![param("callback", callback.clone())],
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
