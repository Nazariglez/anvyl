use anvyx_externs::{
    BinaryOp as ExternBinaryOp, CallbackEscape, CallbackPolicy, CallbackThread,
    ExternCallbackParam, ExternCallbackSignature, ExternEffects, ExternFieldDescriptor,
    ExternFunctionDescriptor, ExternInitDescriptor, ExternMethodDescriptor, ExternModuleDescriptor,
    ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature,
    ExternStaticDescriptor, ExternTypeDescriptor, ExternTypeExpr, ModulePath as ExternModulePath,
    ParamFlow, ProviderDescriptor, ProviderId, ReceiverMode, UnaryOp,
};

use super::support::{
    TypecheckTestResult, assert_deprecated_warning, assert_typecheck_closed, check, check_named,
    check_with_raw_externs, generic_body, nominal_struct, single_expected_projection,
};
use crate::{
    ast::{ExprId, Ident, ModuleOrigin, NominalKind, Type},
    externs::{
        self, ExternInputs, PackageExternInputs,
        catalog::{
            ExternFieldRef, ExternMethodRef, ExternOperatorRef, ExternStaticRef, ExternTypeId,
        },
    },
    resolve::{ModuleId, ModulePath, PackageId},
    test_support::{parse_program, resolved_modules_with_core_option_external},
    typecheck::{
        self, CallableId, DeprecatedUseKind, ExternUseTarget, MemberPathKind, ModuleScope,
        TypeError,
    },
};

#[test]
fn deprecated_source_extern_type_warns_on_type_reference() {
    let result = check(
        "@deprecated(\"use NewHandle\") extern type Handle;
         fn use_handle(handle: Handle) {}
         fn main() {}",
    )
    .unwrap();

    assert_deprecated_warning(
        &result,
        DeprecatedUseKind::ExternType,
        "Handle",
        Some("use NewHandle"),
    );
}

fn extern_path(segments: &[&str]) -> ExternModulePath {
    ExternModulePath {
        segments: segments
            .iter()
            .map(|segment| (*segment).to_string())
            .collect(),
    }
}

fn function_key(module: ModuleScope, name: &str) -> externs::catalog::FunctionKey {
    externs::catalog::FunctionKey {
        module,
        name: Ident::new(name),
    }
}

fn type_key(module: ModuleScope, name: &str) -> externs::catalog::TypeKey {
    externs::catalog::TypeKey {
        module,
        name: Ident::new(name),
    }
}

fn catalog_type(result: &TypecheckTestResult, module: ModuleScope, name: &str) -> ExternTypeId {
    result
        .externs()
        .type_by_key(&type_key(module, name))
        .expect("extern type")
}

fn catalog_field(result: &TypecheckTestResult, owner: ExternTypeId, name: &str) -> ExternFieldRef {
    result
        .externs()
        .field(owner, Ident::new(name))
        .expect("extern field")
        .0
}

fn catalog_method(
    result: &TypecheckTestResult,
    owner: ExternTypeId,
    name: &str,
) -> ExternMethodRef {
    result
        .externs()
        .method(owner, Ident::new(name))
        .expect("extern method")
        .0
}

fn catalog_static(
    result: &TypecheckTestResult,
    owner: ExternTypeId,
    name: &str,
) -> ExternStaticRef {
    result
        .externs()
        .static_method(owner, Ident::new(name))
        .expect("extern static")
        .0
}

fn catalog_binary_operator(
    result: &TypecheckTestResult,
    owner: ExternTypeId,
    op: ExternBinaryOp,
) -> ExternOperatorRef {
    result
        .externs()
        .binary_operator(owner, op, false)
        .expect("extern operator")
        .0
}

fn scope(path: &[&str]) -> ModuleScope {
    ModuleScope::Named(
        ModulePath::new(path.iter().map(|segment| (*segment).to_string()).collect()).unwrap(),
    )
}

fn provider_scope(path: &[&str]) -> ModuleScope {
    ModuleScope::from_module_id(&ModuleId::provider(
        PackageId::synthetic_root(),
        ModulePath::new(path.iter().map(|segment| (*segment).to_string()).collect()).unwrap(),
    ))
}

fn function(name: &str, params: Vec<ExternParam>, ret: ExternTypeExpr) -> ExternFunctionDescriptor {
    ExternFunctionDescriptor {
        name: name.to_string(),
        doc: None,
        signature: ExternSignature { params, ret },
        effects: ExternEffects::default(),
    }
}

fn param(name: &str, ty: ExternTypeExpr) -> ExternParam {
    flow_param(name, ty, ParamFlow::Value)
}

fn flow_param(name: &str, ty: ExternTypeExpr, flow: ParamFlow) -> ExternParam {
    ExternParam {
        name: Some(name.to_string()),
        ty,
        flow,
        escape: CallbackEscape::NonEscaping,
    }
}

fn extern_type(name: &str) -> ExternTypeDescriptor {
    ExternTypeDescriptor {
        name: name.to_string(),
        doc: None,
        rep: ExternRep::Shared,
        fields: vec![],
        variants: vec![],
        init: None,
        methods: vec![],
        statics: vec![],
        operators: vec![],
    }
}

fn field(name: &str, ty: ExternTypeExpr) -> ExternFieldDescriptor {
    ExternFieldDescriptor {
        name: name.to_string(),
        ty,
        computed: false,
        readable: true,
        writable: true,
        get_receiver: ReceiverMode::Shared,
        set_receiver: ReceiverMode::Mutable,
        doc: None,
    }
}

fn computed_field(name: &str, ty: ExternTypeExpr) -> ExternFieldDescriptor {
    ExternFieldDescriptor {
        computed: true,
        ..field(name, ty)
    }
}

fn method(
    name: &str,
    receiver: ReceiverMode,
    params: Vec<ExternParam>,
    ret: ExternTypeExpr,
) -> ExternMethodDescriptor {
    ExternMethodDescriptor {
        name: name.to_string(),
        receiver,
        doc: None,
        signature: ExternSignature { params, ret },
        effects: ExternEffects::default(),
    }
}

fn static_method(
    name: &str,
    params: Vec<ExternParam>,
    ret: ExternTypeExpr,
) -> ExternStaticDescriptor {
    ExternStaticDescriptor {
        name: name.to_string(),
        doc: None,
        signature: ExternSignature { params, ret },
        effects: ExternEffects::default(),
    }
}

fn operator(
    op: ExternOperator,
    params: Vec<ExternParam>,
    ret: ExternTypeExpr,
) -> ExternOperatorDescriptor {
    ExternOperatorDescriptor {
        op,
        receiver: ReceiverMode::Shared,
        signature: ExternSignature { params, ret },
        effects: ExternEffects::default(),
    }
}

fn provider(module: ExternModuleDescriptor) -> ProviderDescriptor {
    provider_with_modules(vec![module])
}

fn provider_with_modules(modules: Vec<ExternModuleDescriptor>) -> ProviderDescriptor {
    ProviderDescriptor {
        provider: ProviderId {
            name: "host".to_string(),
        },
        modules,
    }
}

fn touch_provider(types: Vec<ExternTypeDescriptor>, ty: ExternTypeExpr) -> ProviderDescriptor {
    provider(ExternModuleDescriptor {
        path: extern_path(&["host"]),
        types,
        functions: vec![function(
            "touch",
            vec![flow_param("x", ty, ParamFlow::MutBorrow)],
            ExternTypeExpr::Void,
        )],
    })
}

fn nested_field_types() -> Vec<ExternTypeDescriptor> {
    vec![
        ExternTypeDescriptor {
            fields: vec![field("x", ExternTypeExpr::Float)],
            ..extern_type("Child")
        },
        ExternTypeDescriptor {
            fields: vec![field("child", named("Child"))],
            ..extern_type("Parent")
        },
    ]
}

fn named(name: &str) -> ExternTypeExpr {
    ExternTypeExpr::Named {
        module: None,
        name: name.to_string(),
        args: vec![],
    }
}

fn module_named(module: &[&str], name: &str) -> ExternTypeExpr {
    ExternTypeExpr::Named {
        module: Some(extern_path(module)),
        name: name.to_string(),
        args: vec![],
    }
}

fn callback(params: Vec<ExternTypeExpr>, ret: ExternTypeExpr) -> ExternTypeExpr {
    ExternTypeExpr::Callback(ExternCallbackSignature {
        params: params
            .into_iter()
            .map(|ty| ExternCallbackParam {
                ty,
                escape: CallbackEscape::NonEscaping,
            })
            .collect(),
        ret: Box::new(ret),
        policy: CallbackPolicy {
            escape: CallbackEscape::NonEscaping,
            thread: CallbackThread::SameThread,
        },
    })
}

fn check_with_provider(
    root_source: &str,
    provider: ProviderDescriptor,
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    check_named_with_provider(root_source, &[], provider)
}

fn check_named_with_provider(
    root_source: &str,
    modules: &[(&str, &str)],
    provider: ProviderDescriptor,
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    let root = parse_program(root_source);
    let provider_raw = externs::ingest_providers(ExternInputs {
        packages: vec![PackageExternInputs {
            package: PackageId::synthetic_root(),
            providers: vec![provider],
        }],
    })
    .expect("valid provider");
    let external_modules = externs::raw_extern_module_ids(&provider_raw);
    let resolved = resolved_modules_with_core_option_external(&root, modules, &external_modules);
    let raw = externs::prepare_raw_externs(provider_raw, &root, &resolved).expect("valid externs");
    check_with_raw_externs(&root, &resolved, raw)
}

mod result {
    use super::*;

    #[test]
    fn exposes_source_catalog() {
        let result = check(
            r"
            extern fn tick(dt: float) -> void;
            extern type Handle;
            ",
        )
        .expect("typecheck failed");
        let catalog = result.externs();

        assert!(
            catalog
                .function_by_key(&function_key(ModuleScope::Root, "tick"))
                .is_some()
        );
        assert!(
            catalog
                .type_by_key(&type_key(ModuleScope::Root, "Handle"))
                .is_some()
        );
        assert!(result.extern_uses().is_empty());
        assert_typecheck_closed(&result);
    }

    #[test]
    fn uncalled_has_no_uses() {
        let result = check(
            r"
            extern fn tick(dt: float) -> void;
            fn main() {}
            ",
        )
        .expect("typecheck failed");

        assert!(result.extern_uses().is_empty());
        assert_typecheck_closed(&result);
    }
}

mod calls {
    use super::*;

    #[test]
    fn records_function() {
        let result = check(
            r"
            extern fn tick(dt: float) -> void;
            fn main() { tick(1.0); }
            ",
        )
        .expect("typecheck failed");
        let id = result
            .externs()
            .function_by_key(&function_key(ModuleScope::Root, "tick"))
            .expect("extern function");

        assert_use(&result, ExternUseTarget::Function(id));
        assert_use_total(&result, 1);
        assert!(result.calls().is_empty());
        assert_typecheck_closed(&result);
    }

    #[test]
    fn records_core_runtime_wrapper_shape() {
        let result = check_named_with_provider(
            r#"
            import runtime { println, assert };
            fn main() {
                println("ready");
                assert(true);
                assert(true, "ok");
            }
            "#,
            &[(
                "runtime",
                r#"
                import ext:host { _println, _assert };
                pub fn println<T>(value: T) { _println(#stringify(value)); }
                pub fn assert(condition: bool, message: string = "assertion failed") {
                    _assert(condition, message);
                }
                "#,
            )],
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![],
                functions: vec![
                    function(
                        "_println",
                        vec![param("message", ExternTypeExpr::String)],
                        ExternTypeExpr::Void,
                    ),
                    function(
                        "_assert",
                        vec![
                            param("condition", ExternTypeExpr::Bool),
                            param("message", ExternTypeExpr::String),
                        ],
                        ExternTypeExpr::Void,
                    ),
                ],
            }),
        )
        .expect("typecheck failed");
        let assert_id = result
            .externs()
            .function_by_key(&function_key(provider_scope(&["host"]), "_assert"))
            .expect("runtime assert extern");

        assert_eq!(result.calls().len(), 3);
        assert_eq!(result.default_args().len(), 1);
        assert_use(&result, ExternUseTarget::Function(assert_id));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn cached_generic_specialization_restores_extern_uses() {
        let result = check(
            r#"
            extern fn tick() -> void;
            fn wrap<T>(x: T) { tick(); }
            fn main() {
                wrap(1);
                wrap("x");
                wrap(2);
            }
            "#,
        )
        .expect("typecheck failed");
        let id = result
            .externs()
            .function_by_key(&function_key(ModuleScope::Root, "tick"))
            .expect("extern function");

        assert_use(&result, ExternUseTarget::Function(id));
        assert_use_total(&result, 1);

        let body_use_count = [Type::Int, Type::String]
            .into_iter()
            .map(|ty| generic_body("wrap", vec![ty]))
            .map(|key| {
                result
                    .expect_body(&key)
                    .extern_uses
                    .values()
                    .flatten()
                    .filter(|target| **target == ExternUseTarget::Function(id))
                    .count()
            })
            .sum::<usize>();
        assert_eq!(body_use_count, 2);
        assert_typecheck_closed(&result);
    }
}

fn use_exprs(result: &TypecheckTestResult, expected: ExternUseTarget) -> Vec<ExprId> {
    result
        .extern_uses()
        .iter()
        .filter_map(|(expr_id, targets)| targets.contains(&expected).then_some(*expr_id))
        .collect()
}

fn use_count(result: &TypecheckTestResult, expected: ExternUseTarget) -> usize {
    result
        .extern_uses()
        .values()
        .flatten()
        .filter(|target| **target == expected)
        .count()
}

fn extern_use_count(result: &TypecheckTestResult) -> usize {
    result.extern_uses().values().map(Vec::len).sum()
}

fn assert_use_count(result: &TypecheckTestResult, expected: ExternUseTarget, count: usize) {
    assert_eq!(
        use_count(result, expected),
        count,
        "unexpected extern uses: {:?}",
        result.extern_uses()
    );
}

fn assert_use(result: &TypecheckTestResult, expected: ExternUseTarget) -> ExprId {
    assert_use_count(result, expected, 1);
    let exprs = use_exprs(result, expected);
    assert_eq!(
        exprs.len(),
        1,
        "target attached to multiple exprs: {exprs:?}"
    );
    exprs[0]
}

fn assert_use_total(result: &TypecheckTestResult, count: usize) {
    assert_eq!(
        extern_use_count(result),
        count,
        "unexpected extern uses: {:?}",
        result.extern_uses()
    );
}

mod any {
    use super::*;

    #[test]
    fn provider_flows_to_boundary() {
        check_with_provider(
            r"
            import ext:host { get, put };
            fn main() { put(get()); }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![],
                functions: vec![
                    function("get", vec![], ExternTypeExpr::Any),
                    function(
                        "put",
                        vec![param("value", ExternTypeExpr::Any)],
                        ExternTypeExpr::Void,
                    ),
                ],
            }),
        )
        .expect("typecheck failed");
    }

    #[test]
    fn stringify_rejects_provider_any() {
        let Err(errors) = check_with_provider(
            r"
            import ext:host { get };
            fn main() { #stringify(get()); }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![],
                functions: vec![function("get", vec![], ExternTypeExpr::Any)],
            }),
        ) else {
            panic!("typecheck should fail");
        };

        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], TypeError::ExternAnyEscape { .. }));
    }

    #[test]
    fn stringify_accepts_provider_nominal() {
        check_with_provider(
            r"
            import ext:host { Handle, get };
            fn main() { let text: string = #stringify(get()); }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![function("get", vec![], named("Handle"))],
            }),
        )
        .expect("typecheck failed");
    }
}

mod projection {
    use super::*;

    fn point_projection_provider(flow: ParamFlow) -> ProviderDescriptor {
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![extern_type("Point")],
            functions: vec![
                function("make", vec![], named("Point")),
                function(
                    "touch",
                    vec![flow_param("point", named("Point"), flow)],
                    ExternTypeExpr::Void,
                ),
            ],
        })
    }

    fn check_point_projection(flow: ParamFlow, binding: &str) -> TypecheckTestResult {
        check_with_provider(
            &format!(
                r"
                import ext:host {{ Point, make, touch }};
                struct Holder {{ @as embed point: Point }}
                fn main() {{
                    {binding} holder = Holder {{ point: make() }};
                    touch(holder);
                }}
                "
            ),
            point_projection_provider(flow),
        )
        .expect("typecheck failed")
    }

    fn assert_point_projection(result: &TypecheckTestResult) {
        let (expr_id, fact) = single_expected_projection(result);
        assert_eq!(fact.path, vec![Ident::new("point")]);
        let (_, ty) = result
            .types()
            .find(|(id, _)| **id == expr_id)
            .expect("missing projected expression type")
            .1;
        let nominal = ty.as_nominal().expect("projected extern type");

        assert_eq!(nominal.kind, NominalKind::Extern);
        assert_eq!(nominal.name, Ident::new("Point"));
    }

    #[test]
    fn extern_args_project_by_flow() {
        for (flow, binding) in [
            (ParamFlow::Value, "let"),
            (ParamFlow::Borrow, "let"),
            (ParamFlow::MutBorrow, "var"),
        ] {
            let result = check_point_projection(flow, binding);
            assert_point_projection(&result);
            assert_typecheck_closed(&result);
        }
    }

    #[test]
    fn source_extern_projected_return_records_use() {
        let result = check(
            r"
            struct Entity { x: int }
            struct Enemy { @as embed entity: Entity }
            extern fn make_enemy() -> Enemy;
            fn main() -> Entity { make_enemy() }
            ",
        )
        .expect("typecheck failed");

        let (_, fact) = single_expected_projection(&result);
        assert_eq!(fact.path, vec![Ident::new("entity")]);
        assert_eq!(fact.target_ty, nominal_struct("Entity"));

        let id = result
            .externs()
            .function_by_key(&function_key(ModuleScope::Root, "make_enemy"))
            .expect("extern function");
        assert_use(&result, ExternUseTarget::Function(id));
        assert_typecheck_closed(&result);
    }
}

mod callbacks {
    use super::*;

    #[test]
    fn provider_metadata_checks_fn_value() {
        let result = check_with_provider(
            r"
            import ext:host { apply };
            fn double(x: int) -> int { x * 2 }
            fn main() { apply(5, double); }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![],
                functions: vec![function(
                    "apply",
                    vec![
                        param("value", ExternTypeExpr::Int),
                        param(
                            "cb",
                            callback(vec![ExternTypeExpr::Int], ExternTypeExpr::Int),
                        ),
                    ],
                    ExternTypeExpr::Int,
                )],
            }),
        )
        .expect("typecheck failed");
        let id = result
            .externs()
            .function_by_key(&function_key(provider_scope(&["host"]), "apply"))
            .expect("extern function");
        let function = result.externs().function(id);
        assert!(matches!(
            function.signature.params[1].ty.ty,
            Type::Func { .. }
        ));
        assert_use(&result, ExternUseTarget::Function(id));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn provider_checks_nested_return() {
        let Err(errors) = check_with_provider(
            r#"
            import ext:host { collect };
            fn wrong(x: int) -> [string] { ["bad"] }
            fn main() { collect(wrong); }
            "#,
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![],
                functions: vec![function(
                    "collect",
                    vec![param(
                        "cb",
                        callback(
                            vec![ExternTypeExpr::Int],
                            ExternTypeExpr::List(Box::new(ExternTypeExpr::Int)),
                        ),
                    )],
                    ExternTypeExpr::Void,
                )],
            }),
        ) else {
            panic!("wrong nested callback return type should fail");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
        );
    }
}

mod fields {
    use super::*;

    #[test]
    fn records_read() {
        let result = check(
            r"
            extern type Point { x: float; }
            fn read(p: Point) -> float { p.x }
            ",
        )
        .expect("typecheck failed");
        let owner = catalog_type(&result, ModuleScope::Root, "Point");
        let field = catalog_field(&result, owner, "x");

        assert_use_count(&result, ExternUseTarget::FieldRead(field), 1);
        assert_use_count(&result, ExternUseTarget::FieldWrite(field), 0);
        assert_typecheck_closed(&result);
    }

    #[test]
    fn for_iterable_records_read() {
        let result = check(
            r"
            extern type Bag { values: [int]; }
            fn read(b: Bag) {
                for value in b.values {
                    let _: int = value;
                }
            }
            ",
        )
        .expect("typecheck failed");
        let owner = catalog_type(&result, ModuleScope::Root, "Bag");
        let field = catalog_field(&result, owner, "values");

        assert_use(&result, ExternUseTarget::FieldRead(field));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn provider_records_read() {
        let result = check_with_provider(
            r"
            import ext:host { Point };
            fn read(p: Point) -> float { p.x }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![field("x", ExternTypeExpr::Float)],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Point"))
            .expect("extern type");
        let (field, _) = result
            .externs()
            .field(owner, Ident::new("x"))
            .expect("extern field");

        assert_use(&result, ExternUseTarget::FieldRead(field));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn provider_rejects_unreadable_destructure_field() {
        let Err(errors) = check_with_provider(
            r"
            import ext:host { Point };
            fn read(p: Point) { let Point { x } = p; }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![ExternFieldDescriptor {
                        readable: false,
                        ..computed_field("x", ExternTypeExpr::Float)
                    }],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        ) else {
            panic!("unreadable destructure field should fail");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::UnknownMember { .. })),
            "unexpected errors: {errors:?}"
        );
    }

    #[test]
    fn provider_write_mut_receiver() {
        let result = check_with_provider(
            r"
            import ext:host { Point };
            fn write(ref p: Point) { p.x = 2.0; }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![field("x", ExternTypeExpr::Float)],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Point"))
            .expect("extern type");
        let (field, _) = result
            .externs()
            .field(owner, Ident::new("x"))
            .expect("extern field");

        assert_use(&result, ExternUseTarget::FieldWrite(field));
    }

    #[test]
    fn provider_rejects_immutable_receiver() {
        let Err(errors) = check_with_provider(
            r"
            import ext:host { Point };
            fn write(p: Point) { p.x = 2.0; }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![field("x", ExternTypeExpr::Float)],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        ) else {
            panic!("immutable receiver should fail");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::ImmutableAssignment { .. }))
        );
    }

    #[test]
    fn provider_writes_computed_field_mut_receiver() {
        let result = check_with_provider(
            r"
            import ext:host { Point };
            fn write(ref p: Point) { p.x = 2.0; }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![computed_field("x", ExternTypeExpr::Float)],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Point"))
            .expect("extern type");
        let (field, _) = result
            .externs()
            .field(owner, Ident::new("x"))
            .expect("extern field");

        assert_use(&result, ExternUseTarget::FieldWrite(field));
    }

    #[test]
    fn provider_rejects_unreadable_field_read() {
        let Err(errors) = check_with_provider(
            r"
            import ext:host { Point };
            fn read(p: Point) -> float { p.x }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![ExternFieldDescriptor {
                        readable: false,
                        ..computed_field("x", ExternTypeExpr::Float)
                    }],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        ) else {
            panic!("unreadable field read should fail");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::UnknownMember { .. })),
            "unexpected errors: {errors:?}"
        );
    }

    #[test]
    fn provider_rejects_unwritable_literal_override() {
        let Err(errors) = check_with_provider(
            r"
            import ext:host { Point };
            fn make() -> Point { Point { x: 2.0 } }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![ExternFieldDescriptor {
                        writable: false,
                        ..computed_field("x", ExternTypeExpr::Float)
                    }],
                    init: Some(ExternInitDescriptor {
                        params: vec![],
                        field_init: vec![],
                        presence_init: vec![],
                        ret: named("Point"),
                        effects: ExternEffects::default(),
                    }),
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        ) else {
            panic!("unwritable literal override should fail");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::ImmutableAssignment { .. })),
            "unexpected errors: {errors:?}"
        );
    }

    #[test]
    fn compound_checks_operator() {
        let Err(errors) = check(
            r#"
            extern type Box { x: float; }
            fn write(ref p: Box) { p.x *= "bad"; }
            "#,
        ) else {
            panic!("invalid compound assignment should fail");
        };

        assert!(errors.iter().any(|error| matches!(
            error,
            TypeError::InvalidOperand { .. } | TypeError::TypeMismatch { .. }
        )));
    }
}

mod struct_literals {
    use super::*;

    #[test]
    fn records_init_and_override_write() {
        let result = check(
            r"
            extern type Point rep inline { init(x: float); x: float; computed y: float; }
            fn make() -> Point { Point { x: 1.0, y: 2.0 } }
            ",
        )
        .expect("typecheck failed");
        let owner = catalog_type(&result, ModuleScope::Root, "Point");
        let y = catalog_field(&result, owner, "y");

        assert_use(&result, ExternUseTarget::Init(owner));
        assert_use(&result, ExternUseTarget::FieldWrite(y));
        assert_use_total(&result, 2);
        assert_typecheck_closed(&result);
    }

    #[test]
    fn provider_presence_init_omitted_is_valid() {
        check_with_provider(
            r"
            import ext:host { Point };
            fn make() -> Point { Point { x: 1.0 } }
            ",
            presence_provider(),
        )
        .expect("typecheck failed");
    }

    #[test]
    fn provider_presence_init_provided_is_not_an_override() {
        let result = check_with_provider(
            r"
            import ext:host { Point };
            fn make() -> Point { Point { x: 1.0, y: 2.0 } }
            ",
            presence_provider(),
        )
        .expect("typecheck failed");
        let owner = catalog_type(&result, provider_scope(&["host"]), "Point");

        assert_use(&result, ExternUseTarget::Init(owner));
        assert_use_total(&result, 1);
        assert_typecheck_closed(&result);
    }

    #[test]
    fn provider_presence_init_type_mismatch_fails() {
        let Err(errors) = check_with_provider(
            r#"
            import ext:host { Point };
            fn make() -> Point { Point { x: 1.0, y: "bad" } }
            "#,
            presence_provider(),
        ) else {
            panic!("presence field type mismatch should fail");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
        );
    }

    fn presence_provider() -> ProviderDescriptor {
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![ExternTypeDescriptor {
                fields: vec![
                    field("x", ExternTypeExpr::Float),
                    ExternFieldDescriptor {
                        writable: false,
                        ..field("y", ExternTypeExpr::Float)
                    },
                ],
                init: Some(ExternInitDescriptor {
                    params: vec![
                        param("x", ExternTypeExpr::Float),
                        param("y", ExternTypeExpr::Float),
                    ],
                    field_init: vec!["x".to_string()],
                    presence_init: vec!["y".to_string()],
                    ret: named("Point"),
                    effects: ExternEffects::default(),
                }),
                ..extern_type("Point")
            }],
            functions: vec![],
        })
    }
}

mod methods {
    use super::*;

    #[test]
    fn records_instance_call() {
        let result = check(
            r"
            extern type Point { fn len(self) -> float; }
            fn read(p: Point) -> float { p.len() }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .method(owner, Ident::new("len"))
            .expect("extern method");

        assert_use(&result, ExternUseTarget::Method(method));
        assert!(result.calls().is_empty());
        assert_typecheck_closed(&result);
    }

    #[test]
    fn records_static_call() {
        let result = check(
            r"
            extern type Point { fn origin() -> Point; }
            fn make() -> Point { Point.origin() }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .static_method(owner, Ident::new("origin"))
            .expect("extern static");

        assert_use(&result, ExternUseTarget::Static(method));
        assert!(result.calls().is_empty());
        assert_typecheck_closed(&result);
    }

    #[test]
    fn new_static_records_static() {
        let result = check(
            r"
            extern type Point {
                init;
                fn new(x: float, y: float) -> Self;
            }
            fn make() -> Point { Point.new(1.0, 2.0) }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .static_method(owner, Ident::new("new"))
            .expect("extern static");

        assert_use(&result, ExternUseTarget::Static(method));
        assert_use_count(&result, ExternUseTarget::Init(owner), 0);
        assert_use_total(&result, 1);
        assert!(result.calls().is_empty());
        assert_typecheck_closed(&result);
    }

    #[test]
    fn provider_new_returns_owner() {
        let result = check_with_provider(
            r"
            import ext:host { Point };
            fn make() -> Point { Point.new(1.0, 2.0) }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    variants: vec![],
                    init: Some(ExternInitDescriptor {
                        params: vec![],
                        field_init: vec![],
                        presence_init: vec![],
                        ret: ExternTypeExpr::Void,
                        effects: ExternEffects::default(),
                    }),
                    statics: vec![static_method(
                        "new",
                        vec![
                            param("x", ExternTypeExpr::Float),
                            param("y", ExternTypeExpr::Float),
                        ],
                        named("Point"),
                    )],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Point"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .static_method(owner, Ident::new("new"))
            .expect("extern static");

        assert_use(&result, ExternUseTarget::Static(method));
        assert_use_count(&result, ExternUseTarget::Init(owner), 0);
        assert_typecheck_closed(&result);
    }

    #[test]
    fn accepts_mut_receiver() {
        let result = check(
            r"
            extern type Point { fn move_by(ref self, dx: float); }
            fn move(ref p: Point) { p.move_by(1.0); }
            ",
        )
        .expect("typecheck failed");
        let point = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .method(point, Ident::new("move_by"))
            .expect("extern method");

        assert_use(&result, ExternUseTarget::Method(method));
        assert_use_total(&result, 1);
        assert_typecheck_closed(&result);
    }

    #[test]
    fn writes_field_receiver() {
        let result = check(
            r"
            extern type Point { fn move_by(ref self, dx: float); }
            extern type Holder { point: Point; }
            fn move(ref holder: Holder) { holder.point.move_by(1.0); }
            ",
        )
        .expect("typecheck failed");
        let point = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let holder = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Holder"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .method(point, Ident::new("move_by"))
            .expect("extern method");
        let (field, _) = result
            .externs()
            .field(holder, Ident::new("point"))
            .expect("extern field");

        assert_use_count(&result, ExternUseTarget::FieldRead(field), 0);
        let write_expr = assert_use(&result, ExternUseTarget::FieldWrite(field));
        let method_expr = assert_use(&result, ExternUseTarget::Method(method));
        assert_ne!(write_expr, method_expr);
        assert_use_total(&result, 2);
        assert_typecheck_closed(&result);
    }

    #[test]
    fn writes_nested_field_receiver() {
        let result = check(
            r"
            extern type Point { fn move_by(ref self, dx: float); }
            extern type Child { point: Point; }
            extern type Holder { child: Child; }
            fn move(ref holder: Holder) { holder.child.point.move_by(1.0); }
            ",
        )
        .expect("typecheck failed");
        let point = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let child = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Child"))
            .expect("extern type");
        let holder = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Holder"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .method(point, Ident::new("move_by"))
            .expect("extern method");
        let (child_field, _) = result
            .externs()
            .field(holder, Ident::new("child"))
            .expect("extern field");
        let (point_field, _) = result
            .externs()
            .field(child, Ident::new("point"))
            .expect("extern field");

        let read_expr = assert_use(&result, ExternUseTarget::FieldRead(child_field));
        assert_use_count(&result, ExternUseTarget::FieldRead(point_field), 0);
        let write_expr = assert_use(&result, ExternUseTarget::FieldWrite(point_field));
        let method_expr = assert_use(&result, ExternUseTarget::Method(method));
        assert_eq!(read_expr, write_expr);
        assert_ne!(write_expr, method_expr);
        assert_use_total(&result, 3);
        assert_typecheck_closed(&result);
    }

    #[test]
    fn provider_targets() {
        let result = check_with_provider(
            r"
            import ext:host { Point };
            fn len(p: Point) -> float { p.len() }
            fn make() -> Point { Point.origin() }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    methods: vec![method(
                        "len",
                        ReceiverMode::Value,
                        vec![],
                        ExternTypeExpr::Float,
                    )],
                    statics: vec![static_method("origin", vec![], named("Point"))],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Point"))
            .expect("extern type");
        let (method, _) = result
            .externs()
            .method(owner, Ident::new("len"))
            .expect("extern method");
        let (static_method, _) = result
            .externs()
            .static_method(owner, Ident::new("origin"))
            .expect("extern static");

        assert_use(&result, ExternUseTarget::Method(method));
        assert_use(&result, ExternUseTarget::Static(static_method));
        assert_typecheck_closed(&result);
    }
}

mod operators {
    use super::*;

    #[test]
    fn records_unary() {
        let result = check(
            r"
            extern type Vec2 { op - Self -> Self; }
            fn neg(v: Vec2) -> Vec2 { -v }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Vec2"))
            .expect("extern type");
        let (operator, _) = result
            .externs()
            .unary_operator(owner, UnaryOp::Neg)
            .expect("extern operator");

        assert_use(&result, ExternUseTarget::UnaryOperator(operator));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn records_left_self() {
        let result = check(
            r"
            extern type Vec2 { op Self + Self -> Self; }
            fn add(a: Vec2, b: Vec2) -> Vec2 { a + b }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Vec2"))
            .expect("extern type");
        let (operator, _) = result
            .externs()
            .binary_operator(owner, ExternBinaryOp::Add, false)
            .expect("extern operator");

        assert_use(&result, ExternUseTarget::BinaryOperator(operator));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn records_right_self() {
        let result = check(
            r"
            extern type Vec2 { op float + Self -> Self; }
            fn add(v: Vec2) -> Vec2 { 1.0 + v }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Vec2"))
            .expect("extern type");
        let (operator, _) = result
            .externs()
            .binary_operator(owner, ExternBinaryOp::Add, true)
            .expect("extern operator");

        assert_use(&result, ExternUseTarget::BinaryOperator(operator));
        assert_typecheck_closed(&result);
    }
}

mod compound {
    use super::*;

    #[test]
    fn records_operator_and_write() {
        let result = check(
            r"
            extern type Vec2 { op Self + Self -> Self; }
            extern type Holder { init; value: Vec2; }
            fn add(ref holder: Holder, rhs: Vec2) { holder.value += rhs; }
            ",
        )
        .expect("typecheck failed");
        let vec_owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Vec2"))
            .expect("extern type");
        let holder_owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Holder"))
            .expect("extern type");
        let (operator, _) = result
            .externs()
            .binary_operator(vec_owner, ExternBinaryOp::Add, false)
            .expect("extern operator");
        let (field, _) = result
            .externs()
            .field(holder_owner, Ident::new("value"))
            .expect("extern field");

        assert_use(&result, ExternUseTarget::BinaryOperator(operator));
        assert_use(&result, ExternUseTarget::FieldWrite(field));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn promoted_field_records_extern_read_and_member_path() {
        let result = check(
            r"
            extern type HostStats { hp: int; }
            struct Enemy { embed stats: HostStats }
            fn read(enemy: Enemy) -> int { enemy.hp }
            ",
        )
        .expect("typecheck failed");
        let owner = catalog_type(&result, ModuleScope::Root, "HostStats");
        let field = catalog_field(&result, owner, "hp");
        let fact = result
            .member_paths()
            .values()
            .next()
            .expect("missing member path fact");

        assert_use_count(&result, ExternUseTarget::FieldRead(field), 1);
        assert_use_count(&result, ExternUseTarget::FieldWrite(field), 0);
        assert_eq!(fact.kind, MemberPathKind::Field);
        assert_eq!(fact.path, vec![Ident::new("stats"), Ident::new("hp")]);
        assert_eq!(fact.origin_member, Ident::new("hp"));
    }

    #[test]
    fn records_field_write() {
        let result = check(
            r"
            extern type Point { x: float; }
            fn write(ref p: Point) { p.x = 1.0; }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let (field, _) = result
            .externs()
            .field(owner, Ident::new("x"))
            .expect("extern field");

        assert_use_count(&result, ExternUseTarget::FieldRead(field), 0);
        assert_use_count(&result, ExternUseTarget::FieldWrite(field), 1);
    }

    #[test]
    fn records_computed_write() {
        let result = check(
            r"
            extern type Point { computed x: float; }
            fn write(ref p: Point) { p.x = 1.0; }
            ",
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Point"))
            .expect("extern type");
        let (field, _) = result
            .externs()
            .field(owner, Ident::new("x"))
            .expect("extern field");

        assert_use_count(&result, ExternUseTarget::FieldRead(field), 0);
        assert_use_count(&result, ExternUseTarget::FieldWrite(field), 1);
        assert_use_total(&result, 1);
    }

    #[test]
    fn records_user_field_writes_through_extern_field() {
        for (body, reads, writes) in [
            ("holder.point.x = 1.0;", 0, 1),
            ("holder.point.x += 1.0;", 1, 1),
        ] {
            let result = check(&format!(
                r"
                struct Point {{ x: float }}
                extern type Holder {{ point: Point; }}
                fn write(ref holder: Holder) {{ {body} }}
                "
            ))
            .expect("typecheck failed");
            let owner = catalog_type(&result, ModuleScope::Root, "Holder");
            let field = catalog_field(&result, owner, "point");

            assert_use_count(&result, ExternUseTarget::FieldRead(field), reads);
            assert_use_count(&result, ExternUseTarget::FieldWrite(field), writes);
            assert_use_total(&result, reads + writes);
        }
    }

    #[test]
    fn records_exact_uses() {
        let result = check(
            r"
            extern type Vec2 { op Self + Self -> Self; }
            extern type Holder { value: Vec2; }
            fn add(ref holder: Holder, rhs: Vec2) { holder.value += rhs; }
            ",
        )
        .expect("typecheck failed");
        assert_compound_assignment_uses(&result);
    }

    #[test]
    fn records_computed_exact_uses() {
        let result = check(
            r"
            extern type Vec2 { op Self + Self -> Self; }
            extern type Holder { computed value: Vec2; }
            fn add(ref holder: Holder, rhs: Vec2) { holder.value += rhs; }
            ",
        )
        .expect("typecheck failed");
        assert_compound_assignment_uses(&result);
    }

    fn assert_compound_assignment_uses(result: &TypecheckTestResult) {
        let vec_owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Vec2"))
            .expect("extern type");
        let holder_owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Holder"))
            .expect("extern type");
        let (operator, _) = result
            .externs()
            .binary_operator(vec_owner, ExternBinaryOp::Add, false)
            .expect("extern operator");
        let (field, _) = result
            .externs()
            .field(holder_owner, Ident::new("value"))
            .expect("extern field");

        let read = ExternUseTarget::FieldRead(field);
        let write = ExternUseTarget::FieldWrite(field);
        let operator = ExternUseTarget::BinaryOperator(operator);
        let field_expr = assert_use(result, read);
        assert_eq!(
            assert_use(result, write),
            field_expr,
            "compound field read/write must attach to the lvalue field expression"
        );
        assert_ne!(
            assert_use(result, operator),
            field_expr,
            "compound operator target must attach to the assignment expression"
        );
        assert_use_total(result, 3);
    }

    #[test]
    fn chained_write_reads_prefix() {
        let result = check(
            r"
            extern type Child { x: float; }
            extern type Parent { child: Child; }
            fn write(ref p: Parent) { p.child.x = 1.0; }
            ",
        )
        .expect("typecheck failed");
        let child_owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Child"))
            .expect("extern type");
        let parent_owner = result
            .externs()
            .type_by_key(&type_key(ModuleScope::Root, "Parent"))
            .expect("extern type");
        let (child_field, _) = result
            .externs()
            .field(parent_owner, Ident::new("child"))
            .expect("extern field");
        let (x_field, _) = result
            .externs()
            .field(child_owner, Ident::new("x"))
            .expect("extern field");

        assert_use_count(&result, ExternUseTarget::FieldRead(child_field), 1);
        assert_use_count(&result, ExternUseTarget::FieldWrite(x_field), 1);
    }
}

mod provider_imports {
    use super::*;

    #[test]
    fn provider_records_binary() {
        let result = check_with_provider(
            r"
            import ext:host { Vec2 };
            fn add(a: Vec2, b: Vec2) -> Vec2 { a + b }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    operators: vec![operator(
                        ExternOperator::Binary {
                            op: ExternBinaryOp::Add,
                            self_on_right: false,
                        },
                        vec![param("rhs", named("Vec2"))],
                        named("Vec2"),
                    )],
                    ..extern_type("Vec2")
                }],
                functions: vec![],
            }),
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Vec2"))
            .expect("extern type");
        let (operator, _) = result
            .externs()
            .binary_operator(owner, ExternBinaryOp::Add, false)
            .expect("extern operator");

        assert_use(&result, ExternUseTarget::BinaryOperator(operator));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn exposes_provider_catalog() {
        let result = check_with_provider(
            "",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![function(
                    "tick",
                    vec![param("handle", named("Handle"))],
                    ExternTypeExpr::Void,
                )],
            }),
        )
        .expect("typecheck failed");
        let catalog = result.externs();

        assert!(
            catalog
                .function_by_key(&function_key(provider_scope(&["host"]), "tick"))
                .is_some()
        );
        assert!(
            catalog
                .type_by_key(&type_key(provider_scope(&["host"]), "Handle"))
                .is_some()
        );
        assert_typecheck_closed(&result);
    }

    #[test]
    fn share_identity() {
        let result = check_with_provider(
            r"
            import ext:host { Handle, tick };
            fn use_it(handle: Handle) { tick(handle); }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![extern_type("Handle")],
                functions: vec![function(
                    "tick",
                    vec![param("handle", named("Handle"))],
                    ExternTypeExpr::Void,
                )],
            }),
        )
        .expect("typecheck failed");

        let function_key = function_key(provider_scope(&["host"]), "tick");
        let type_key = type_key(provider_scope(&["host"]), "Handle");
        let function = result
            .externs()
            .function(result.externs().function_by_key(&function_key).unwrap());
        let ty = result
            .externs()
            .ty(result.externs().type_by_key(&type_key).unwrap());

        assert_eq!(function.key, function_key);
        assert_eq!(ty.key, type_key);
        let imported_tick = result
            .decls()
            .imported_value(&ModuleScope::Root, Ident::new("tick"))
            .expect("imported extern function");
        let callable = result
            .decls()
            .callable_for_value(&imported_tick)
            .expect("callable extern function");
        assert_eq!(
            callable.def.id,
            CallableId::extern_function(provider_scope(&["host"]), Ident::new("tick"))
        );
        assert_eq!(
            result
                .decls()
                .imported_type_binding(&ModuleScope::Root, Ident::new("Handle"))
                .and_then(typecheck::TypeBinding::into_nominal),
            Some(ty.nominal.clone())
        );
        let id = result
            .externs()
            .function_by_key(&function_key)
            .expect("provider extern function");
        assert_use(&result, ExternUseTarget::Function(id));
        assert!(result.calls().is_empty());
        assert_typecheck_closed(&result);
    }

    #[test]
    fn imported_type_members_record_uses() {
        let result = check_with_provider(
            r"
            import ext:host { Point };
            fn use_it(ref p: Point) -> float {
                let q = Point { x: 1.0, y: 2.0 };
                p.x = q.y;
                p.move_by(q);
                let r = Point.origin();
                -r + p
            }
            ",
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![ExternTypeDescriptor {
                    rep: ExternRep::Inline,
                    fields: vec![
                        field("x", ExternTypeExpr::Float),
                        field("y", ExternTypeExpr::Float),
                    ],
                    variants: vec![],
                    init: Some(ExternInitDescriptor {
                        params: vec![
                            param("x", ExternTypeExpr::Float),
                            param("y", ExternTypeExpr::Float),
                        ],
                        field_init: vec!["x".to_string(), "y".to_string()],
                        presence_init: vec![],
                        ret: ExternTypeExpr::Void,
                        effects: ExternEffects::default(),
                    }),
                    methods: vec![method(
                        "move_by",
                        ReceiverMode::Mutable,
                        vec![param("delta", named("Point"))],
                        ExternTypeExpr::Void,
                    )],
                    statics: vec![static_method("origin", vec![], named("Point"))],
                    operators: vec![
                        operator(ExternOperator::Unary(UnaryOp::Neg), vec![], named("Point")),
                        operator(
                            ExternOperator::Binary {
                                op: ExternBinaryOp::Add,
                                self_on_right: false,
                            },
                            vec![param("rhs", named("Point"))],
                            ExternTypeExpr::Float,
                        ),
                    ],
                    ..extern_type("Point")
                }],
                functions: vec![],
            }),
        )
        .expect("typecheck failed");
        let owner = catalog_type(&result, provider_scope(&["host"]), "Point");
        let x_field = catalog_field(&result, owner, "x");
        let y_field = catalog_field(&result, owner, "y");
        let method = catalog_method(&result, owner, "move_by");
        let static_method = catalog_static(&result, owner, "origin");
        let unary = result
            .externs()
            .unary_operator(owner, UnaryOp::Neg)
            .expect("extern unary")
            .0;
        let binary = catalog_binary_operator(&result, owner, ExternBinaryOp::Add);

        assert_use(&result, ExternUseTarget::Init(owner));
        assert_use(&result, ExternUseTarget::FieldRead(y_field));
        assert_use(&result, ExternUseTarget::FieldWrite(x_field));
        assert_use(&result, ExternUseTarget::Method(method));
        assert_use(&result, ExternUseTarget::Static(static_method));
        assert_use(&result, ExternUseTarget::UnaryOperator(unary));
        assert_use(&result, ExternUseTarget::BinaryOperator(binary));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn local_type_ignores_alias() {
        let Err(errors) = check_named_with_provider(
            r"
            import ext:host { take };
            import other { Vec2 };
            fn use_it(v: Vec2) { take(v); }
            ",
            &[("other", "pub struct Vec2 { x: int }")],
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![extern_type("Vec2")],
                functions: vec![function(
                    "take",
                    vec![param("v", named("Vec2"))],
                    ExternTypeExpr::Void,
                )],
            }),
        ) else {
            panic!("source import alias should not satisfy provider-local type");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::TypeMismatch { .. })),
            "unexpected errors: {errors:?}"
        );
    }

    #[test]
    fn shares_module_without_conflict() {
        let result = check_named_with_provider(
            r"
            import api { source_tick };
            import ext:api { provider_tick };
            fn use_it() { source_tick(); provider_tick(); }
            ",
            &[("api", "pub extern fn source_tick();")],
            provider(ExternModuleDescriptor {
                path: extern_path(&["api"]),
                types: vec![],
                functions: vec![function("provider_tick", vec![], ExternTypeExpr::Void)],
            }),
        )
        .expect("typecheck failed");
        let source_tick = result
            .externs()
            .function_by_key(&function_key(scope(&["api"]), "source_tick"))
            .expect("source extern function");
        let provider_tick = result
            .externs()
            .function_by_key(&function_key(provider_scope(&["api"]), "provider_tick"))
            .expect("provider extern function");

        assert_use(&result, ExternUseTarget::Function(source_tick));
        assert_use(&result, ExternUseTarget::Function(provider_tick));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn same_name_conflicts() {
        let Err(errors) = check_named_with_provider(
            "import api { tick }; import ext:api { tick }; fn use_it() { tick(); }",
            &[("api", "extern fn tick();")],
            provider(ExternModuleDescriptor {
                path: extern_path(&["api"]),
                types: vec![],
                functions: vec![function("tick", vec![], ExternTypeExpr::Void)],
            }),
        ) else {
            panic!("same-name source/provider externs should conflict");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::Decl(_))),
            "unexpected errors: {errors:?}"
        );
    }

    #[test]
    fn provider_extern_not_visible_without_import() {
        let Err(errors) = check_named_with_provider(
            "fn use_it() { tick(); }",
            &[],
            provider(ExternModuleDescriptor {
                path: extern_path(&["host"]),
                types: vec![],
                functions: vec![function("tick", vec![], ExternTypeExpr::Void)],
            }),
        ) else {
            panic!("provider extern should not be visible without import");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::UndefinedVariable { .. })),
            "unexpected errors: {errors:?}"
        );
    }
}

mod mut_borrow {
    use super::*;

    #[test]
    fn accepts_local() {
        let result = check_with_provider(
            r"
            import ext:host { touch };
            fn use_it() { var x = 1; touch(x); }
            ",
            touch_provider(vec![], ExternTypeExpr::Int),
        )
        .expect("typecheck failed");
        let id = result
            .externs()
            .function_by_key(&function_key(provider_scope(&["host"]), "touch"))
            .expect("provider extern function");

        assert_use(&result, ExternUseTarget::Function(id));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn lambda_capture_access_is_mutable() {
        let result = check_with_provider(
            r"
            import ext:host { touch };
            fn use_it() {
                var x = 1;
                let f = || { touch(x); };
                f();
            }
            ",
            touch_provider(vec![], ExternTypeExpr::Int),
        )
        .expect("typecheck failed");

        let capture = result
            .lambda_captures()
            .values()
            .find(|capture| capture.name.as_str() == "x")
            .expect("x capture fact");
        assert_eq!(capture.access, typecheck::CaptureAccess::Mutable);
    }

    #[test]
    fn accepts_direct_field() {
        let result = check_with_provider(
            r"
            import ext:host { Point, touch };
            fn use_it(ref p: Point) { touch(p.x); }
            ",
            touch_provider(
                vec![ExternTypeDescriptor {
                    fields: vec![field("x", ExternTypeExpr::Float)],
                    ..extern_type("Point")
                }],
                ExternTypeExpr::Float,
            ),
        )
        .expect("typecheck failed");
        let owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Point"))
            .expect("extern type");
        let (field, _) = result
            .externs()
            .field(owner, Ident::new("x"))
            .expect("extern field");

        assert_use(&result, ExternUseTarget::FieldWrite(field));
    }

    #[test]
    fn accepts_nested_field() {
        let result = check_with_provider(
            r"
            import ext:host { Parent, touch };
            fn use_it(ref p: Parent) { touch(p.child.x); }
            ",
            touch_provider(nested_field_types(), ExternTypeExpr::Float),
        )
        .expect("typecheck failed");
        let parent_owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Parent"))
            .expect("parent extern type");
        let child_owner = result
            .externs()
            .type_by_key(&type_key(provider_scope(&["host"]), "Child"))
            .expect("child extern type");
        let (child_field, _) = result
            .externs()
            .field(parent_owner, Ident::new("child"))
            .expect("child field");
        let (x_field, _) = result
            .externs()
            .field(child_owner, Ident::new("x"))
            .expect("x field");

        assert_use_count(&result, ExternUseTarget::FieldRead(child_field), 1);
        assert_use_count(&result, ExternUseTarget::FieldWrite(x_field), 1);
    }
}

mod named_modules {
    use super::*;

    #[test]
    fn same_provider_name_uses_imported_target() {
        let result = check_with_provider(
            r"
            import ext:right { tick };
            fn use_it() { tick(2); }
            ",
            provider_with_modules(vec![
                ExternModuleDescriptor {
                    path: extern_path(&["left"]),
                    types: vec![],
                    functions: vec![function(
                        "tick",
                        vec![param("x", ExternTypeExpr::Float)],
                        ExternTypeExpr::Void,
                    )],
                },
                ExternModuleDescriptor {
                    path: extern_path(&["right"]),
                    types: vec![],
                    functions: vec![function(
                        "tick",
                        vec![param("x", ExternTypeExpr::Int)],
                        ExternTypeExpr::Void,
                    )],
                },
            ]),
        )
        .expect("typecheck failed");
        let right = result
            .externs()
            .function_by_key(&function_key(provider_scope(&["right"]), "tick"))
            .expect("right extern function");

        assert_use(&result, ExternUseTarget::Function(right));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn absolute_type_drives_call() {
        let result = check_with_provider(
            r"
            import ext:host { take };
            import ext:math.types { Vec2 };
            fn use_it(v: Vec2) { take(v); }
            ",
            provider_with_modules(vec![
                ExternModuleDescriptor {
                    path: extern_path(&["host"]),
                    types: vec![],
                    functions: vec![function(
                        "take",
                        vec![param("v", module_named(&["math", "types"], "Vec2"))],
                        ExternTypeExpr::Void,
                    )],
                },
                ExternModuleDescriptor {
                    path: extern_path(&["math", "types"]),
                    types: vec![extern_type("Vec2")],
                    functions: vec![],
                },
            ]),
        )
        .expect("typecheck failed");

        let imported = result
            .decls()
            .imported_value(&ModuleScope::Root, Ident::new("take"))
            .expect("imported extern function");
        let callable = result
            .decls()
            .callable_for_value(&imported)
            .expect("callable extern function");

        let param = &callable.def.sig.params[0];
        let nominal = param.ty.as_nominal().expect("extern nominal param");
        assert_eq!(nominal.kind, NominalKind::Extern);
        assert_eq!(nominal.name, Ident::new("Vec2"));
        assert_eq!(
            nominal.origin,
            Some(ModuleOrigin::Provider {
                package: PackageId::synthetic_root().to_string(),
                path: vec!["math".to_string(), "types".to_string()].into(),
            })
        );
        assert_typecheck_closed(&result);
    }

    #[test]
    fn absolute_type_rejects_other_module() {
        let Err(errors) = check_with_provider(
            r"
            import ext:host { take };
            import ext:other { Vec2 as OtherVec2 };
            fn use_it(v: OtherVec2) { take(v); }
            ",
            provider_with_modules(vec![
                ExternModuleDescriptor {
                    path: extern_path(&["host"]),
                    types: vec![],
                    functions: vec![function(
                        "take",
                        vec![param("v", module_named(&["math", "types"], "Vec2"))],
                        ExternTypeExpr::Void,
                    )],
                },
                ExternModuleDescriptor {
                    path: extern_path(&["math", "types"]),
                    types: vec![extern_type("Vec2")],
                    functions: vec![],
                },
                ExternModuleDescriptor {
                    path: extern_path(&["other"]),
                    types: vec![extern_type("Vec2")],
                    functions: vec![],
                },
            ]),
        ) else {
            panic!("same-named type from another module should fail");
        };

        assert!(
            errors
                .iter()
                .any(|error| matches!(error, TypeError::TypeMismatch { .. })),
            "unexpected errors: {errors:?}"
        );
    }

    #[test]
    fn catalog() {
        let result = check_named(
            "import math { Vec2, dot }; fn use_it(v: Vec2) -> float { dot(v) }",
            &[(
                "math",
                "pub extern fn dot(v: Vec2) -> float; pub extern type Vec2;",
            )],
        )
        .expect("typecheck failed");
        let dot = result
            .externs()
            .function_by_key(&function_key(scope(&["math"]), "dot"))
            .expect("extern function");

        assert!(
            result
                .externs()
                .type_by_key(&type_key(scope(&["math"]), "Vec2"))
                .is_some()
        );
        assert_use(&result, ExternUseTarget::Function(dot));
        assert_typecheck_closed(&result);
    }

    #[test]
    fn uses_record_targets() {
        let result = check_named(
            r"
            import math { Point, use_it };
            fn main(ref p: Point) -> Point { use_it(p, 1) }
            ",
            &[(
                "math",
                r"
                extern fn tick(p: Point);
                pub extern type Point rep inline {
                    init(x: float);
                    x: float;
                    fn shift(ref self, delta: Point);
                    fn origin() -> Self;
                    op Self + Self -> Self;
                }
                pub fn use_it<T>(ref p: Point, tag: T) -> Point {
                    let q = Point { x: 1.0 };
                    p.x = q.x;
                    p.shift(q);
                    tick(p);
                    Point.origin() + p
                }
                ",
            )],
        )
        .expect("typecheck failed");
        let module = scope(&["math"]);
        let tick = result
            .externs()
            .function_by_key(&function_key(module.clone(), "tick"))
            .expect("extern function");
        let owner = catalog_type(&result, module, "Point");
        let field = catalog_field(&result, owner, "x");
        let method = catalog_method(&result, owner, "shift");
        let static_method = catalog_static(&result, owner, "origin");
        let operator = catalog_binary_operator(&result, owner, ExternBinaryOp::Add);

        assert_use(&result, ExternUseTarget::Function(tick));
        assert_use(&result, ExternUseTarget::Init(owner));
        assert_use(&result, ExternUseTarget::FieldRead(field));
        assert_use(&result, ExternUseTarget::FieldWrite(field));
        assert_use(&result, ExternUseTarget::Method(method));
        assert_use(&result, ExternUseTarget::Static(static_method));
        assert_use(&result, ExternUseTarget::BinaryOperator(operator));
        assert_typecheck_closed(&result);
    }
}
