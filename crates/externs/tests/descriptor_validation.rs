use anvyx_externs::*;

fn module(segments: &[&str]) -> ModulePath {
    ModulePath {
        segments: segments
            .iter()
            .map(|segment| (*segment).to_string())
            .collect(),
    }
}

fn ty(module: ModulePath, name: &str) -> ExternTypeKey {
    ExternTypeKey {
        module,
        name: name.to_string(),
    }
}

fn empty_rust_package() -> RawProviderPackage {
    RawProviderPackage {
        exports: vec![RawProviderExport::Rust(RawRustProviderExport {
            provider: ProviderId {
                name: "host".to_string(),
            },
            modules: vec![RawRustModuleExport {
                descriptor: ExternModuleDescriptor {
                    path: module(&["host"]),
                    types: vec![],
                    functions: vec![],
                },
                types: vec![],
                bindings: vec![],
            }],
        })],
    }
}

#[test]
fn catalog_reports_only_the_first_duplicate_module() {
    let wire = serde_json::to_value(empty_rust_package()).expect("serialize package");
    let mut first = wire["exports"][0].clone();
    first["Rust"]["modules"][0]["descriptor"]["path"]["segments"][0] = serde_json::json!("first");
    let mut middle = first.clone();
    middle["Rust"]["provider"]["name"] = serde_json::json!("middle");
    let mut fresh = middle["Rust"]["modules"][0].clone();
    fresh["descriptor"]["path"]["segments"][0] = serde_json::json!("fresh");
    middle["Rust"]["modules"] = serde_json::json!([fresh, first["Rust"]["modules"][0].clone()]);
    let mut later = first.clone();
    later["Rust"]["provider"]["name"] = serde_json::json!("later");
    later["Rust"]["modules"][0]["descriptor"]["path"]["segments"][0] = serde_json::json!("fresh");
    let package = serde_json::from_value(serde_json::json!({
        "exports": [first, middle, later],
    }))
    .expect("deserialize package");

    let error = ProviderCatalog::try_new(vec![(
        ProviderPackageKey("host".to_string()),
        package,
        Some("host".to_string()),
    )])
    .expect_err("duplicate module must reject the package")
    .to_string();
    assert_eq!(error, "duplicate provider module `first` in package `host`");
    assert!(!error.contains("fresh"));
}

fn valid_provider() -> ProviderDescriptor {
    ProviderDescriptor {
        provider: ProviderId {
            name: "math".to_string(),
        },
        modules: vec![ExternModuleDescriptor {
            path: module(&["math"]),
            types: vec![ExternTypeDescriptor {
                name: "Vec2".to_string(),
                doc: None,
                rep: ExternRep::Inline,
                layout: Some(ExternLayout { size: 8, align: 8 }),
                materialization: Some(ExternMaterialization::Copy),
                owns_heap_edges: Some(false),
                fields: vec![ExternFieldDescriptor {
                    name: "x".to_string(),
                    ty: ExternTypeExpr::Float,
                    computed: false,
                    readable: true,
                    writable: true,
                    get_receiver: ReceiverMode::Shared,
                    set_receiver: ReceiverMode::Mutable,
                    doc: None,
                }],
                variants: vec![],
                init: Some(ExternInitDescriptor {
                    params: vec![ExternParam {
                        name: Some("x".to_string()),
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
                        escape: CallbackEscape::NonEscaping,
                    }],
                    field_init: vec!["x".to_string()],
                    presence_init: vec![],
                    ret: ExternTypeExpr::Void,
                    effects: ExternEffects::default(),
                }),
                methods: vec![ExternMethodDescriptor {
                    name: "len".to_string(),
                    doc: None,
                    receiver: ReceiverMode::Shared,
                    signature: ExternSignature {
                        params: vec![],
                        ret: ExternTypeExpr::Float,
                    },
                    effects: ExternEffects::default(),
                }],
                statics: vec![ExternStaticDescriptor {
                    name: "zero".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![],
                        ret: ExternTypeExpr::Named {
                            module: None,
                            name: "Vec2".to_string(),
                            args: vec![],
                        },
                    },
                    effects: ExternEffects::default(),
                }],
                operators: vec![ExternOperatorDescriptor {
                    op: ExternOperator::Unary(UnaryOp::Neg),
                    receiver: ReceiverMode::Shared,
                    signature: ExternSignature {
                        params: vec![],
                        ret: ExternTypeExpr::Named {
                            module: None,
                            name: "Vec2".to_string(),
                            args: vec![],
                        },
                    },
                    effects: ExternEffects::default(),
                }],
            }],
            functions: vec![ExternFunctionDescriptor {
                name: "visit".to_string(),
                doc: None,
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: Some("f".to_string()),
                        ty: ExternTypeExpr::Callback(ExternCallbackSignature {
                            params: vec![ExternCallbackParam {
                                ty: ExternTypeExpr::Float,
                                escape: CallbackEscape::NonEscaping,
                            }],
                            ret: Box::new(ExternTypeExpr::Void),
                            policy: CallbackPolicy {
                                escape: CallbackEscape::NonEscaping,
                                thread: CallbackThread::SameThread,
                            },
                        }),
                        flow: ParamFlow::Borrow,
                        escape: CallbackEscape::NonEscaping,
                    }],
                    ret: ExternTypeExpr::Void,
                },
                effects: ExternEffects::default(),
            }],
        }],
    }
}

#[test]
fn builds_descriptor_only_provider() {
    let descriptor = valid_provider();
    let provider = descriptor.provider.clone();
    let package = ProviderPackageKey("host".to_string());
    let catalog = ProviderCatalog::try_new(vec![(
        package.clone(),
        RawProviderPackage {
            exports: vec![RawProviderExport::Descriptor(descriptor)],
        },
        None,
    )])
    .expect("descriptor-only provider must validate");
    let key = ExternBindingKey {
        target: ExternBindingTarget::Function(ExternFunctionKey {
            module: module(&["math"]),
            name: "visit".to_string(),
        }),
        operation: ExternBindingOp::Call,
    };

    assert_eq!(
        catalog.binding(&package, &provider, &key),
        Err(ProviderCatalogError::DescriptorOnly)
    );
}

fn param(ty: ExternTypeExpr) -> ExternParam {
    ExternParam {
        name: None,
        ty,
        flow: ParamFlow::Value,
        escape: CallbackEscape::NonEscaping,
    }
}

fn function_with_signature(
    params: Vec<ExternTypeExpr>,
    ret: ExternTypeExpr,
) -> Result<(), Vec<ExternDescriptorError>> {
    let mut provider = valid_provider();
    provider.modules[0].functions[0].signature = ExternSignature {
        params: params.into_iter().map(param).collect(),
        ret,
    };
    validate(&provider)
}

#[test]
fn rejects_invalid_crate_aliases_for_empty_rust_exports() {
    for alias in ["", "not-an-ident", "type", "r#self"] {
        let result = ProviderCatalog::try_new(vec![(
            ProviderPackageKey("test".to_string()),
            empty_rust_package(),
            Some(alias.to_string()),
        )]);
        assert!(matches!(
            result,
            Err(ProviderCatalogError::InvalidCrateAlias {
                alias: rejected,
                ..
            }) if rejected == alias
        ));
    }
}

#[test]
fn rejects_final_abi_matrix() {
    let errors = function_with_signature(
        vec![ExternTypeExpr::Named {
            module: None,
            name: "Box".to_string(),
            args: vec![ExternTypeExpr::Int],
        }],
        ExternTypeExpr::Void,
    )
    .unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::InvalidAbiType {
        position: AbiPosition::ParamValue,
        reason: AbiTypeError::GenericNamedArgsUnsupported,
    }));
}

#[test]
fn rejects_invalid_names() {
    let mut provider = valid_provider();
    provider.provider.name.clear();
    provider.modules[0].types[0].fields[0].name = "__get_x".to_string();
    provider.modules[0].functions[0].name = "2d".to_string();

    let errors = validate(&provider).unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "provider",
        name: String::new(),
    }));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "field",
        name: "__get_x".to_string(),
    }));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "function",
        name: "2d".to_string(),
    }));
}

#[test]
fn rejects_invalid_module_and_type_names() {
    let mut provider = valid_provider();
    provider.modules[0].path = ModulePath {
        segments: vec!["bad-name".to_string()],
    };
    provider.modules[0].types[0].name = "1Vec2".to_string();
    provider.modules[0].functions[0].signature.params[0].name = Some("__x".to_string());
    provider.modules[0].types[0].operators[0].signature.ret = ExternTypeExpr::Named {
        module: None,
        name: String::new(),
        args: vec![],
    };

    let errors = validate(&provider).unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "module segment",
        name: "bad-name".to_string(),
    }));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "type",
        name: "1Vec2".to_string(),
    }));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "parameter",
        name: "__x".to_string(),
    }));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "named type",
        name: String::new(),
    }));
}

#[test]
fn rejects_init_param_field_shape_mismatch() {
    let mut provider = valid_provider();
    provider.modules[0].types[0]
        .init
        .as_mut()
        .unwrap()
        .field_init = vec![];

    let errors = validate(&provider).unwrap_err();

    assert!(
        errors.contains(&ExternDescriptorError::InitParamFieldCountMismatch {
            ty: ty(module(&["math"]), "Vec2"),
            params: 1,
            field_init: 0,
        })
    );
}

#[test]
fn rejects_unnamed_init_param() {
    let mut provider = valid_provider();
    provider.modules[0].types[0].init.as_mut().unwrap().params[0].name = None;

    let errors = validate(&provider).unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::UnnamedInitParam {
        ty: ty(module(&["math"]), "Vec2"),
        index: 0,
    }));
}

#[test]
fn rejects_init_param_field_name_mismatch() {
    let mut provider = valid_provider();
    provider.modules[0].types[0]
        .init
        .as_mut()
        .unwrap()
        .field_init[0] = "y".to_string();

    let errors = validate(&provider).unwrap_err();

    assert!(
        errors.contains(&ExternDescriptorError::InitParamFieldMismatch {
            ty: ty(module(&["math"]), "Vec2"),
            index: 0,
            param: "x".to_string(),
            field: "y".to_string(),
        })
    );
}

#[test]
fn rejects_empty_module_path() {
    let mut provider = valid_provider();
    provider.modules[0].path.segments.clear();

    let errors = validate(&provider).unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::EmptyModulePath));
}

#[test]
fn rejects_invalid_member_names() {
    let mut provider = valid_provider();
    provider.modules[0].types[0]
        .init
        .as_mut()
        .unwrap()
        .field_init[0] = "bad-field".to_string();
    provider.modules[0].types[0].methods[0].name = "2len".to_string();
    provider.modules[0].types[0].statics[0].name = "__zero".to_string();

    let errors = validate(&provider).unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "field initializer",
        name: "bad-field".to_string(),
    }));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "method",
        name: "2len".to_string(),
    }));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "static",
        name: "__zero".to_string(),
    }));
}

#[test]
fn rejects_duplicates() {
    let mut provider = valid_provider();
    provider.modules.push(provider.modules[0].clone());
    provider.modules[0].types[0]
        .init
        .as_mut()
        .unwrap()
        .field_init
        .push("x".to_string());

    let errors = validate(&provider).unwrap_err();
    let key = ty(module(&["math"]), "Vec2");

    assert!(errors.contains(&ExternDescriptorError::DuplicateModule(module(&["math"]))));
    assert!(errors.contains(&ExternDescriptorError::DuplicateFieldInit {
        ty: key,
        name: "x".to_string(),
    }));
}

#[test]
fn rejects_bad_operator_arity() {
    let mut provider = valid_provider();
    provider.modules[0].types[0].operators = vec![
        ExternOperatorDescriptor {
            op: ExternOperator::Unary(UnaryOp::Neg),
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Float,
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                }],
                ret: ExternTypeExpr::Float,
            },
            effects: ExternEffects::default(),
        },
        ExternOperatorDescriptor {
            op: ExternOperator::Binary {
                op: BinaryOp::Add,
                self_on_right: false,
            },
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![],
                ret: ExternTypeExpr::Float,
            },
            effects: ExternEffects::default(),
        },
        ExternOperatorDescriptor {
            op: ExternOperator::Binary {
                op: BinaryOp::Sub,
                self_on_right: false,
            },
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![
                    ExternParam {
                        name: None,
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
                        escape: CallbackEscape::NonEscaping,
                    },
                    ExternParam {
                        name: None,
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
                        escape: CallbackEscape::NonEscaping,
                    },
                ],
                ret: ExternTypeExpr::Float,
            },
            effects: ExternEffects::default(),
        },
    ];

    let errors = validate(&provider).unwrap_err();
    let key = ty(module(&["math"]), "Vec2");

    assert!(
        errors.contains(&ExternDescriptorError::InvalidOperatorSignature {
            ty: key.clone(),
            op: ExternOperator::Unary(UnaryOp::Neg),
            expected_params: 0,
            actual_params: 1,
        })
    );
    assert!(
        errors.contains(&ExternDescriptorError::InvalidOperatorSignature {
            ty: key.clone(),
            op: ExternOperator::Binary {
                op: BinaryOp::Add,
                self_on_right: false,
            },
            expected_params: 1,
            actual_params: 0,
        })
    );
    assert!(
        errors.contains(&ExternDescriptorError::InvalidOperatorSignature {
            ty: key,
            op: ExternOperator::Binary {
                op: BinaryOp::Sub,
                self_on_right: false,
            },
            expected_params: 1,
            actual_params: 2,
        })
    );
}

#[test]
fn rejects_bad_operator_return() {
    let mut provider = valid_provider();
    provider.modules[0].types[0].operators = vec![
        ExternOperatorDescriptor {
            op: ExternOperator::Binary {
                op: BinaryOp::Eq,
                self_on_right: false,
            },
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Float,
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                }],
                ret: ExternTypeExpr::Float,
            },
            effects: ExternEffects::default(),
        },
        ExternOperatorDescriptor {
            op: ExternOperator::Binary {
                op: BinaryOp::LessThan,
                self_on_right: false,
            },
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Float,
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                }],
                ret: ExternTypeExpr::Named {
                    module: None,
                    name: "Vec2".to_string(),
                    args: vec![],
                },
            },
            effects: ExternEffects::default(),
        },
    ];

    let errors = validate(&provider).unwrap_err();
    let key = ty(module(&["math"]), "Vec2");

    assert!(
        errors.contains(&ExternDescriptorError::InvalidOperatorReturn {
            ty: key.clone(),
            op: ExternOperator::Binary {
                op: BinaryOp::Eq,
                self_on_right: false,
            },
            expected: OperatorReturn::Bool,
            actual: ExternTypeExpr::Float,
        })
    );
    assert!(
        errors.contains(&ExternDescriptorError::InvalidOperatorReturn {
            ty: key.clone(),
            op: ExternOperator::Binary {
                op: BinaryOp::LessThan,
                self_on_right: false,
            },
            expected: OperatorReturn::Bool,
            actual: ExternTypeExpr::Named {
                module: None,
                name: "Vec2".to_string(),
                args: vec![],
            },
        })
    );
}

#[test]
fn rejects_non_return_void() {
    let mut provider = valid_provider();
    provider.modules[0].types[0].fields[0].ty =
        ExternTypeExpr::List(Box::new(ExternTypeExpr::Void));

    let errors = validate(&provider).unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::VoidType {
        context: "nested type position",
    }));
}

#[test]
fn rejects_invalid_named_module() {
    let mut provider = valid_provider();
    provider.modules[0].types[0].operators[0].signature.ret = ExternTypeExpr::Named {
        module: Some(ModulePath { segments: vec![] }),
        name: "Vec2".to_string(),
        args: vec![ExternTypeExpr::Void],
    };

    let errors = validate(&provider).unwrap_err();

    assert!(errors.contains(&ExternDescriptorError::EmptyModulePath));
    assert!(errors.contains(&ExternDescriptorError::VoidType {
        context: "nested type position",
    }));
}

#[test]
fn rejects_incomplete_and_shared_representation_metadata() {
    let mut provider = valid_provider();
    provider.modules[0].types[0].materialization = None;
    let key = ty(module(&["math"]), "Vec2");

    let errors = validate(&provider).unwrap_err();
    assert!(
        errors.contains(&ExternDescriptorError::InvalidRepresentationMetadata { ty: key.clone() })
    );

    let ty = &mut provider.modules[0].types[0];
    ty.rep = ExternRep::Shared;
    ty.layout = Some(ExternLayout { size: 8, align: 8 });
    ty.materialization = None;
    ty.owns_heap_edges = Some(false);
    let errors = validate(&provider).unwrap_err();
    assert!(errors.contains(&ExternDescriptorError::InvalidRepresentationMetadata { ty: key }));
}

#[test]
fn rejects_invalid_inline_layout() {
    let key = ty(module(&["math"]), "Vec2");
    for (size, align) in [(8, 0), (8, 3), (9, 8)] {
        let mut provider = valid_provider();
        provider.modules[0].types[0].layout = Some(ExternLayout { size, align });

        let errors = validate(&provider).unwrap_err();
        assert!(errors.contains(&ExternDescriptorError::InvalidLayout {
            ty: key.clone(),
            size,
            align,
        }));
    }
}

#[test]
fn checks_callback_types() {
    let mut provider = valid_provider();
    provider.modules[0].functions[0].signature.params[0].ty =
        ExternTypeExpr::Callback(ExternCallbackSignature {
            params: vec![ExternCallbackParam {
                ty: ExternTypeExpr::Void,
                escape: CallbackEscape::NonEscaping,
            }],
            ret: Box::new(ExternTypeExpr::Named {
                module: Some(ModulePath { segments: vec![] }),
                name: String::new(),
                args: vec![],
            }),
            policy: CallbackPolicy {
                escape: CallbackEscape::Escaping,
                thread: CallbackThread::SameThread,
            },
        });

    let errors = validate(&provider).unwrap_err();

    assert!(
        errors.contains(&ExternDescriptorError::CallbackEscapeMismatch {
            param: Some("f".to_string()),
            param_escape: CallbackEscape::NonEscaping,
            policy_escape: CallbackEscape::Escaping,
        })
    );
    assert!(errors.contains(&ExternDescriptorError::VoidType {
        context: "parameter position",
    }));
    assert!(errors.contains(&ExternDescriptorError::EmptyModulePath));
    assert!(errors.contains(&ExternDescriptorError::InvalidName {
        kind: "named type",
        name: String::new(),
    }));
}
#[test]
fn displays_final_abi_shapes() {
    let callback = ExternCallbackSignature {
        params: vec![],
        ret: Box::new(ExternTypeExpr::Void),
        policy: CallbackPolicy {
            escape: CallbackEscape::NonEscaping,
            thread: CallbackThread::SameThread,
        },
    };
    let cases = [
        (ExternTypeExpr::Void, "void"),
        (ExternTypeExpr::Unit, "()"),
        (ExternTypeExpr::Bool, "bool"),
        (ExternTypeExpr::Int, "int"),
        (ExternTypeExpr::Float, "float"),
        (ExternTypeExpr::String, "string"),
        (ExternTypeExpr::Char, "char"),
        (ExternTypeExpr::Any, "any"),
        (ExternTypeExpr::option(ExternTypeExpr::Int), "int?"),
        (
            ExternTypeExpr::result(
                ExternTypeExpr::String,
                ExternTypeExpr::named(None, "LoadError"),
            ),
            "Result<string, LoadError>",
        ),
        (
            ExternTypeExpr::Tuple(vec![ExternTypeExpr::Int, ExternTypeExpr::Float]),
            "(int, float)",
        ),
        (ExternTypeExpr::Tuple(vec![ExternTypeExpr::Bool]), "(bool,)"),
        (
            ExternTypeExpr::array(ExternTypeExpr::Float, 4),
            "[float; 4]",
        ),
        (ExternTypeExpr::list(ExternTypeExpr::Int), "[int]"),
        (
            ExternTypeExpr::map(ExternTypeExpr::String, ExternTypeExpr::Bool),
            "[string: bool]",
        ),
        (ExternTypeExpr::slice(ExternTypeExpr::Int), "slice[int]"),
        (
            ExternTypeExpr::Named {
                module: Some(ModulePath {
                    segments: vec!["math".to_string()],
                }),
                name: "Vec2".to_string(),
                args: vec![ExternTypeExpr::Float],
            },
            "math.Vec2<float>",
        ),
        (ExternTypeExpr::Callback(callback), "callback"),
    ];

    for (ty, expected) in cases {
        assert_eq!(ty.to_string(), expected);
    }
}

#[test]
fn operators_report_return_requirements() {
    for op in [
        BinaryOp::Eq,
        BinaryOp::NotEq,
        BinaryOp::LessThan,
        BinaryOp::GreaterThan,
        BinaryOp::LessThanEq,
        BinaryOp::GreaterThanEq,
    ] {
        assert_eq!(
            ExternOperator::Binary {
                op,
                self_on_right: false,
            }
            .return_requirement(),
            OperatorReturn::Bool
        );
    }

    for op in [
        BinaryOp::Add,
        BinaryOp::Sub,
        BinaryOp::Mul,
        BinaryOp::Div,
        BinaryOp::Rem,
    ] {
        assert_eq!(
            ExternOperator::Binary {
                op,
                self_on_right: false,
            }
            .return_requirement(),
            OperatorReturn::NonVoid
        );
    }

    assert_eq!(
        ExternOperator::Unary(UnaryOp::Neg).return_requirement(),
        OperatorReturn::NonVoid
    );
}
