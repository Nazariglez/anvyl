use anvyx_externs::{
    BinaryOp as ExternBinaryOp, CallbackEscape, CallbackPolicy, CallbackThread,
    ExternCallbackSignature, ExternEffects, ExternFieldDescriptor, ExternFunctionDescriptor,
    ExternInitDescriptor, ExternMethodDescriptor, ExternModuleDescriptor, ExternOperator,
    ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature, ExternStaticDescriptor,
    ExternTypeDescriptor, ExternTypeExpr, FieldAccess, ModulePath as ExternModulePath, ParamFlow,
    ProviderDescriptor, ProviderId, ReceiverMode, UnaryOp,
};

use super::support::{assert_typecheck_closed, check, check_named};
use crate::{
    ast::{ExprId, Ident, NominalKind, Program, Type},
    externs::{
        self, ExternInputs,
        catalog::{ExternCatalogContext, ExternCatalogError, ExternContextItem},
    },
    lexer::tokenize,
    parser,
    resolve::{ModuleKey, ModulePath, ResolveResult, ResolvedModule},
    typecheck::{self, ExternUseTarget, ModuleScope, TypeError},
};

fn parse(source: &str) -> Program {
    let tokens = tokenize(source).expect("tokenize failed");
    parser::parse_ast(&tokens).expect("parse failed")
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

fn module_path(path: &str) -> ModulePath {
    ModulePath::new(path.split('.').map(str::to_string).collect()).unwrap()
}

fn scope(path: &[&str]) -> ModuleScope {
    ModuleScope::Named(
        ModulePath::new(path.iter().map(|segment| (*segment).to_string()).collect()).unwrap(),
    )
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

fn field(name: &str, ty: ExternTypeExpr) -> ExternFieldDescriptor {
    access_field(name, ty, FieldAccess::ReadWrite { computed: false })
}

fn access_field(name: &str, ty: ExternTypeExpr, access: FieldAccess) -> ExternFieldDescriptor {
    ExternFieldDescriptor {
        name: name.to_string(),
        ty,
        access,
        doc: None,
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
        params,
        ret: Box::new(ret),
        policy: CallbackPolicy {
            escape: CallbackEscape::NonEscaping,
            thread: CallbackThread::SameThread,
        },
    })
}

fn resolved_modules(modules: &[(&str, &str)]) -> ResolveResult {
    ResolveResult {
        module_groups: vec![
            modules
                .iter()
                .map(|(name, source)| ResolvedModule {
                    key: ModuleKey::Named(module_path(name)),
                    program: parse(source),
                })
                .collect(),
        ],
    }
}

fn check_with_provider(
    root_source: &str,
    provider: ProviderDescriptor,
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    check_named_with_provider(root_source, &[], &[], provider)
}

fn check_named_with_provider(
    root_source: &str,
    modules: &[(&str, &str)],
    always_active: &[&str],
    provider: ProviderDescriptor,
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    let root = parse(root_source);
    let resolved = resolved_modules(modules);
    let mut raw = externs::collect_source_externs(&root, &resolved).expect("valid source externs");
    raw.append(
        externs::ingest_providers(ExternInputs {
            providers: vec![provider],
        })
        .expect("valid provider"),
    );
    let always_active_modules = always_active
        .iter()
        .map(|name| ModuleScope::Named(module_path(name)))
        .collect();
    typecheck::check_with_modules(&root, &resolved, always_active_modules, raw)
}

#[test]
fn result_exposes_source_extern_catalog() {
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
fn result_use_map_starts_empty_for_uncalled_externs() {
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

#[test]
fn source_function_call_records_use_target() {
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
fn source_function_call_rejects_wrong_arity() {
    let Err(errors) = check(
        r"
        extern fn tick(dt: float) -> void;
        fn main() { tick(); }
        ",
    ) else {
        panic!("wrong arity should fail");
    };

    assert!(matches!(
        errors.as_slice(),
        [TypeError::WrongArgCount { .. }]
    ));
}

#[test]
fn source_function_call_rejects_wrong_arg_type() {
    let Err(errors) = check(
        r#"
        extern fn take(x: int) -> void;
        fn main() { take("bad"); }
        "#,
    ) else {
        panic!("wrong argument type should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
    );
}

fn expect_type_errors(
    result: Result<typecheck::TypecheckResult, Vec<TypeError>>,
) -> Vec<TypeError> {
    let Err(errors) = result else {
        panic!("expected typecheck errors");
    };
    errors
}

fn assert_has_error(errors: &[TypeError], matches: impl Fn(&TypeError) -> bool) {
    assert!(
        errors.iter().any(matches),
        "expected matching error in {errors:?}"
    );
}

fn use_exprs(result: &typecheck::TypecheckResult, expected: ExternUseTarget) -> Vec<ExprId> {
    result
        .extern_uses()
        .iter()
        .filter_map(|(expr_id, targets)| targets.contains(&expected).then_some(*expr_id))
        .collect()
}

fn use_count(result: &typecheck::TypecheckResult, expected: ExternUseTarget) -> usize {
    result
        .extern_uses()
        .values()
        .flatten()
        .filter(|target| **target == expected)
        .count()
}

fn extern_use_count(result: &typecheck::TypecheckResult) -> usize {
    result.extern_uses().values().map(Vec::len).sum()
}

fn assert_use_count(result: &typecheck::TypecheckResult, expected: ExternUseTarget, count: usize) {
    assert_eq!(
        use_count(result, expected),
        count,
        "unexpected extern uses: {:?}",
        result.extern_uses()
    );
}

fn assert_use(result: &typecheck::TypecheckResult, expected: ExternUseTarget) -> ExprId {
    assert_use_count(result, expected, 1);
    let exprs = use_exprs(result, expected);
    assert_eq!(
        exprs.len(),
        1,
        "target attached to multiple exprs: {exprs:?}"
    );
    exprs[0]
}

fn assert_use_total(result: &typecheck::TypecheckResult, count: usize) {
    assert_eq!(
        extern_use_count(result),
        count,
        "unexpected extern uses: {:?}",
        result.extern_uses()
    );
}

#[test]
fn source_any_result_flows_to_any_boundary() {
    check(
        r"
        extern fn get() -> any;
        extern fn put(value: any);
        fn main() { put(get()); }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_any_result_rejects_bind_to_local() {
    let Err(errors) = check(
        r"
        extern fn get() -> any;
        fn main() { let value = get(); }
        ",
    ) else {
        panic!("extern any local binding should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_any_result_rejects_return_from_user_function() {
    let Err(errors) = check(
        r"
        extern fn get() -> any;
        fn main() -> int { get() }
        ",
    ) else {
        panic!("extern any return should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_any_result_rejects_pass_to_ordinary_accept_any_builtin() {
    let Err(errors) = check(
        r"
        extern fn get() -> any;
        fn main() { println(get()); }
        ",
    ) else {
        panic!("extern any builtin argument should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_any_result_rejects_hide_in_ordinary_container() {
    let Err(errors) = check(
        r"
        extern fn get() -> any;
        fn main() { let values = [get()]; }
        ",
    ) else {
        panic!("extern any container escape should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_any_result_rejects_hide_in_block_or_join() {
    let errors = expect_type_errors(check(
        r"
        extern fn get() -> any;
        fn main(cond: bool) {
            let block = { get() };
            let joined = if cond { get() } else { get() };
        }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn source_any_result_rejects_hide_in_tuple_shapes() {
    let errors = expect_type_errors(check(
        r"
        extern fn get() -> any;
        fn main() {
            let tuple = (get(), get());
            let named = (left: get(), right: get());
        }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn source_any_composite_can_flow_to_extern_any_boundary() {
    check(
        r"
        extern fn get() -> any;
        extern fn put(value: any);
        fn main(cond: bool) { put(if cond { get() } else { get() }); }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_user_function_signature_rejects_expose_any() {
    let Err(errors) = check("fn id(value: any) -> any { value }") else {
        panic!("user any signature should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_user_aggregate_rejects_expose_any() {
    let Err(errors) = check("struct Bag { value: any }") else {
        panic!("user aggregate any field should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_user_enum_rejects_expose_any() {
    let Err(errors) = check("enum Bag { Value(any) }") else {
        panic!("user enum any payload should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_any_result_rejects_pass_to_ordinary_function() {
    let errors = expect_type_errors(check(
        r"
        extern fn get() -> any;
        fn take<T>(value: T) {}
        fn main() { take(get()); }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn source_any_result_rejects_escape_via_explicit_generic_struct_literal() {
    let errors = expect_type_errors(check(
        r"
        struct Box<T> { value: T }
        extern fn get() -> any;
        fn main() { let x = Box<any> { value: get() }; }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn source_any_result_rejects_escape_via_generic_struct_literal_hint() {
    let errors = expect_type_errors(check(
        r"
        struct Box<T> { value: T }
        extern fn get() -> any;
        fn main() { let x: Box<any> = Box { value: get() }; }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn named_source_module_user_function_signature_rejects_expose_any() {
    let errors = expect_type_errors(check_named(
        "import lib { keep }; fn main() { keep(1); }",
        &[("lib", "pub fn keep(value: any) -> any { value }")],
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn named_source_module_user_aggregate_rejects_expose_any() {
    let errors = expect_type_errors(check_named(
        "import lib { Bag }; fn main(bag: Bag) {}",
        &[("lib", "pub struct Bag { value: any }")],
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn named_source_module_user_enum_rejects_expose_any() {
    let errors = expect_type_errors(check_named(
        "import lib { Bag }; fn main(bag: Bag) {}",
        &[("lib", "pub enum Bag { Value(any) }")],
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn source_any_field_result_flows_to_any_boundary() {
    check(
        r"
        extern type Box { value: any; }
        extern fn put(value: any);
        fn main(box: Box) { put(box.value); }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_any_field_result_rejects_bind_to_local() {
    let Err(errors) = check(
        r"
        extern type Box { value: any; }
        fn main(box: Box) { let value = box.value; }
        ",
    ) else {
        panic!("extern any field binding should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternAnyEscape { .. }))
    );
}

#[test]
fn source_literal_allows_extern_any_field_boundary() {
    check(
        r"
        extern fn get() -> any;
        extern type Box { init; value: any; }
        fn main() { Box { value: get() }; }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn provider_any_result_flows_to_any_boundary() {
    check_with_provider(
        r"
        import host { get, put };
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
fn source_callback_named_function_succeeds() {
    let result = check(
        r"
        extern fn apply(value: int, cb: fn(int) -> int) -> int;
        fn double(x: int) -> int { x * 2 }
        fn main() { apply(5, double); }
        ",
    )
    .expect("typecheck failed");
    let id = result
        .externs()
        .function_by_key(&function_key(ModuleScope::Root, "apply"))
        .expect("extern function");

    assert_use(&result, ExternUseTarget::Function(id));
    assert_typecheck_closed(&result);
}

#[test]
fn source_callback_function_typed_local_succeeds() {
    check(
        r"
        extern fn apply(value: int, cb: fn(int) -> int) -> int;
        fn double(x: int) -> int { x * 2 }
        fn main() {
            let cb: fn(int) -> int = double;
            apply(5, cb);
        }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_callback_extern_function_value_succeeds_without_call_use() {
    let result = check(
        r"
        extern fn transform(x: int) -> int;
        extern fn apply(value: int, cb: fn(int) -> int) -> int;
        fn main() { apply(5, transform); }
        ",
    )
    .expect("typecheck failed");
    let apply = result
        .externs()
        .function_by_key(&function_key(ModuleScope::Root, "apply"))
        .expect("extern function");
    let transform = result
        .externs()
        .function_by_key(&function_key(ModuleScope::Root, "transform"))
        .expect("extern function");

    assert_use(&result, ExternUseTarget::Function(apply));
    assert_use_count(&result, ExternUseTarget::Function(transform), 0);
    assert_use_total(&result, 1);
    assert_typecheck_closed(&result);
}

#[test]
fn source_callback_wrong_arity_fails() {
    let Err(errors) = check(
        r"
        extern fn apply(value: int, cb: fn(int) -> int) -> int;
        fn add(a: int, b: int) -> int { a + b }
        fn main() { apply(5, add); }
        ",
    ) else {
        panic!("wrong callback arity should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
    );
}

#[test]
fn source_callback_wrong_param_type_fails() {
    let Err(errors) = check(
        r"
        extern fn apply(value: int, cb: fn(int) -> int) -> int;
        fn len(s: string) -> int { 1 }
        fn main() { apply(5, len); }
        ",
    ) else {
        panic!("wrong callback parameter type should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
    );
}

#[test]
fn source_callback_wrong_return_type_fails() {
    let Err(errors) = check(
        r#"
        extern fn apply(value: int, cb: fn(int) -> int) -> int;
        fn label(x: int) -> string { "x" }
        fn main() { apply(5, label); }
        "#,
    ) else {
        panic!("wrong callback return type should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
    );
}

#[test]
fn source_callback_accepts_extern_nominal_param() {
    check(
        r"
        extern type Counter;
        extern fn with_counter(cb: fn(Counter) -> int) -> int;
        fn read(counter: Counter) -> int { 1 }
        fn main() { with_counter(read); }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn provider_callback_boundary_metadata_checks_function_value() {
    let result = check_with_provider(
        r"
        import host { apply };
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
        .function_by_key(&function_key(scope(&["host"]), "apply"))
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
fn provider_callback_return_container_checks_nested_element_type() {
    let Err(errors) = check_with_provider(
        r#"
        import host { collect };
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

#[test]
fn source_callback_closure_argument_remains_deferred_to_lambda_support() {
    let Err(errors) = check(
        r"
        extern fn apply(value: int, cb: fn(int) -> int) -> int;
        fn main() { apply(5, |x| x * 2); }
        ",
    ) else {
        panic!("closure callbacks are still blocked by lambda typechecking");
    };

    assert!(!errors.is_empty());
}

#[test]
fn source_field_read_records_use() {
    let result = check(
        r"
        extern type Point { x: float; }
        fn read(p: Point) -> float { p.x }
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

    assert_use(&result, ExternUseTarget::FieldRead(field));
    assert_typecheck_closed(&result);
}

#[test]
fn provider_field_read_records_import() {
    let result = check_with_provider(
        r"
        import host { Point };
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
        .type_by_key(&type_key(scope(&["host"]), "Point"))
        .expect("extern type");
    let (field, _) = result
        .externs()
        .field(owner, Ident::new("x"))
        .expect("extern field");

    assert_use(&result, ExternUseTarget::FieldRead(field));
    assert_typecheck_closed(&result);
}

#[test]
fn source_unknown_field_emits_one_error_for_called_member() {
    let Err(errors) = check(
        r"
        extern type Point { x: float; }
        fn read(p: Point) { p.missing(); }
        ",
    ) else {
        panic!("unknown member should fail");
    };

    assert_eq!(errors.len(), 1, "unexpected errors: {errors:?}");
    assert!(matches!(
        errors.as_slice(),
        [TypeError::UnknownMember { .. }]
    ));
}

#[test]
fn provider_field_write_accepts_mutable_receiver() {
    let result = check_with_provider(
        r"
        import host { Point };
        fn write(var p: Point) { p.x = 2.0; }
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
        .type_by_key(&type_key(scope(&["host"]), "Point"))
        .expect("extern type");
    let (field, _) = result
        .externs()
        .field(owner, Ident::new("x"))
        .expect("extern field");

    assert_use(&result, ExternUseTarget::FieldWrite(field));
}

#[test]
fn provider_field_write_rejects_immutable_receiver() {
    let Err(errors) = check_with_provider(
        r"
        import host { Point };
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
fn provider_readonly_field_rejects_write() {
    let Err(errors) = check_with_provider(
        r"
        import host { Point };
        fn write(var p: Point) { p.x = 2.0; }
        ",
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![ExternTypeDescriptor {
                fields: vec![access_field(
                    "x",
                    ExternTypeExpr::Float,
                    FieldAccess::ReadOnly { computed: false },
                )],
                ..extern_type("Point")
            }],
            functions: vec![],
        }),
    ) else {
        panic!("readonly field should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ImmutableAssignment { .. }))
    );
}

#[test]
fn source_field_compound_assignment_checks_operator() {
    let Err(errors) = check(
        r#"
        extern type Box { x: float; }
        fn write(var p: Box) { p.x *= "bad"; }
        "#,
    ) else {
        panic!("invalid compound assignment should fail");
    };

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::InvalidOperand { .. } | TypeError::TypeMismatch { .. }
    )));
}

#[test]
fn source_struct_literal_records_init() {
    let result = check(
        r"
        extern type Point { init; x: float; y: float; }
        fn make() -> Point { Point { x: 1.0, y: 2.0 } }
        ",
    )
    .expect("typecheck failed");
    let owner = result
        .externs()
        .type_by_key(&type_key(ModuleScope::Root, "Point"))
        .expect("extern type");

    assert_use(&result, ExternUseTarget::Init(owner));
    assert_use_total(&result, 1);
    assert_typecheck_closed(&result);
}

#[test]
fn source_init_params_are_rejected() {
    let errors = expect_type_errors(check(
        r"
        extern type Point { init(x: float); x: float; }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(
            error,
            TypeError::ExternCatalog(ExternCatalogError::UnsupportedInitParams { count: 1, .. })
        )
    });
}

#[test]
fn source_struct_literal_rejects_no_init() {
    let Err(errors) = check(
        r"
        extern type Point { x: float; }
        fn make() -> Point { Point { x: 1.0 } }
        ",
    ) else {
        panic!("no-init literal should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::InvalidStructLiteral { .. }))
    );
}

#[test]
fn source_struct_literal_rejects_duplicate_field() {
    let Err(errors) = check(
        r"
        extern type Point { init; x: float; }
        fn make() -> Point { Point { x: 1.0, x: 2.0 } }
        ",
    ) else {
        panic!("duplicate field should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::DuplicateField { .. }))
    );
}

#[test]
fn source_struct_literal_rejects_unknown_field() {
    let Err(errors) = check(
        r"
        extern type Point { init; x: float; }
        fn make() -> Point { Point { x: 1.0, y: 2.0 } }
        ",
    ) else {
        panic!("unknown field should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::UnknownMember { .. }))
    );
}

#[test]
fn source_struct_literal_rejects_type_mismatch() {
    let Err(errors) = check(
        r#"
        extern type Point { init; x: float; }
        fn make() -> Point { Point { x: "bad" } }
        "#,
    ) else {
        panic!("field type mismatch should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
    );
}

#[test]
fn source_struct_literal_rejects_computed_field() {
    let Err(errors) = check(
        r"
        extern type Point { init; computed x: float; }
        fn make() -> Point { Point { x: 1.0 } }
        ",
    ) else {
        panic!("computed field init should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ImmutableAssignment { .. }))
    );
}

#[test]
fn source_struct_literal_rejects_missing_field() {
    let Err(errors) = check(
        r"
        extern type Point { init; x: float; y: float; }
        fn make() -> Point { Point { x: 1.0 } }
        ",
    ) else {
        panic!("missing field should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::MissingField { .. }))
    );
}

#[test]
fn source_struct_destructure_accepts_partial_fields() {
    let result = check(
        r"
        extern type Point { init; x: float; y: float; }
        fn destructure(p: Point) { let Point { x } = p; }
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

    assert_use(&result, ExternUseTarget::FieldRead(field));
}

#[test]
fn source_any_struct_destructure_rejects_local_escape() {
    let errors = expect_type_errors(check(
        r"
        extern type Box { value: any; }
        fn destructure(box: Box) { let Box { value } = box; }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ExternAnyEscape { .. })
    });
}

#[test]
fn source_struct_destructure_rejects_duplicate_field() {
    let Err(errors) = check(
        r"
        extern type Point { init; x: float; }
        fn destructure(p: Point) { let Point { x, x } = p; }
        ",
    ) else {
        panic!("duplicate field should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::DuplicateField { .. }))
    );
}

#[test]
fn source_struct_destructure_rejects_unknown_field() {
    let Err(errors) = check(
        r"
        extern type Point { init; x: float; }
        fn destructure(p: Point) { let Point { y } = p; }
        ",
    ) else {
        panic!("unknown field should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::UnknownMember { .. }))
    );
}

#[test]
fn source_instance_method_call_records_use() {
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
fn source_static_method_call_records_use() {
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
fn source_new_static_method_records_static_not_init() {
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
fn provider_new_static_method_returns_owner_type() {
    let result = check_with_provider(
        r"
        import host { Point };
        fn make() -> Point { Point.new(1.0, 2.0) }
        ",
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![ExternTypeDescriptor {
                init: Some(ExternInitDescriptor {
                    params: vec![],
                    field_init: vec![],
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
        .type_by_key(&type_key(scope(&["host"]), "Point"))
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
fn source_method_checks_cross_type_parameter() {
    check(
        r"
        extern type Point { fn distance_to(self, other: Point) -> float; }
        fn distance(a: Point, b: Point) -> float { a.distance_to(b) }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_method_rejects_wrong_arg_count() {
    let Err(errors) = check(
        r"
        extern type Point { fn move_by(self, dx: float, dy: float); }
        fn move(p: Point) { p.move_by(1.0); }
        ",
    ) else {
        panic!("wrong arity should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::WrongArgCount { .. }))
    );
}

#[test]
fn source_method_rejects_wrong_arg_type() {
    let Err(errors) = check(
        r#"
        extern type Point { fn move_by(self, dx: float); }
        fn move(p: Point) { p.move_by("bad"); }
        "#,
    ) else {
        panic!("wrong type should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
    );
}

#[test]
fn source_mutable_method_accepts_mutable_receiver() {
    check(
        r"
        extern type Point { fn move_by(var self, dx: float); }
        fn move(var p: Point) { p.move_by(1.0); }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_mutable_method_rejects_immutable_receiver() {
    let Err(errors) = check(
        r"
        extern type Point { fn move_by(var self, dx: float); }
        fn move(p: Point) { p.move_by(1.0); }
        ",
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
fn source_mutable_method_uses_nested_place_access() {
    check(
        r"
        extern type Point { fn move_by(var self, dx: float); }
        extern type Holder { point: Point; }
        fn move(var holder: Holder) { holder.point.move_by(1.0); }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_instance_method_on_type_fails() {
    let Err(errors) = check(
        r"
        extern type Point { fn len(self) -> float; }
        fn read() -> float { Point.len() }
        ",
    ) else {
        panic!("instance method on type should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::UnknownMember { .. }))
    );
}

#[test]
fn source_static_method_on_value_fails() {
    let Err(errors) = check(
        r"
        extern type Point { fn origin() -> Point; }
        fn make(p: Point) -> Point { p.origin() }
        ",
    ) else {
        panic!("static method on value should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::UnknownMember { .. }))
    );
}

#[test]
fn provider_methods_and_statics_use_catalog_targets() {
    let result = check_with_provider(
        r"
        import host { Point };
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
        .type_by_key(&type_key(scope(&["host"]), "Point"))
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

#[test]
fn source_unary_operator_records_use() {
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
fn source_unary_operator_rejects_undeclared_operator() {
    let Err(errors) = check(
        r"
        extern type Vec2;
        fn neg(v: Vec2) -> Vec2 { -v }
        ",
    ) else {
        panic!("undeclared unary operator should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::InvalidOperand { .. }))
    );
}

#[test]
fn source_binary_operator_records_left_self() {
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
fn source_binary_operator_records_right_self() {
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

#[test]
fn source_binary_operator_rejects_wrong_other_operand_type() {
    let Err(errors) = check(
        r#"
        extern type Vec2 { op Self + float -> Self; }
        fn add(v: Vec2) -> Vec2 { v + "bad" }
        "#,
    ) else {
        panic!("wrong extern operator operand should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::InvalidOperand { .. }))
    );
}

#[test]
fn source_right_self_operator_reports_wrong_left_operand_type() {
    let errors = expect_type_errors(check(
        r#"
        extern type Vec2 { op float + Self -> Self; }
        fn add(v: Vec2) -> Vec2 { "bad" + v }
        "#,
    ));

    assert_has_error(&errors, |error| {
        matches!(
            error,
            TypeError::InvalidOperand {
                operand_type: Type::String,
                ..
            }
        )
    });
}

#[test]
fn source_equality_requires_operator_declaration() {
    let Err(errors) = check(
        r"
        extern type Vec2;
        fn eq(a: Vec2, b: Vec2) -> bool { a == b }
        ",
    ) else {
        panic!("extern equality without operator should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::InvalidOperand { .. }))
    );
}

#[test]
fn source_equality_and_comparison_return_bool() {
    check(
        r"
        extern type Vec2 {
            op Self == Self -> bool;
            op Self < Self -> bool;
        }
        fn eq(a: Vec2, b: Vec2) -> bool { a == b }
        fn lt(a: Vec2, b: Vec2) -> bool { a < b }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_binary_operator_rejects_ambiguous_cross_type_declarations() {
    let Err(errors) = check(
        r"
        extern type A { op Self + B -> A; }
        extern type B { op A + Self -> B; }
        fn add(a: A, b: B) -> A { a + b }
        ",
    ) else {
        panic!("ambiguous extern operator should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::InvalidOperand { .. }))
    );
}

#[test]
fn source_unsupported_binary_operator_does_not_record_operator_use() {
    let Err(errors) = check(
        r"
        extern type Flags;
        fn and(a: Flags, b: Flags) -> int { a & b }
        ",
    ) else {
        panic!("unsupported extern operator should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::TypeMismatch { .. }))
    );
}

#[test]
fn source_compound_assignment_records_operator_and_write() {
    let result = check(
        r"
        extern type Vec2 { op Self + Self -> Self; }
        extern type Holder { init; value: Vec2; }
        fn add(var holder: Holder, rhs: Vec2) { holder.value += rhs; }
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
fn source_binary_operator_rejects_any_operand_without_any_boundary() {
    let errors = expect_type_errors(check(
        r"
        extern fn get() -> any;
        extern type Vec2 { op Self + int -> Self; }
        fn add(v: Vec2) -> Vec2 { v + get() }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(
            error,
            TypeError::ExternAnyEscape { .. }
                | TypeError::InvalidOperand { .. }
                | TypeError::TypeMismatch { .. }
        )
    });
}

#[test]
fn source_binary_operator_accepts_any_operand_with_any_boundary() {
    check(
        r"
        extern fn get() -> any;
        extern type Vec2 { op Self + any -> Self; }
        fn add(v: Vec2) -> Vec2 { v + get() }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_compound_assignment_rejects_any_operand_without_any_boundary() {
    let errors = expect_type_errors(check(
        r"
        extern fn get() -> any;
        extern type Vec2 { op Self + int -> Self; }
        extern type Holder { value: Vec2; }
        fn add(var holder: Holder) { holder.value += get(); }
        ",
    ));

    assert_has_error(&errors, |error| {
        matches!(
            error,
            TypeError::ExternAnyEscape { .. }
                | TypeError::InvalidOperand { .. }
                | TypeError::TypeMismatch { .. }
        )
    });
}

#[test]
fn source_compound_assignment_accepts_any_operand_with_any_boundary() {
    check(
        r"
        extern fn get() -> any;
        extern type Vec2 { op Self + any -> Self; }
        extern type Holder { value: Vec2; }
        fn add(var holder: Holder) { holder.value += get(); }
        ",
    )
    .expect("typecheck failed");
}

#[test]
fn source_field_read_records_exact_target() {
    let result = check(
        r"
        extern type Point { x: float; }
        fn read(p: Point) -> float { p.x }
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

    assert_use_count(&result, ExternUseTarget::FieldRead(field), 1);
    assert_use_count(&result, ExternUseTarget::FieldWrite(field), 0);
}

#[test]
fn source_field_write_records_exact_target() {
    let result = check(
        r"
        extern type Point { x: float; }
        fn write(var p: Point) { p.x = 1.0; }
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
fn source_compound_assignment_records_exact_uses() {
    let result = check(
        r"
        extern type Vec2 { op Self + Self -> Self; }
        extern type Holder { value: Vec2; }
        fn add(var holder: Holder, rhs: Vec2) { holder.value += rhs; }
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

    let read = ExternUseTarget::FieldRead(field);
    let write = ExternUseTarget::FieldWrite(field);
    let operator = ExternUseTarget::BinaryOperator(operator);
    let field_expr = assert_use(&result, read);
    assert_eq!(
        assert_use(&result, write),
        field_expr,
        "compound field read/write must attach to the lvalue field expression"
    );
    assert_ne!(
        assert_use(&result, operator),
        field_expr,
        "compound operator target must attach to the assignment expression"
    );
    assert_use_total(&result, 3);
}

#[test]
fn source_chained_extern_field_assignment_records_prefix_read() {
    let result = check(
        r"
        extern type Child { x: float; }
        extern type Parent { child: Child; }
        fn write(var p: Parent) { p.child.x = 1.0; }
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

#[test]
fn provider_binary_operator_records_use() {
    let result = check_with_provider(
        r"
        import host { Vec2 };
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
        .type_by_key(&type_key(scope(&["host"]), "Vec2"))
        .expect("extern type");
    let (operator, _) = result
        .externs()
        .binary_operator(owner, ExternBinaryOp::Add, false)
        .expect("extern operator");

    assert_use(&result, ExternUseTarget::BinaryOperator(operator));
    assert_typecheck_closed(&result);
}

#[test]
fn result_exposes_provider_extern_catalog() {
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
            .function_by_key(&function_key(scope(&["host"]), "tick"))
            .is_some()
    );
    assert!(
        catalog
            .type_by_key(&type_key(scope(&["host"]), "Handle"))
            .is_some()
    );
    assert_typecheck_closed(&result);
}

#[test]
fn provider_imports_share_declaration_and_catalog_identity() {
    let result = check_with_provider(
        r"
        import host { Handle, tick };
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

    let function_key = function_key(scope(&["host"]), "tick");
    let type_key = type_key(scope(&["host"]), "Handle");
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
        typecheck::CallableId::extern_function(scope(&["host"]), Ident::new("tick"))
    );
    assert_eq!(
        result
            .decls()
            .imported_type(&ModuleScope::Root, Ident::new("Handle")),
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
fn provider_imported_type_members_record_uses() {
    let result = check_with_provider(
        r"
        import host { Point };
        fn use_it(var p: Point) -> float {
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
                fields: vec![
                    field("x", ExternTypeExpr::Float),
                    field("y", ExternTypeExpr::Float),
                ],
                init: Some(ExternInitDescriptor {
                    params: vec![],
                    field_init: vec![],
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
    let owner = result
        .externs()
        .type_by_key(&type_key(scope(&["host"]), "Point"))
        .expect("extern type");
    let (x_field, _) = result
        .externs()
        .field(owner, Ident::new("x"))
        .expect("extern field");
    let (y_field, _) = result
        .externs()
        .field(owner, Ident::new("y"))
        .expect("extern field");
    let (method, _) = result
        .externs()
        .method(owner, Ident::new("move_by"))
        .expect("extern method");
    let (static_method, _) = result
        .externs()
        .static_method(owner, Ident::new("origin"))
        .expect("extern static");
    let (unary, _) = result
        .externs()
        .unary_operator(owner, UnaryOp::Neg)
        .expect("extern unary");
    let (binary, _) = result
        .externs()
        .binary_operator(owner, ExternBinaryOp::Add, false)
        .expect("extern binary");

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
fn provider_local_signature_type_does_not_use_source_import_aliases() {
    let Err(errors) = check_named_with_provider(
        r"
        import host { take };
        import other { Vec2 };
        fn use_it(v: Vec2) { take(v); }
        ",
        &[("other", "pub struct Vec2 { x: int }")],
        &[],
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
fn source_and_provider_can_share_module_when_names_do_not_conflict() {
    let result = check_named_with_provider(
        r"
        import api { source_tick, provider_tick };
        fn use_it() { source_tick(); provider_tick(); }
        ",
        &[("api", "extern fn source_tick();")],
        &[],
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
        .function_by_key(&function_key(scope(&["api"]), "provider_tick"))
        .expect("provider extern function");

    assert_use(&result, ExternUseTarget::Function(source_tick));
    assert_use(&result, ExternUseTarget::Function(provider_tick));
    assert_typecheck_closed(&result);
}

#[test]
fn same_name_source_provider_conflict_is_declaration_error() {
    let Err(errors) = check_named_with_provider(
        "import api { tick }; fn use_it() { tick(); }",
        &[("api", "extern fn tick();")],
        &[],
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
fn always_active_provider_module_does_not_export_values_without_import() {
    let Err(errors) = check_named_with_provider(
        "fn use_it() { tick(); }",
        &[("host", "pub fn ordinary() -> int { 1 }")],
        &["host"],
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![],
            functions: vec![function("tick", vec![], ExternTypeExpr::Void)],
        }),
    ) else {
        panic!("always-active provider extern should not be visible without import");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::UndefinedVariable { .. })),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn provider_mut_borrow_param_accepts_mutable_local() {
    let result = check_with_provider(
        r"
        import host { touch };
        fn use_it() { var x = 1; touch(x); }
        ",
        touch_provider(vec![], ExternTypeExpr::Int),
    )
    .expect("typecheck failed");
    let id = result
        .externs()
        .function_by_key(&function_key(scope(&["host"]), "touch"))
        .expect("provider extern function");

    assert_use(&result, ExternUseTarget::Function(id));
    assert_typecheck_closed(&result);
}

#[test]
fn provider_mut_borrow_param_rejects_immutable_local() {
    let Err(errors) = check_with_provider(
        r"
        import host { touch };
        fn use_it() { let x = 1; touch(x); }
        ",
        touch_provider(vec![], ExternTypeExpr::Int),
    ) else {
        panic!("immutable argument should fail");
    };

    assert!(matches!(
        errors.as_slice(),
        [TypeError::ImmutableAssignment { .. }]
    ));
}

#[test]
fn provider_mut_borrow_param_rejects_rvalue() {
    let errors = expect_type_errors(check_with_provider(
        r"
        import host { touch };
        fn use_it() { touch(1); }
        ",
        touch_provider(vec![], ExternTypeExpr::Int),
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ImmutableAssignment { .. })
    });
}

#[test]
fn provider_mut_borrow_param_rejects_readonly_extern_field() {
    let errors = expect_type_errors(check_with_provider(
        r"
        import host { Point, touch };
        fn use_it(var p: Point) { touch(p.x); }
        ",
        touch_provider(
            vec![ExternTypeDescriptor {
                fields: vec![access_field(
                    "x",
                    ExternTypeExpr::Float,
                    FieldAccess::ReadOnly { computed: false },
                )],
                ..extern_type("Point")
            }],
            ExternTypeExpr::Float,
        ),
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ImmutableAssignment { .. })
    });
}

#[test]
fn provider_mut_borrow_param_rejects_computed_extern_field() {
    let errors = expect_type_errors(check_with_provider(
        r"
        import host { Point, touch };
        fn use_it(var p: Point) { touch(p.x); }
        ",
        touch_provider(
            vec![ExternTypeDescriptor {
                fields: vec![access_field(
                    "x",
                    ExternTypeExpr::Float,
                    FieldAccess::ReadWrite { computed: true },
                )],
                ..extern_type("Point")
            }],
            ExternTypeExpr::Float,
        ),
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ImmutableAssignment { .. })
    });
}

#[test]
fn provider_mut_borrow_param_rejects_immutable_nested_extern_field() {
    let errors = expect_type_errors(check_with_provider(
        r"
        import host { Parent, touch };
        fn use_it(p: Parent) { touch(p.child.x); }
        ",
        touch_provider(nested_field_types(), ExternTypeExpr::Float),
    ));

    assert_has_error(&errors, |error| {
        matches!(error, TypeError::ImmutableAssignment { .. })
    });
}

#[test]
fn provider_mut_borrow_param_accepts_writable_nested_extern_field() {
    let result = check_with_provider(
        r"
        import host { Parent, touch };
        fn use_it(var p: Parent) { touch(p.child.x); }
        ",
        touch_provider(nested_field_types(), ExternTypeExpr::Float),
    )
    .expect("typecheck failed");
    let parent_owner = result
        .externs()
        .type_by_key(&type_key(scope(&["host"]), "Parent"))
        .expect("parent extern type");
    let child_owner = result
        .externs()
        .type_by_key(&type_key(scope(&["host"]), "Child"))
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

#[test]
fn same_provider_function_name_uses_imported_module_target() {
    let result = check_with_provider(
        r"
        import right { tick };
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
        .function_by_key(&function_key(scope(&["right"]), "tick"))
        .expect("right extern function");

    assert_use(&result, ExternUseTarget::Function(right));
    assert_typecheck_closed(&result);
}

#[test]
fn provider_absolute_signature_type_drives_imported_call_checking() {
    let result = check_with_provider(
        r"
        import host { take };
        import math.types { Vec2 };
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
    let origin = vec!["math".to_string(), "types".to_string()];

    assert_eq!(nominal.kind, NominalKind::Extern);
    assert_eq!(nominal.name, Ident::new("Vec2"));
    assert_eq!(nominal.origin.as_deref(), Some(origin.as_slice()));
    assert_typecheck_closed(&result);
}

#[test]
fn provider_absolute_signature_type_rejects_same_named_other_module_type() {
    let Err(errors) = check_with_provider(
        r"
        import host { take };
        import other { Vec2 as OtherVec2 };
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
fn rejects_unresolved_catalog_types() {
    let Err(errors) = check_with_provider(
        "",
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![],
            functions: vec![function(
                "broken",
                vec![param("missing", named("Missing"))],
                ExternTypeExpr::Void,
            )],
        }),
    ) else {
        panic!("invalid catalog should fail");
    };

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, TypeError::ExternCatalog(_))),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn rejects_invalid_member_catalog_types() {
    let Err(errors) = check_with_provider(
        "",
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![ExternTypeDescriptor {
                methods: vec![ExternMethodDescriptor {
                    name: "move_by".to_string(),
                    doc: None,
                    receiver: ReceiverMode::Shared,
                    signature: ExternSignature {
                        params: vec![param("missing", named("MissingMethod"))],
                        ret: ExternTypeExpr::Void,
                    },
                    effects: ExternEffects::default(),
                }],
                statics: vec![ExternStaticDescriptor {
                    name: "make".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![],
                        ret: named("MissingStatic"),
                    },
                    effects: ExternEffects::default(),
                }],
                operators: vec![ExternOperatorDescriptor {
                    op: ExternOperator::Unary(UnaryOp::Neg),
                    signature: ExternSignature {
                        params: vec![],
                        ret: named("MissingOperator"),
                    },
                    effects: ExternEffects::default(),
                }],
                ..extern_type("Handle")
            }],
            functions: vec![],
        }),
    ) else {
        panic!("invalid catalog should fail");
    };

    for expected in ["MissingMethod", "MissingStatic", "MissingOperator"] {
        assert!(
            errors.iter().any(|error| matches!(
                error,
                TypeError::ExternCatalog(ExternCatalogError::UnknownType { name, .. })
                    if name.as_str() == expected
            )),
            "missing {expected} in {errors:?}"
        );
    }

    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::ExternCatalog(ExternCatalogError::UnknownType {
            context: ExternCatalogContext {
                item: ExternContextItem::Operator { .. },
                ..
            },
            name,
            ..
        }) if name.as_str() == "MissingOperator"
    )));
}

#[test]
fn named_source_module_catalog() {
    let result = check_named(
        "import math { Vec2, dot }; fn use_it(v: Vec2) -> float { dot(v) }",
        &[("math", "extern fn dot(v: Vec2) -> float; extern type Vec2;")],
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
fn named_source_module_uses_record_targets() {
    let result = check_named(
        r"
        import math { Point, use_it };
        fn main(p: Point) -> Point { use_it(p, 1) }
        ",
        &[(
            "math",
            r"
            extern fn tick(p: Point);
            extern type Point {
                init;
                x: float;
                fn shift(var self, delta: Point);
                fn origin() -> Self;
                op Self + Self -> Self;
            }
            pub fn use_it<T>(var p: Point, tag: T) -> Point {
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
    let owner = result
        .externs()
        .type_by_key(&type_key(module, "Point"))
        .expect("extern type");
    let (field, _) = result
        .externs()
        .field(owner, Ident::new("x"))
        .expect("extern field");
    let (method, _) = result
        .externs()
        .method(owner, Ident::new("shift"))
        .expect("extern method");
    let (static_method, _) = result
        .externs()
        .static_method(owner, Ident::new("origin"))
        .expect("extern static");
    let (operator, _) = result
        .externs()
        .binary_operator(owner, ExternBinaryOp::Add, false)
        .expect("extern operator");

    assert_use(&result, ExternUseTarget::Function(tick));
    assert_use(&result, ExternUseTarget::Init(owner));
    assert_use(&result, ExternUseTarget::FieldRead(field));
    assert_use(&result, ExternUseTarget::FieldWrite(field));
    assert_use(&result, ExternUseTarget::Method(method));
    assert_use(&result, ExternUseTarget::Static(static_method));
    assert_use(&result, ExternUseTarget::BinaryOperator(operator));
    assert_typecheck_closed(&result);
}
