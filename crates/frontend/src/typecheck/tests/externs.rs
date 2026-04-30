use std::collections::HashSet;

use anvyx_externs::{
    ExternEffects, ExternFunctionDescriptor, ExternInitDescriptor, ExternMethodDescriptor,
    ExternModuleDescriptor, ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep,
    ExternSignature, ExternStaticDescriptor, ExternTypeDescriptor, ExternTypeExpr,
    ModulePath as ExternModulePath, ParamFlow, ProviderDescriptor, ProviderId, ReceiverMode,
    UnaryOp,
};

use super::support::{assert_typecheck_closed, check, check_named};
use crate::{
    ast::{Ident, NominalKind, Program},
    externs::{
        self, ExternInputs,
        catalog::{ExternCatalogContext, ExternCatalogError, ExternContextItem},
    },
    lexer::tokenize,
    parser,
    resolve::{ModulePath, ResolveResult},
    typecheck::{self, ModuleScope, TypeError},
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
    ExternParam {
        name: Some(name.to_string()),
        ty,
        flow: ParamFlow::Value,
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

fn check_with_provider(
    root_source: &str,
    provider: ProviderDescriptor,
) -> Result<typecheck::TypecheckResult, Vec<TypeError>> {
    let root = parse(root_source);
    let resolved = ResolveResult {
        module_groups: vec![],
    };
    let raw = externs::ingest_providers(ExternInputs {
        providers: vec![provider],
    })
    .expect("valid provider");
    typecheck::check_with_modules(&root, &resolved, HashSet::new(), raw)
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
    let errors = match check_with_provider(
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
    ) {
        Ok(_) => panic!("same-named type from another module should fail"),
        Err(errors) => errors,
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
    let errors = match check_with_provider(
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
    ) {
        Ok(_) => panic!("invalid catalog should fail"),
        Err(errors) => errors,
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
    let errors = match check_with_provider(
        "",
        provider(ExternModuleDescriptor {
            path: extern_path(&["host"]),
            types: vec![ExternTypeDescriptor {
                init: Some(ExternInitDescriptor {
                    params: vec![param("missing", named("MissingInit"))],
                    field_init: vec![],
                }),
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
    ) {
        Ok(_) => panic!("invalid catalog should fail"),
        Err(errors) => errors,
    };

    for expected in [
        "MissingInit",
        "MissingMethod",
        "MissingStatic",
        "MissingOperator",
    ] {
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
fn named_source_module_externs_are_in_result_catalog() {
    let result = check_named(
        "import math { Vec2, dot }; fn use_it(v: Vec2) -> float { dot(v) }",
        &[("math", "extern fn dot(v: Vec2) -> float; extern type Vec2;")],
    )
    .expect("typecheck failed");

    assert!(
        result
            .externs()
            .function_by_key(&function_key(scope(&["math"]), "dot"))
            .is_some()
    );
    assert!(
        result
            .externs()
            .type_by_key(&type_key(scope(&["math"]), "Vec2"))
            .is_some()
    );
}
