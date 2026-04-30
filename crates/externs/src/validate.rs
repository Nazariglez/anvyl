use std::collections::HashSet;

use crate::{descriptor::*, keys::*};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternDescriptorError {
    InvalidName {
        kind: NameKind,
        name: String,
    },
    EmptyModulePath,
    DuplicateModule(ModulePath),
    DuplicateType {
        module: ModulePath,
        name: String,
    },
    DuplicateFunction {
        module: ModulePath,
        name: String,
    },
    DuplicateField {
        ty: ExternTypeKey,
        name: String,
    },
    DuplicateMethod {
        ty: ExternTypeKey,
        name: String,
    },
    DuplicateStatic {
        ty: ExternTypeKey,
        name: String,
    },
    DuplicateOperator {
        ty: ExternTypeKey,
        op: ExternOperator,
    },
    InvalidOperatorSignature {
        ty: ExternTypeKey,
        op: ExternOperator,
        expected_params: usize,
        actual_params: usize,
    },
    InvalidOperatorReturn {
        ty: ExternTypeKey,
        op: ExternOperator,
        expected: OperatorReturn,
        actual: ExternTypeExpr,
    },
    DuplicateFieldInit {
        ty: ExternTypeKey,
        name: String,
    },
    VoidType {
        context: TypeContext,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NameKind {
    Provider,
    ModuleSegment,
    Type,
    Function,
    Field,
    FieldInit,
    Method,
    Static,
    Param,
    NamedType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeContext {
    Param,
    Nested,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperatorReturn {
    Bool,
    NonVoid,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypePosition {
    Param,
    Return,
    Nested,
}

impl TypePosition {
    fn void_context(self) -> Option<TypeContext> {
        match self {
            TypePosition::Param => Some(TypeContext::Param),
            TypePosition::Return => None,
            TypePosition::Nested => Some(TypeContext::Nested),
        }
    }

    fn callback_param_position(self) -> Self {
        match self {
            TypePosition::Nested => TypePosition::Nested,
            TypePosition::Param | TypePosition::Return => TypePosition::Param,
        }
    }
}

pub fn validate(provider: &ProviderDescriptor) -> Result<(), Vec<ExternDescriptorError>> {
    let mut errors = vec![];
    check_name(NameKind::Provider, &provider.provider.name, &mut errors);

    let mut modules = HashSet::new();
    for module in &provider.modules {
        check_module_path(&module.path, &mut errors);
        check_unique_module(&mut modules, &module.path, &mut errors);
        check_module(module, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_module(module: &ExternModuleDescriptor, errors: &mut Vec<ExternDescriptorError>) {
    let mut types = HashSet::new();
    for ty in &module.types {
        check_unique_name(NameKind::Type, &mut types, &ty.name, errors, |name| {
            ExternDescriptorError::DuplicateType {
                module: module.path.clone(),
                name,
            }
        });
        check_type(module, ty, errors);
    }

    let mut functions = HashSet::new();
    for function in &module.functions {
        check_unique_name(
            NameKind::Function,
            &mut functions,
            &function.name,
            errors,
            |name| ExternDescriptorError::DuplicateFunction {
                module: module.path.clone(),
                name,
            },
        );
        check_signature(&function.signature, errors);
    }
}

fn check_type(
    module: &ExternModuleDescriptor,
    ty: &ExternTypeDescriptor,
    errors: &mut Vec<ExternDescriptorError>,
) {
    let key = ExternTypeKey {
        module: module.path.clone(),
        name: ty.name.clone(),
    };

    let mut fields = HashSet::new();
    for field in &ty.fields {
        check_unique_name(NameKind::Field, &mut fields, &field.name, errors, |name| {
            ExternDescriptorError::DuplicateField {
                ty: key.clone(),
                name,
            }
        });
        check_type_expr(&field.ty, TypePosition::Param, errors);
    }

    if let Some(init) = &ty.init {
        check_params(&init.params, errors);
        let mut field_inits = HashSet::new();
        for field in &init.field_init {
            check_unique_name(
                NameKind::FieldInit,
                &mut field_inits,
                field,
                errors,
                |name| ExternDescriptorError::DuplicateFieldInit {
                    ty: key.clone(),
                    name,
                },
            );
        }
    }

    let mut methods = HashSet::new();
    for method in &ty.methods {
        check_unique_name(
            NameKind::Method,
            &mut methods,
            &method.name,
            errors,
            |name| ExternDescriptorError::DuplicateMethod {
                ty: key.clone(),
                name,
            },
        );
        check_signature(&method.signature, errors);
    }

    let mut statics = HashSet::new();
    for static_method in &ty.statics {
        check_unique_name(
            NameKind::Static,
            &mut statics,
            &static_method.name,
            errors,
            |name| ExternDescriptorError::DuplicateStatic {
                ty: key.clone(),
                name,
            },
        );
        check_signature(&static_method.signature, errors);
    }

    let mut operators = HashSet::new();
    for operator in &ty.operators {
        check_unique_operator(&mut operators, &key, operator.op, errors);
        check_operator_signature(&key, operator, errors);
    }
}

fn check_unique_module(
    modules: &mut HashSet<ModulePath>,
    path: &ModulePath,
    errors: &mut Vec<ExternDescriptorError>,
) {
    if !modules.insert(path.clone()) {
        errors.push(ExternDescriptorError::DuplicateModule(path.clone()));
    }
}

fn check_unique_name(
    kind: NameKind,
    names: &mut HashSet<String>,
    name: &str,
    errors: &mut Vec<ExternDescriptorError>,
    duplicate: impl FnOnce(String) -> ExternDescriptorError,
) {
    check_name(kind, name, errors);
    if !names.insert(name.to_string()) {
        errors.push(duplicate(name.to_string()));
    }
}

fn check_unique_operator(
    operators: &mut HashSet<ExternOperator>,
    ty: &ExternTypeKey,
    op: ExternOperator,
    errors: &mut Vec<ExternDescriptorError>,
) {
    if !operators.insert(op) {
        errors.push(ExternDescriptorError::DuplicateOperator { ty: ty.clone(), op });
    }
}

fn check_operator_signature(
    ty: &ExternTypeKey,
    operator: &ExternOperatorDescriptor,
    errors: &mut Vec<ExternDescriptorError>,
) {
    check_signature(&operator.signature, errors);

    let expected_params = match operator.op {
        ExternOperator::Unary(_) => 0,
        ExternOperator::Binary { .. } => 1,
    };
    let actual_params = operator.signature.params.len();
    if actual_params != expected_params {
        errors.push(ExternDescriptorError::InvalidOperatorSignature {
            ty: ty.clone(),
            op: operator.op,
            expected_params,
            actual_params,
        });
    }

    let expected = match operator.op {
        ExternOperator::Binary { op, .. } if op.returns_bool() => OperatorReturn::Bool,
        _ => OperatorReturn::NonVoid,
    };
    if !operator_return_matches(expected, &operator.signature.ret) {
        errors.push(ExternDescriptorError::InvalidOperatorReturn {
            ty: ty.clone(),
            op: operator.op,
            expected,
            actual: operator.signature.ret.clone(),
        });
    }
}

fn operator_return_matches(expected: OperatorReturn, actual: &ExternTypeExpr) -> bool {
    match expected {
        OperatorReturn::Bool => matches!(actual, ExternTypeExpr::Bool),
        OperatorReturn::NonVoid => !matches!(actual, ExternTypeExpr::Void),
    }
}

fn check_signature(signature: &ExternSignature, errors: &mut Vec<ExternDescriptorError>) {
    check_params(&signature.params, errors);
    check_type_expr(&signature.ret, TypePosition::Return, errors);
}

fn check_params(params: &[ExternParam], errors: &mut Vec<ExternDescriptorError>) {
    for param in params {
        if let Some(name) = &param.name {
            check_name(NameKind::Param, name, errors);
        }
        check_type_expr(&param.ty, TypePosition::Param, errors);
    }
}

fn check_type_expr(
    ty: &ExternTypeExpr,
    position: TypePosition,
    errors: &mut Vec<ExternDescriptorError>,
) {
    match ty {
        ExternTypeExpr::Void => {
            if let Some(context) = position.void_context() {
                errors.push(ExternDescriptorError::VoidType { context });
            }
        }
        ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Any => {}
        ExternTypeExpr::List(item) | ExternTypeExpr::Option(item) => {
            check_type_expr(item, TypePosition::Nested, errors);
        }
        ExternTypeExpr::Map(key, value) => {
            check_type_expr(key, TypePosition::Nested, errors);
            check_type_expr(value, TypePosition::Nested, errors);
        }
        ExternTypeExpr::Named { module, name, args } => {
            if let Some(module) = module {
                check_module_path(module, errors);
            }
            check_name(NameKind::NamedType, name, errors);
            for arg in args {
                check_type_expr(arg, TypePosition::Nested, errors);
            }
        }
        ExternTypeExpr::Callback(callback) => {
            let param_position = position.callback_param_position();
            for param in &callback.params {
                check_type_expr(param, param_position, errors);
            }
            check_type_expr(&callback.ret, TypePosition::Return, errors);
        }
    }
}

fn check_module_path(path: &ModulePath, errors: &mut Vec<ExternDescriptorError>) {
    if path.segments.is_empty() {
        errors.push(ExternDescriptorError::EmptyModulePath);
    }
    for segment in &path.segments {
        check_name(NameKind::ModuleSegment, segment, errors);
    }
}

fn check_name(kind: NameKind, name: &str, errors: &mut Vec<ExternDescriptorError>) {
    if !is_valid_name(name) {
        errors.push(ExternDescriptorError::InvalidName {
            kind,
            name: name.to_string(),
        });
    }
}

fn is_valid_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first.is_alphabetic() || first == '_')
        && chars.all(|c| c.is_alphanumeric() || c == '_')
        && !name.starts_with("__")
}

#[cfg(test)]
mod tests {
    use super::*;

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
                    fields: vec![ExternFieldDescriptor {
                        name: "x".to_string(),
                        ty: ExternTypeExpr::Float,
                        access: FieldAccess::ReadWrite { computed: false },
                        doc: None,
                    }],
                    init: Some(ExternInitDescriptor {
                        params: vec![ExternParam {
                            name: Some("x".to_string()),
                            ty: ExternTypeExpr::Float,
                            flow: ParamFlow::Value,
                        }],
                        field_init: vec!["x".to_string()],
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
                                params: vec![ExternTypeExpr::Float],
                                ret: Box::new(ExternTypeExpr::Void),
                                policy: CallbackPolicy {
                                    escape: CallbackEscape::NonEscaping,
                                    thread: CallbackThread::SameThread,
                                },
                            }),
                            flow: ParamFlow::Borrow,
                        }],
                        ret: ExternTypeExpr::Void,
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        }
    }

    #[test]
    fn valid_descriptor_passes_validation() {
        assert_eq!(validate(&valid_provider()), Ok(()));
    }

    #[test]
    fn rejects_invalid_names() {
        let mut provider = valid_provider();
        provider.provider.name.clear();
        provider.modules[0].types[0].fields[0].name = "__get_x".to_string();
        provider.modules[0].functions[0].name = "2d".to_string();

        let errors = validate(&provider).unwrap_err();

        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::Provider,
            name: String::new(),
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::Field,
            name: "__get_x".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::Function,
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
        provider.modules[0].types[0].init.as_mut().unwrap().params[0].name =
            Some("__x".to_string());
        provider.modules[0].types[0].operators[0].signature.ret = ExternTypeExpr::Named {
            module: None,
            name: String::new(),
            args: vec![],
        };

        let errors = validate(&provider).unwrap_err();

        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::ModuleSegment,
            name: "bad-name".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::Type,
            name: "1Vec2".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::Param,
            name: "__x".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::NamedType,
            name: String::new(),
        }));
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
            kind: NameKind::FieldInit,
            name: "bad-field".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::Method,
            name: "2len".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::Static,
            name: "__zero".to_string(),
        }));
    }

    #[test]
    fn rejects_duplicates() {
        let mut provider = valid_provider();
        let dup_module = provider.modules[0].clone();
        let dup_type = provider.modules[0].types[0].clone();
        let function = provider.modules[0].functions[0].clone();
        let field = provider.modules[0].types[0].fields[0].clone();
        let method = provider.modules[0].types[0].methods[0].clone();
        let static_method = provider.modules[0].types[0].statics[0].clone();
        let operator = provider.modules[0].types[0].operators[0].clone();
        provider.modules.push(dup_module);
        provider.modules[0].types.push(dup_type);
        provider.modules[0].functions.push(function);
        provider.modules[0].types[0].fields.push(field);
        provider.modules[0].types[0].methods.push(method);
        provider.modules[0].types[0].statics.push(static_method);
        provider.modules[0].types[0].operators.push(operator);
        provider.modules[0].types[0]
            .init
            .as_mut()
            .unwrap()
            .field_init
            .push("x".to_string());

        let errors = validate(&provider).unwrap_err();
        let key = ty(module(&["math"]), "Vec2");

        assert!(errors.contains(&ExternDescriptorError::DuplicateModule(module(&["math"]))));
        assert!(errors.contains(&ExternDescriptorError::DuplicateType {
            module: module(&["math"]),
            name: "Vec2".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::DuplicateFunction {
            module: module(&["math"]),
            name: "visit".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::DuplicateField {
            ty: key.clone(),
            name: "x".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::DuplicateMethod {
            ty: key.clone(),
            name: "len".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::DuplicateStatic {
            ty: key.clone(),
            name: "zero".to_string(),
        }));
        assert!(errors.contains(&ExternDescriptorError::DuplicateOperator {
            ty: key.clone(),
            op: ExternOperator::Unary(UnaryOp::Neg),
        }));
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
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: None,
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
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
                signature: ExternSignature {
                    params: vec![
                        ExternParam {
                            name: None,
                            ty: ExternTypeExpr::Float,
                            flow: ParamFlow::Value,
                        },
                        ExternParam {
                            name: None,
                            ty: ExternTypeExpr::Float,
                            flow: ParamFlow::Value,
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
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: None,
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
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
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: None,
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
                    }],
                    ret: ExternTypeExpr::Named {
                        module: None,
                        name: "Vec2".to_string(),
                        args: vec![],
                    },
                },
                effects: ExternEffects::default(),
            },
            ExternOperatorDescriptor {
                op: ExternOperator::Binary {
                    op: BinaryOp::Mul,
                    self_on_right: false,
                },
                signature: ExternSignature {
                    params: vec![ExternParam {
                        name: None,
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
                    }],
                    ret: ExternTypeExpr::Void,
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
        assert!(
            errors.contains(&ExternDescriptorError::InvalidOperatorReturn {
                ty: key,
                op: ExternOperator::Binary {
                    op: BinaryOp::Mul,
                    self_on_right: false,
                },
                expected: OperatorReturn::NonVoid,
                actual: ExternTypeExpr::Void,
            })
        );
    }

    #[test]
    fn accepts_named_operator_return() {
        let mut provider = valid_provider();
        provider.modules[0].types[0].operators = vec![ExternOperatorDescriptor {
            op: ExternOperator::Binary {
                op: BinaryOp::Add,
                self_on_right: false,
            },
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Float,
                    flow: ParamFlow::Value,
                }],
                ret: ExternTypeExpr::Named {
                    module: None,
                    name: "Vec2".to_string(),
                    args: vec![],
                },
            },
            effects: ExternEffects::default(),
        }];

        assert_eq!(validate(&provider), Ok(()));
    }

    #[test]
    fn rejects_non_return_void() {
        let mut provider = valid_provider();
        provider.modules[0].functions[0].signature.params[0].ty = ExternTypeExpr::Void;
        provider.modules[0].types[0].fields[0].ty =
            ExternTypeExpr::List(Box::new(ExternTypeExpr::Void));
        provider.modules[0].types[0].operators[0].signature.ret = ExternTypeExpr::Void;

        let errors = validate(&provider).unwrap_err();

        assert!(errors.contains(&ExternDescriptorError::VoidType {
            context: TypeContext::Param,
        }));
        assert!(errors.contains(&ExternDescriptorError::VoidType {
            context: TypeContext::Nested,
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
            context: TypeContext::Nested,
        }));
    }

    #[test]
    fn checks_callback_types() {
        let mut provider = valid_provider();
        provider.modules[0].functions[0].signature.params[0].ty =
            ExternTypeExpr::Callback(ExternCallbackSignature {
                params: vec![ExternTypeExpr::Void],
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

        assert!(errors.contains(&ExternDescriptorError::VoidType {
            context: TypeContext::Param,
        }));
        assert!(errors.contains(&ExternDescriptorError::EmptyModulePath));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::NamedType,
            name: String::new(),
        }));
    }
}
