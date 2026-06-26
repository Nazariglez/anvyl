use std::{collections::HashSet, fmt};

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
    UnsupportedInitParams {
        ty: ExternTypeKey,
        count: usize,
    },
    MixedVariantFields {
        ty: ExternTypeKey,
        variant: String,
    },
    VoidType {
        context: TypeContext,
    },
    InvalidAbiType {
        position: AbiPosition,
        reason: AbiTypeError,
    },
    CallbackEscapeMismatch {
        param: Option<String>,
        param_escape: CallbackEscape,
        policy_escape: CallbackEscape,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValidationMode {
    Provider,
    SourceModule,
}

impl fmt::Display for NameKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Provider => "provider",
            Self::ModuleSegment => "module segment",
            Self::Type => "type",
            Self::Function => "function",
            Self::Field => "field",
            Self::FieldInit => "field initializer",
            Self::Method => "method",
            Self::Static => "static",
            Self::Param => "parameter",
            Self::NamedType => "named type",
        })
    }
}

impl fmt::Display for TypeContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Param => "parameter position",
            Self::Nested => "nested type position",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypePosition {
    Param(ParamFlow),
    Return,
    Field,
}

impl TypePosition {
    fn abi_position(self) -> AbiPosition {
        match self {
            TypePosition::Param(ParamFlow::Value) => AbiPosition::ParamValue,
            TypePosition::Param(ParamFlow::Borrow) => AbiPosition::ParamBorrow,
            TypePosition::Param(ParamFlow::MutBorrow) => AbiPosition::ParamMutBorrow,
            TypePosition::Return => AbiPosition::Return,
            TypePosition::Field => AbiPosition::Field,
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
        check_module(module, ValidationMode::Provider, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn validate_module_contents(
    module: &ExternModuleDescriptor,
) -> Result<(), Vec<ExternDescriptorError>> {
    let mut errors = vec![];
    check_module(module, ValidationMode::SourceModule, &mut errors);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn check_module(
    module: &ExternModuleDescriptor,
    mode: ValidationMode,
    errors: &mut Vec<ExternDescriptorError>,
) {
    let mut types = HashSet::new();
    for ty in &module.types {
        check_unique_name(NameKind::Type, &mut types, &ty.name, errors, |name| {
            ExternDescriptorError::DuplicateType {
                module: module.path.clone(),
                name,
            }
        });
        check_type(module, ty, mode, errors);
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
        check_signature(&function.signature, mode, errors);
    }
}

fn check_type(
    module: &ExternModuleDescriptor,
    ty: &ExternTypeDescriptor,
    mode: ValidationMode,
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
        check_type_expr(&field.ty, TypePosition::Field, mode, errors);
    }

    let mut variants = HashSet::new();
    for variant in &ty.variants {
        check_unique_name(
            NameKind::Field,
            &mut variants,
            &variant.name,
            errors,
            |name| ExternDescriptorError::DuplicateField {
                ty: key.clone(),
                name,
            },
        );
        let mut fields = HashSet::new();
        let has_named = variant.fields.iter().any(|field| field.name.is_some());
        let has_unnamed = variant.fields.iter().any(|field| field.name.is_none());
        if has_named && has_unnamed {
            errors.push(ExternDescriptorError::MixedVariantFields {
                ty: key.clone(),
                variant: variant.name.clone(),
            });
        }
        for field in &variant.fields {
            if let Some(name) = &field.name {
                check_unique_name(NameKind::Field, &mut fields, name, errors, |name| {
                    ExternDescriptorError::DuplicateField {
                        ty: key.clone(),
                        name,
                    }
                });
            }
            check_type_expr(&field.ty, TypePosition::Field, mode, errors);
        }
    }

    if let Some(init) = &ty.init {
        if mode == ValidationMode::SourceModule && !init.params.is_empty() {
            errors.push(ExternDescriptorError::UnsupportedInitParams {
                ty: key.clone(),
                count: init.params.len(),
            });
        }
        check_signature(
            &ExternSignature {
                params: init.params.clone(),
                ret: init.ret.clone(),
            },
            mode,
            errors,
        );
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
        check_signature(&method.signature, mode, errors);
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
        check_signature(&static_method.signature, mode, errors);
    }

    let mut operators = HashSet::new();
    for operator in &ty.operators {
        check_unique_operator(&mut operators, &key, operator.op, errors);
        check_operator_signature(&key, operator, mode, errors);
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
    mode: ValidationMode,
    errors: &mut Vec<ExternDescriptorError>,
) {
    check_signature(&operator.signature, mode, errors);

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

    let expected = operator.op.return_requirement();
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

fn check_signature(
    signature: &ExternSignature,
    mode: ValidationMode,
    errors: &mut Vec<ExternDescriptorError>,
) {
    check_params(&signature.params, mode, errors);
    check_type_expr(&signature.ret, TypePosition::Return, mode, errors);
}

fn check_params(
    params: &[ExternParam],
    mode: ValidationMode,
    errors: &mut Vec<ExternDescriptorError>,
) {
    for param in params {
        if let Some(name) = &param.name {
            check_name(NameKind::Param, name, errors);
        }
        check_callback_escape(param.name.as_deref(), param.escape, &param.ty, errors);
        check_type_expr(&param.ty, TypePosition::Param(param.flow), mode, errors);
    }
}

fn check_callback_escape(
    param: Option<&str>,
    escape: CallbackEscape,
    ty: &ExternTypeExpr,
    errors: &mut Vec<ExternDescriptorError>,
) {
    let ExternTypeExpr::Callback(callback) = ty else {
        return;
    };
    if let Err(mismatch) = effective_callback_escape(escape, callback) {
        errors.push(ExternDescriptorError::CallbackEscapeMismatch {
            param: param.map(str::to_string),
            param_escape: mismatch.param_escape,
            policy_escape: mismatch.policy_escape,
        });
    }
}

fn check_type_expr(
    ty: &ExternTypeExpr,
    position: TypePosition,
    mode: ValidationMode,
    errors: &mut Vec<ExternDescriptorError>,
) {
    if let Err(violations) = ty.classify_abi(position.abi_position()) {
        for violation in violations {
            if mode == ValidationMode::SourceModule
                && violation.reason == AbiTypeError::GenericNamedArgsUnsupported
            {
                continue;
            }
            push_abi_violation(violation, errors);
        }
    }
    check_type_names(ty, errors);
}

fn push_abi_violation(violation: AbiTypeViolation, errors: &mut Vec<ExternDescriptorError>) {
    if violation.reason == AbiTypeError::VoidOutsideReturn {
        errors.push(ExternDescriptorError::VoidType {
            context: void_type_context(violation.position),
        });
    } else {
        errors.push(ExternDescriptorError::InvalidAbiType {
            position: violation.position,
            reason: violation.reason,
        });
    }
}

fn void_type_context(position: AbiPosition) -> TypeContext {
    match position {
        AbiPosition::ParamValue
        | AbiPosition::ParamBorrow
        | AbiPosition::ParamMutBorrow
        | AbiPosition::CallbackParam => TypeContext::Param,
        AbiPosition::Return
        | AbiPosition::CallbackReturn
        | AbiPosition::Field
        | AbiPosition::Nested => TypeContext::Nested,
    }
}

fn check_type_names(ty: &ExternTypeExpr, errors: &mut Vec<ExternDescriptorError>) {
    match ty {
        ExternTypeExpr::Void
        | ExternTypeExpr::Unit
        | ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Any => {}
        ExternTypeExpr::List(item)
        | ExternTypeExpr::Option(item)
        | ExternTypeExpr::Array { elem: item, .. }
        | ExternTypeExpr::Slice(item) => check_type_names(item, errors),
        ExternTypeExpr::Map(key, value) | ExternTypeExpr::Result(key, value) => {
            check_type_names(key, errors);
            check_type_names(value, errors);
        }
        ExternTypeExpr::Tuple(fields) => {
            for field in fields {
                check_type_names(field, errors);
            }
        }
        ExternTypeExpr::Named { module, name, args } => {
            if let Some(module) = module {
                check_module_path(module, errors);
            }
            check_name(NameKind::NamedType, name, errors);
            for arg in args {
                check_type_names(arg, errors);
            }
        }
        ExternTypeExpr::Callback(callback) => {
            for param in &callback.params {
                check_callback_escape(None, param.escape, &param.ty, errors);
                check_type_names(&param.ty, errors);
            }
            check_type_names(&callback.ret, errors);
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
    if !is_valid_extern_name(name) {
        errors.push(ExternDescriptorError::InvalidName {
            kind,
            name: name.to_string(),
        });
    }
}

fn is_valid_extern_name(name: &str) -> bool {
    is_identifier_shaped(name) && !name.starts_with("__")
}

fn is_identifier_shaped(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first.is_alphabetic() || first == '_') && chars.all(|c| c.is_alphanumeric() || c == '_')
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
                        computed: false,
                        readable: true,
                        writable: true,
                        get_receiver: ReceiverMode::Shared,
                        set_receiver: ReceiverMode::Mutable,
                        doc: None,
                    }],
                    variants: vec![],
                    init: Some(ExternInitDescriptor {
                        params: vec![],
                        field_init: vec!["x".to_string()],
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

    fn same_thread_callback(ret: ExternTypeExpr) -> ExternTypeExpr {
        ExternTypeExpr::Callback(ExternCallbackSignature {
            params: vec![ExternCallbackParam {
                ty: ExternTypeExpr::Int,
                escape: CallbackEscape::NonEscaping,
            }],
            ret: Box::new(ret),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
                thread: CallbackThread::SameThread,
            },
        })
    }

    #[test]
    fn valid_descriptor_passes_validation() {
        assert_eq!(validate(&valid_provider()), Ok(()));
    }

    #[test]
    fn accepts_final_abi_matrix() {
        let cases = [
            (
                vec![
                    ExternTypeExpr::Unit,
                    ExternTypeExpr::Bool,
                    ExternTypeExpr::Int,
                    ExternTypeExpr::Float,
                    ExternTypeExpr::String,
                    ExternTypeExpr::Any,
                ],
                ExternTypeExpr::Unit,
            ),
            (
                vec![ExternTypeExpr::option(ExternTypeExpr::result(
                    ExternTypeExpr::Unit,
                    ExternTypeExpr::named(None, "LoadError"),
                ))],
                ExternTypeExpr::result(
                    ExternTypeExpr::list(ExternTypeExpr::Int),
                    ExternTypeExpr::named(None, "LoadError"),
                ),
            ),
            (
                vec![ExternTypeExpr::Tuple(vec![
                    ExternTypeExpr::array(ExternTypeExpr::Float, 4),
                    ExternTypeExpr::list(ExternTypeExpr::String),
                    ExternTypeExpr::map(ExternTypeExpr::String, ExternTypeExpr::Int),
                ])],
                ExternTypeExpr::Tuple(vec![ExternTypeExpr::Int, ExternTypeExpr::Bool]),
            ),
            (
                vec![ExternTypeExpr::slice(ExternTypeExpr::Int)],
                ExternTypeExpr::Void,
            ),
            (
                vec![same_thread_callback(ExternTypeExpr::Int)],
                ExternTypeExpr::Void,
            ),
        ];

        for (params, ret) in cases {
            assert_eq!(function_with_signature(params, ret), Ok(()));
        }
    }

    #[test]
    fn rejects_final_abi_matrix() {
        let callback = same_thread_callback(ExternTypeExpr::Void);
        let cases = [
            (
                vec![],
                ExternTypeExpr::slice(ExternTypeExpr::Int),
                ExternDescriptorError::InvalidAbiType {
                    position: AbiPosition::Return,
                    reason: AbiTypeError::SliceOutsideParam,
                },
            ),
            (
                vec![ExternTypeExpr::list(ExternTypeExpr::slice(
                    ExternTypeExpr::Int,
                ))],
                ExternTypeExpr::Void,
                ExternDescriptorError::InvalidAbiType {
                    position: AbiPosition::Nested,
                    reason: AbiTypeError::SliceNested,
                },
            ),
            (
                vec![],
                callback.clone(),
                ExternDescriptorError::InvalidAbiType {
                    position: AbiPosition::Return,
                    reason: AbiTypeError::CallbackOutsideParam,
                },
            ),
            (
                vec![ExternTypeExpr::list(callback)],
                ExternTypeExpr::Void,
                ExternDescriptorError::InvalidAbiType {
                    position: AbiPosition::Nested,
                    reason: AbiTypeError::CallbackNested,
                },
            ),
            (
                vec![ExternTypeExpr::Named {
                    module: None,
                    name: "Box".to_string(),
                    args: vec![ExternTypeExpr::Int],
                }],
                ExternTypeExpr::Void,
                ExternDescriptorError::InvalidAbiType {
                    position: AbiPosition::ParamValue,
                    reason: AbiTypeError::GenericNamedArgsUnsupported,
                },
            ),
            (
                vec![ExternTypeExpr::Void],
                ExternTypeExpr::Void,
                ExternDescriptorError::VoidType {
                    context: TypeContext::Param,
                },
            ),
        ];

        for (params, ret, expected) in cases {
            let errors = function_with_signature(params, ret).unwrap_err();
            assert!(
                errors.contains(&expected),
                "missing {expected:?} in {errors:?}"
            );
        }
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
        provider.modules[0].functions[0].signature.params[0].name = Some("__x".to_string());
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
    fn rejects_source_module_init_params() {
        let mut provider = valid_provider();
        provider.modules[0].types[0].init.as_mut().unwrap().params = vec![ExternParam {
            name: Some("x".to_string()),
            ty: ExternTypeExpr::Float,
            flow: ParamFlow::Value,
            escape: CallbackEscape::NonEscaping,
        }];

        let errors = validate_module_contents(&provider.modules[0]).unwrap_err();

        assert!(
            errors.contains(&ExternDescriptorError::UnsupportedInitParams {
                ty: ty(module(&["math"]), "Vec2"),
                count: 1,
            })
        );
        assert!(validate(&provider).is_ok());
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
            ExternOperatorDescriptor {
                op: ExternOperator::Binary {
                    op: BinaryOp::Mul,
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
    fn rejects_invalid_final_abi_positions() {
        let mut provider = valid_provider();
        let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
            params: vec![],
            ret: Box::new(ExternTypeExpr::Void),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
                thread: CallbackThread::SameThread,
            },
        });
        provider.modules[0].functions[0].signature = ExternSignature {
            params: vec![
                ExternParam {
                    name: Some("nested".to_string()),
                    ty: ExternTypeExpr::Tuple(vec![
                        ExternTypeExpr::slice(ExternTypeExpr::Int),
                        callback.clone(),
                    ]),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
                ExternParam {
                    name: Some("returns_callback".to_string()),
                    ty: ExternTypeExpr::Callback(ExternCallbackSignature {
                        params: vec![],
                        ret: Box::new(callback.clone()),
                        policy: CallbackPolicy {
                            escape: CallbackEscape::NonEscaping,
                            thread: CallbackThread::SameThread,
                        },
                    }),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
                ExternParam {
                    name: Some("generic".to_string()),
                    ty: ExternTypeExpr::Named {
                        module: None,
                        name: "Box".to_string(),
                        args: vec![ExternTypeExpr::Int],
                    },
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
            ],
            ret: ExternTypeExpr::slice(ExternTypeExpr::Int),
        };

        let errors = validate(&provider).unwrap_err();

        assert!(errors.contains(&ExternDescriptorError::InvalidAbiType {
            position: AbiPosition::Nested,
            reason: AbiTypeError::SliceNested,
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidAbiType {
            position: AbiPosition::Nested,
            reason: AbiTypeError::CallbackNested,
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidAbiType {
            position: AbiPosition::CallbackReturn,
            reason: AbiTypeError::CallbackReturnUnsupported,
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidAbiType {
            position: AbiPosition::ParamValue,
            reason: AbiTypeError::GenericNamedArgsUnsupported,
        }));
        assert!(errors.contains(&ExternDescriptorError::InvalidAbiType {
            position: AbiPosition::Return,
            reason: AbiTypeError::SliceOutsideParam,
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
            context: TypeContext::Param,
        }));
        assert!(errors.contains(&ExternDescriptorError::EmptyModulePath));
        assert!(errors.contains(&ExternDescriptorError::InvalidName {
            kind: NameKind::NamedType,
            name: String::new(),
        }));
    }
}
