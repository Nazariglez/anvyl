use std::collections::HashSet;

use crate::{callback_escape_matches, descriptor::*, keys::*};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternDescriptorError {
    InvalidName {
        kind: &'static str,
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
    UnnamedInitParam {
        ty: ExternTypeKey,
        index: usize,
    },
    InitParamFieldCountMismatch {
        ty: ExternTypeKey,
        params: usize,
        field_init: usize,
    },
    InitParamFieldMismatch {
        ty: ExternTypeKey,
        index: usize,
        param: String,
        field: String,
    },
    MixedVariantFields {
        ty: ExternTypeKey,
        variant: String,
    },
    InvalidRepresentationMetadata {
        ty: ExternTypeKey,
    },
    InvalidLayout {
        ty: ExternTypeKey,
        size: u64,
        align: u64,
    },
    VoidType {
        context: &'static str,
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
enum NameKind {
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
enum TypeContext {
    Param,
    Nested,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValidationMode {
    Provider,
    SourceModule,
}

impl NameKind {
    fn as_str(self) -> &'static str {
        match self {
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
        }
    }
}

impl TypeContext {
    fn as_str(self) -> &'static str {
        match self {
            Self::Param => "parameter position",
            Self::Nested => "nested type position",
        }
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

    if mode == ValidationMode::Provider {
        let metadata_valid = match ty.rep {
            ExternRep::Inline => {
                ty.layout.is_some() && ty.materialization.is_some() && ty.owns_heap_edges.is_some()
            }
            ExternRep::Shared => {
                ty.layout.is_none() && ty.materialization.is_none() && ty.owns_heap_edges.is_some()
            }
        };
        if !metadata_valid {
            errors.push(ExternDescriptorError::InvalidRepresentationMetadata { ty: key.clone() });
        }
        if let Some(layout) = ty.layout
            && (layout.align == 0
                || !layout.align.is_power_of_two()
                || layout.size % layout.align != 0)
        {
            errors.push(ExternDescriptorError::InvalidLayout {
                ty: key.clone(),
                size: layout.size,
                align: layout.align,
            });
        }
    }

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
        let mut presence_inits = HashSet::new();
        for field in &init.presence_init {
            check_unique_name(
                NameKind::FieldInit,
                &mut presence_inits,
                field,
                errors,
                |name| ExternDescriptorError::DuplicateFieldInit {
                    ty: key.clone(),
                    name,
                },
            );
            if field_inits.contains(field) {
                errors.push(ExternDescriptorError::DuplicateFieldInit {
                    ty: key.clone(),
                    name: field.clone(),
                });
            }
        }
        let init_count = init.field_init.len() + init.presence_init.len();
        if init.params.len() != init_count {
            errors.push(ExternDescriptorError::InitParamFieldCountMismatch {
                ty: key.clone(),
                params: init.params.len(),
                field_init: init_count,
            });
        }
        let init_names = field_inits
            .union(&presence_inits)
            .cloned()
            .collect::<HashSet<_>>();
        let mut param_names = HashSet::new();
        for (index, param) in init.params.iter().enumerate() {
            let Some(param_name) = &param.name else {
                errors.push(ExternDescriptorError::UnnamedInitParam {
                    ty: key.clone(),
                    index,
                });
                continue;
            };
            param_names.insert(param_name.clone());
            if !init_names.contains(param_name) {
                errors.push(ExternDescriptorError::InitParamFieldMismatch {
                    ty: key.clone(),
                    index,
                    param: param_name.clone(),
                    field: init
                        .field_init
                        .get(index)
                        .or_else(|| init.presence_init.get(index))
                        .cloned()
                        .unwrap_or_default(),
                });
            }
        }
        for field in init_names.difference(&param_names) {
            errors.push(ExternDescriptorError::InitParamFieldMismatch {
                ty: key.clone(),
                index: init.params.len(),
                param: String::new(),
                field: field.clone(),
            });
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
    if !callback_escape_matches(escape, callback) {
        errors.push(ExternDescriptorError::CallbackEscapeMismatch {
            param: param.map(str::to_string),
            param_escape: escape,
            policy_escape: callback.policy.escape,
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
                && violation.1 == AbiTypeError::GenericNamedArgsUnsupported
            {
                continue;
            }
            push_abi_violation(violation, errors);
        }
    }
    check_type_names(ty, errors);
}

fn push_abi_violation(
    violation: (AbiPosition, AbiTypeError),
    errors: &mut Vec<ExternDescriptorError>,
) {
    if violation.1 == AbiTypeError::VoidOutsideReturn {
        errors.push(ExternDescriptorError::VoidType {
            context: void_type_context(violation.0).as_str(),
        });
    } else {
        errors.push(ExternDescriptorError::InvalidAbiType {
            position: violation.0,
            reason: violation.1,
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
        | AbiPosition::Nested
        | AbiPosition::NestedParam => TypeContext::Nested,
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
        | ExternTypeExpr::Char
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
            kind: kind.as_str(),
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
