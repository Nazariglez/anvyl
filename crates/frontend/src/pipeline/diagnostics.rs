use anvyx_externs::{
    ExternDescriptorError, ExternOperator, ExternTypeExpr, ExternTypeKey,
    ModulePath as ExternModulePath, NameKind, OperatorReturn, TypeContext,
};
use chumsky::error::{Rich, RichReason};

use crate::{
    ast::{ConstValue, Ident},
    externs::{
        ExternInputError, ExternProvenance, RawExternDecl, RawExternFunctionKey,
        RawExternIdentityKey, RawExternMemberKey, RawExternScope, RawExternTypeKey,
        UnsupportedSourceKind, UnsupportedSourceParamReason,
    },
    lexer::SpannedToken,
    resolve::{ModulePath, ResolveError},
    typecheck::{
        ArityError, BindingNamespace, BindingOrigin, ConstDiagnostic, DeclError, MemberAccessKind,
        ModuleScope, TypeError,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    message: String,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

pub(super) fn diagnose_lex_error(error: &Rich<'_, char>) -> Diagnostic {
    Diagnostic::error(match error.reason() {
        RichReason::Custom(message) => message.clone(),
        RichReason::ExpectedFound { found, .. } => match found.as_deref() {
            Some(found) => format!("Unexpected character '{found}'"),
            None => "Unexpected end of input".to_string(),
        },
    })
}

pub(super) fn diagnose_parse_error(error: &Rich<'_, SpannedToken>) -> Diagnostic {
    Diagnostic::error(match error.reason() {
        RichReason::Custom(message) => message.clone(),
        RichReason::ExpectedFound { found, .. } => match found.as_deref() {
            Some((token, _)) => format!("Unexpected token '{token}'"),
            None => "Unexpected end of input".to_string(),
        },
    })
}

pub(super) fn diagnose_resolve_error(error: &ResolveError) -> Diagnostic {
    let message = match error {
        ResolveError::ModuleNotFound { path, .. } if path.first_segment() == Some("std") => {
            format!(
                "Unknown standard library module '{}'",
                render_module_path(path)
            )
        }
        ResolveError::ModuleNotFound { path, .. } => {
            format!(
                "Cannot find module file for module '{}'",
                render_module_path(path)
            )
        }
        ResolveError::LoadFailed { path, message, .. } => {
            format!(
                "Cannot load module '{}': {message}",
                render_module_path(path)
            )
        }
        ResolveError::DuplicatePreloadedModule { path } => {
            format!(
                "module '{}' is preloaded more than once",
                render_module_path(path)
            )
        }
    };
    Diagnostic::error(message)
}

pub(super) fn diagnose_unresolved_always_active_module(module: &ModuleScope) -> Diagnostic {
    Diagnostic::error(format!(
        "always-active module was not resolved: {}",
        render_module_scope(module)
    ))
}

pub(super) fn diagnose_extern_input_error(error: &ExternInputError) -> Diagnostic {
    let message = match error {
        ExternInputError::InvalidProviderDescriptor { provider, error } => format!(
            "invalid extern descriptor from provider '{}': {}",
            provider.name,
            render_extern_descriptor_error(error, None)
        ),
        ExternInputError::InvalidRawDescriptor { decl, scope, error } => format!(
            "invalid extern descriptor from {}: {}",
            render_raw_decl(decl),
            render_extern_descriptor_error(error, Some(scope))
        ),
        ExternInputError::DuplicateRawIdentity {
            key,
            first,
            duplicate,
        } => render_duplicate_raw_identity(
            render_raw_identity_kind(key),
            &render_raw_identity_key(key),
            first,
            duplicate,
        ),
        ExternInputError::UnsupportedSource { kind, .. } => render_unsupported_source(kind),
    };
    Diagnostic::error(message)
}

fn render_unsupported_source(kind: &UnsupportedSourceKind) -> String {
    match kind {
        UnsupportedSourceKind::Type(ty) => format!("unsupported source extern type '{ty}'"),
        UnsupportedSourceKind::Operator(op) => format!("unsupported source extern operator '{op}'"),
        UnsupportedSourceKind::Param { name, reason } => {
            render_unsupported_param(Some(name), *reason, false)
        }
        UnsupportedSourceKind::CallbackParam { reason } => {
            render_unsupported_param(None, *reason, true)
        }
    }
}

fn render_unsupported_param(
    name: Option<&str>,
    reason: UnsupportedSourceParamReason,
    callback: bool,
) -> String {
    let prefix = match reason {
        UnsupportedSourceParamReason::Mutable => "mutable",
        UnsupportedSourceParamReason::CastAccept => "cast-accepting",
        UnsupportedSourceParamReason::Default => "default",
    };
    let subject = if callback {
        "callback parameter"
    } else {
        "parameter"
    };
    let plural = if callback {
        "callback parameters"
    } else {
        "parameters"
    };
    match name {
        Some(name) => format!(
            "unsupported source extern {subject} '{name}': {prefix} {plural} are not supported in source extern declarations"
        ),
        None => format!(
            "unsupported source extern {subject}: {prefix} {plural} are not supported in source extern declarations"
        ),
    }
}

fn render_raw_identity_kind(key: &RawExternIdentityKey) -> &'static str {
    match key {
        RawExternIdentityKey::Function(_) => "function",
        RawExternIdentityKey::Type(_) => "type",
        RawExternIdentityKey::Member(key) => render_raw_member_kind(key),
    }
}

fn render_raw_member_kind(key: &RawExternMemberKey) -> &'static str {
    match &key.selector {
        anvyx_externs::ExternMemberSelector::Field(_) => "field",
        anvyx_externs::ExternMemberSelector::Method(_) => "method",
        anvyx_externs::ExternMemberSelector::Static(_) => "static method",
        anvyx_externs::ExternMemberSelector::Init => "init",
        anvyx_externs::ExternMemberSelector::Operator(_) => "operator",
    }
}

fn render_raw_identity_key(key: &RawExternIdentityKey) -> String {
    match key {
        RawExternIdentityKey::Function(key) => render_raw_function_key(key),
        RawExternIdentityKey::Type(key) => render_raw_type_key(key),
        RawExternIdentityKey::Member(key) => render_raw_member_key(key),
    }
}

fn render_duplicate_raw_identity(
    kind: &str,
    key: &str,
    first: &RawExternDecl,
    duplicate: &RawExternDecl,
) -> String {
    format!(
        "duplicate extern {kind} '{key}' declared in {} and {}",
        render_raw_decl(first),
        render_raw_decl(duplicate)
    )
}

fn render_raw_decl(decl: &RawExternDecl) -> String {
    render_extern_provenance(&decl.provenance)
}

fn render_extern_provenance(provenance: &ExternProvenance) -> String {
    match provenance {
        ExternProvenance::Provider { provider } => format!("provider '{}'", provider.name),
        ExternProvenance::Source { module } => match module {
            RawExternScope::Root => "source root".to_string(),
            RawExternScope::Named(path) => {
                format!("source module '{}'", render_extern_module_path(path))
            }
        },
    }
}

fn render_raw_function_key(key: &RawExternFunctionKey) -> String {
    format!("{}.{}", render_raw_scope(&key.module), key.name)
}

fn render_raw_type_key(key: &RawExternTypeKey) -> String {
    format!("{}.{}", render_raw_scope(&key.module), key.name)
}

fn render_raw_member_key(key: &RawExternMemberKey) -> String {
    format!(
        "{}.{}",
        render_raw_type_key(&key.owner),
        render_extern_member_selector(&key.selector)
    )
}

fn render_raw_scope(scope: &RawExternScope) -> String {
    match scope {
        RawExternScope::Root => "<root>".to_string(),
        RawExternScope::Named(path) => render_extern_module_path(path),
    }
}

fn render_extern_member_selector(selector: &anvyx_externs::ExternMemberSelector) -> String {
    match selector {
        anvyx_externs::ExternMemberSelector::Field(name)
        | anvyx_externs::ExternMemberSelector::Method(name)
        | anvyx_externs::ExternMemberSelector::Static(name) => name.clone(),
        anvyx_externs::ExternMemberSelector::Init => "init".to_string(),
        anvyx_externs::ExternMemberSelector::Operator(op) => {
            render_extern_operator(*op).to_string()
        }
    }
}

pub(super) fn diagnose_type_error(error: &TypeError) -> Diagnostic {
    Diagnostic::error(match error {
        TypeError::Decl(error) => render_decl_error(error),
        TypeError::UndefinedVariable { name, .. } => format!("Unknown variable '{name}'"),
        TypeError::TypeMismatch {
            expected, found, ..
        } => {
            format!("Mismatched types: expected '{expected}', found '{found}'")
        }
        TypeError::ConstMismatch {
            expected, found, ..
        } => format!(
            "Mismatched types: expected const '{}', found '{}'",
            render_const_diagnostic(expected),
            render_const_diagnostic(found)
        ),
        TypeError::RecursiveInference { .. } => {
            "recursive type inference is not allowed".to_string()
        }
        TypeError::CannotInferType { .. } => "Could not infer type".to_string(),
        TypeError::UnknownType {
            qualifier, name, ..
        } => format!(
            "Unknown type '{}'",
            render_qualified_name(*qualifier, *name)
        ),
        TypeError::CannotInferConst { .. } => "Could not infer const value".to_string(),
        TypeError::NotCallable { ty, .. } => format!("type '{ty}' is not callable"),
        TypeError::WrongArgCount {
            expected, found, ..
        } => {
            format!("Wrong number of arguments: expected {expected}, found {found}")
        }
        TypeError::DuplicateName { name, .. } => format!("name '{name}' is already declared"),
        TypeError::ImmutableAssignment { name, .. } => {
            format!("cannot assign to immutable value '{name}'")
        }
        TypeError::InvalidOperand {
            op, operand_type, ..
        } => format!("Invalid operand type: operator '{op}' cannot be applied to '{operand_type}'"),
        TypeError::MissingReturn { expected, .. } => {
            format!("Mismatched types: expected '{expected}', found 'void'")
        }
        TypeError::UnusedValue { .. } => "unused value".to_string(),
        TypeError::IfConditionNotBool { found, .. } => {
            format!("Condition of if expression must be bool: found '{found}'")
        }
        TypeError::WhileConditionNotBool { found, .. } => {
            format!("Condition of while must be bool: found '{found}'")
        }
        TypeError::BreakOutsideLoop { .. } => "break outside of loop".to_string(),
        TypeError::ContinueOutsideLoop { .. } => "continue outside of loop".to_string(),
        TypeError::ForIterableNotSupported { found, .. } => {
            format!("type '{found}' cannot be iterated")
        }
        TypeError::UnsupportedPattern { pattern, .. } => format!("Unsupported pattern: {pattern}"),
        TypeError::TuplePatternArityMismatch {
            expected, found, ..
        } => {
            format!("tuple pattern arity mismatch: expected {expected}, found {found}")
        }
        TypeError::NamedPatternOnPositional { .. } => {
            "named tuple pattern cannot match positional tuple".to_string()
        }
        TypeError::TuplePatternLabelMismatch {
            expected, found, ..
        } => {
            format!("tuple pattern label mismatch: expected '{expected}', found '{found}'")
        }
        TypeError::OrPatternUnsupported { .. } => "or-patterns are not supported".to_string(),
        TypeError::EmptyMatch { .. } => "match expression must have at least one arm".to_string(),
        TypeError::UnreachableFalsePattern { .. } => "false pattern is unreachable".to_string(),
        TypeError::MemberAccessOnNonAggregate {
            ty, member, kind, ..
        }
        | TypeError::UnknownMember {
            ty, member, kind, ..
        } => {
            let kind = render_member_access_kind(*kind);
            format!("Unknown {kind} '{member}' for type '{ty}'")
        }
        TypeError::UndefinedModuleMember { module, name, .. } => {
            format!(
                "Unknown member '{name}' in module '{}'",
                render_module_scope(module)
            )
        }
        TypeError::AmbiguousExtendMethod { receiver, name, .. } => {
            format!("ambiguous method '{name}' for type '{receiver}'")
        }
        TypeError::DuplicateField { name, .. } => format!("Duplicate field '{name}'"),
        TypeError::MissingField { name, .. } => format!("Missing field '{name}'"),
        TypeError::InvalidStructLiteral { name, kind, .. } => {
            format!("type '{name}' does not support struct literal construction ({kind})")
        }
        TypeError::UnknownStructLiteral {
            qualifier, name, ..
        } => format!(
            "Unknown struct '{}'",
            render_qualified_name(*qualifier, *name)
        ),
        TypeError::UnknownEnumVariant {
            enum_name, variant, ..
        } => format!("Unknown variant '{variant}' for enum '{enum_name}'"),
        TypeError::GenericArity(ArityError::TypeArgs { expected, found }) => {
            format!("wrong number of type parameters: expected {expected}, found {found}")
        }
        TypeError::UnboundGenericParam { name, .. } => {
            format!("Could not infer type parameter '{name}'")
        }
        TypeError::UnknownConst { name, .. } => format!("unknown constant '{name}'"),
        TypeError::ConstCycle { name, .. } => format!("constant '{name}' depends on itself"),
        TypeError::NonConstExpression { .. } => "not a constant expression".to_string(),
        TypeError::ConstTypeMismatch {
            expected, found, ..
        } => {
            format!("constant type mismatch: expected '{expected}', found '{found}'")
        }
        TypeError::InvalidConstCast { from, to, .. } => {
            format!("cannot cast constant from '{from}' to '{to}'")
        }
        TypeError::ConstDivisionByZero { .. } => {
            "division by zero in constant expression".to_string()
        }
        TypeError::ConstOverflow { .. } => "constant expression overflow".to_string(),
        TypeError::ExpectedIntConst { found, .. } => {
            format!("expected integer constant, found '{found}'")
        }
        TypeError::NegativeArrayLength { value, .. } => {
            format!("array length must not be negative: {value}")
        }
        TypeError::GenericArgKindMismatch { expected, .. } => {
            format!("expected {expected} generic argument")
        }
        TypeError::DuplicateGenericParam { name, .. } => {
            format!("duplicate generic parameter '{name}'")
        }
    })
}

fn render_member_access_kind(kind: MemberAccessKind) -> &'static str {
    match kind {
        MemberAccessKind::Field => "field",
        MemberAccessKind::Method => "method",
    }
}

fn render_qualified_name(qualifier: Option<Ident>, name: Ident) -> String {
    match qualifier {
        Some(qualifier) => format!("{qualifier}.{name}"),
        None => name.to_string(),
    }
}

fn render_decl_error(error: &DeclError) -> String {
    match error {
        DeclError::DuplicateValue { name, .. } => format!("value '{name}' is already declared"),
        DeclError::DuplicateType { name, .. } => format!("type '{name}' is already defined"),
        DeclError::MissingImportMember { imported, name, .. } => {
            format!(
                "Unknown member '{name}' in module '{}'",
                render_module_scope(imported)
            )
        }
        DeclError::PrivateImportMember { imported, name, .. } => {
            format!(
                "member '{name}' in module '{}' is private",
                render_module_scope(imported)
            )
        }
        DeclError::ImportConflict {
            name,
            namespace,
            first: BindingOrigin::Local,
            ..
        } => format!(
            "imported {} '{name}' conflicts with a locally defined {}",
            render_binding_namespace(*namespace),
            render_binding_namespace(*namespace)
        ),
        DeclError::ImportConflict {
            name, namespace, ..
        } => format!(
            "imported {} '{name}' conflicts with a previously imported name",
            render_binding_namespace(*namespace)
        ),
        DeclError::DuplicateModuleBinding { name, .. } => {
            format!("module binding '{name}' is already in use")
        }
        DeclError::DuplicateGenericParam { name, .. } => {
            format!("duplicate generic parameter '{name}'")
        }
        DeclError::UnknownType {
            qualifier, name, ..
        } => format!(
            "Unknown type '{}'",
            render_qualified_name(*qualifier, *name)
        ),
        DeclError::ReexportConflict {
            name,
            namespace,
            first,
            second,
            ..
        } => format!(
            "{} '{name}' is re-exported by both {} and {}",
            render_binding_namespace(*namespace),
            render_binding_origin(first),
            render_binding_origin(second)
        ),
    }
}

fn render_binding_namespace(namespace: BindingNamespace) -> &'static str {
    match namespace {
        BindingNamespace::Value => "value",
        BindingNamespace::Type => "type",
        BindingNamespace::Module => "module",
    }
}

fn render_binding_origin(origin: &BindingOrigin) -> String {
    match origin {
        BindingOrigin::Local => "local declarations".to_string(),
        BindingOrigin::Import { source } | BindingOrigin::Reexport { source } => {
            format!("'{}'", render_module_scope(source))
        }
    }
}

fn render_const_diagnostic(diagnostic: &ConstDiagnostic) -> String {
    match diagnostic {
        ConstDiagnostic::Value(value) => render_const_value(value),
        ConstDiagnostic::Name(name) => name.to_string(),
        ConstDiagnostic::Unknown => "unknown".to_string(),
    }
}

fn render_const_value(value: &ConstValue) -> String {
    match value {
        ConstValue::Int(value) => value.to_string(),
        ConstValue::Float(value) => value.to_string(),
        ConstValue::Bool(value) => value.to_string(),
        ConstValue::String(value) => value.clone(),
    }
}

fn render_extern_descriptor_error(
    error: &ExternDescriptorError,
    raw_scope: Option<&RawExternScope>,
) -> String {
    match error {
        ExternDescriptorError::InvalidName { kind, name } => {
            format!("invalid {} name '{name}'", render_name_kind(*kind))
        }
        ExternDescriptorError::EmptyModulePath => "module path must not be empty".to_string(),
        ExternDescriptorError::DuplicateModule(path) => {
            format!("duplicate module '{}'", render_extern_module_path(path))
        }
        ExternDescriptorError::DuplicateType { module, name } => format!(
            "duplicate type '{name}' in module '{}'",
            render_extern_module_path(module)
        ),
        ExternDescriptorError::DuplicateFunction { module, name } => format!(
            "duplicate function '{name}' in module '{}'",
            render_extern_module_path(module)
        ),
        ExternDescriptorError::DuplicateField { ty, name } => format!(
            "duplicate field '{name}' on extern type '{}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::DuplicateMethod { ty, name } => format!(
            "duplicate method '{name}' on extern type '{}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::DuplicateStatic { ty, name } => format!(
            "duplicate static method '{name}' on extern type '{}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::DuplicateOperator { ty, op } => format!(
            "duplicate operator '{}' on extern type '{}'",
            render_extern_operator(*op),
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InvalidOperatorSignature {
            ty,
            op,
            expected_params,
            actual_params,
        } => format!(
            "invalid operator '{}' on extern type '{}': expected {expected_params} parameter(s), found {actual_params}",
            render_extern_operator(*op),
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InvalidOperatorReturn {
            ty,
            op,
            expected,
            actual,
        } => format!(
            "invalid operator '{}' on extern type '{}': expected {} return type, found '{}'",
            render_extern_operator(*op),
            render_extern_type_key(ty, raw_scope),
            render_operator_return(*expected),
            render_extern_type_expr(actual)
        ),
        ExternDescriptorError::DuplicateFieldInit { ty, name } => format!(
            "duplicate init field '{name}' on extern type '{}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::VoidType { context } => {
            format!(
                "void type is not allowed in {}",
                render_type_context(*context)
            )
        }
    }
}

fn render_name_kind(kind: NameKind) -> &'static str {
    match kind {
        NameKind::Provider => "provider",
        NameKind::ModuleSegment => "module segment",
        NameKind::Type => "type",
        NameKind::Function => "function",
        NameKind::Field => "field",
        NameKind::FieldInit => "field init",
        NameKind::Method => "method",
        NameKind::Static => "static method",
        NameKind::Param => "parameter",
        NameKind::NamedType => "named type",
    }
}

fn render_operator_return(expected: OperatorReturn) -> &'static str {
    match expected {
        OperatorReturn::Bool => "bool",
        OperatorReturn::NonVoid => "non-void",
    }
}

fn render_type_context(context: TypeContext) -> &'static str {
    match context {
        TypeContext::Param => "parameter position",
        TypeContext::Nested => "nested type position",
    }
}

fn render_extern_type_key(key: &ExternTypeKey, raw_scope: Option<&RawExternScope>) -> String {
    let module = raw_scope.map_or_else(|| render_extern_module_path(&key.module), render_raw_scope);
    format!("{}.{}", module, key.name)
}

fn render_extern_module_path(path: &ExternModulePath) -> String {
    path.segments.join(".")
}

fn render_extern_operator(op: ExternOperator) -> &'static str {
    match op {
        ExternOperator::Unary(anvyx_externs::UnaryOp::Neg) => "unary -",
        ExternOperator::Binary { op, self_on_right } => match (op, self_on_right) {
            (anvyx_externs::BinaryOp::Add, false) => "+",
            (anvyx_externs::BinaryOp::Add, true) => "right +",
            (anvyx_externs::BinaryOp::Sub, false) => "-",
            (anvyx_externs::BinaryOp::Sub, true) => "right -",
            (anvyx_externs::BinaryOp::Mul, false) => "*",
            (anvyx_externs::BinaryOp::Mul, true) => "right *",
            (anvyx_externs::BinaryOp::Div, false) => "/",
            (anvyx_externs::BinaryOp::Div, true) => "right /",
            (anvyx_externs::BinaryOp::Rem, false) => "%",
            (anvyx_externs::BinaryOp::Rem, true) => "right %",
            (anvyx_externs::BinaryOp::Eq, false) => "==",
            (anvyx_externs::BinaryOp::Eq, true) => "right ==",
            (anvyx_externs::BinaryOp::NotEq, false) => "!=",
            (anvyx_externs::BinaryOp::NotEq, true) => "right !=",
            (anvyx_externs::BinaryOp::LessThan, false) => "<",
            (anvyx_externs::BinaryOp::LessThan, true) => "right <",
            (anvyx_externs::BinaryOp::GreaterThan, false) => ">",
            (anvyx_externs::BinaryOp::GreaterThan, true) => "right >",
            (anvyx_externs::BinaryOp::LessThanEq, false) => "<=",
            (anvyx_externs::BinaryOp::LessThanEq, true) => "right <=",
            (anvyx_externs::BinaryOp::GreaterThanEq, false) => ">=",
            (anvyx_externs::BinaryOp::GreaterThanEq, true) => "right >=",
        },
    }
}

fn render_extern_type_expr(ty: &ExternTypeExpr) -> String {
    match ty {
        ExternTypeExpr::Void => "void".to_string(),
        ExternTypeExpr::Bool => "bool".to_string(),
        ExternTypeExpr::Int => "int".to_string(),
        ExternTypeExpr::Float => "float".to_string(),
        ExternTypeExpr::String => "string".to_string(),
        ExternTypeExpr::Any => "any".to_string(),
        ExternTypeExpr::List(item) => format!("[{}]", render_extern_type_expr(item)),
        ExternTypeExpr::Map(key, value) => format!(
            "[{}: {}]",
            render_extern_type_expr(key),
            render_extern_type_expr(value)
        ),
        ExternTypeExpr::Option(item) => format!("{}?", render_extern_type_expr(item)),
        ExternTypeExpr::Named { module, name, args } => {
            let mut rendered = match module {
                Some(module) => format!("{}.{}", render_extern_module_path(module), name),
                None => name.clone(),
            };
            if !args.is_empty() {
                let args = args
                    .iter()
                    .map(render_extern_type_expr)
                    .collect::<Vec<_>>()
                    .join(", ");
                rendered.push_str(&format!("<{args}>"));
            }
            rendered
        }
        ExternTypeExpr::Callback(_) => "callback".to_string(),
    }
}

fn render_module_path(path: &ModulePath) -> String {
    path.segments().join(".")
}

fn render_module_scope(scope: &ModuleScope) -> String {
    match scope {
        ModuleScope::Root => "<root>".to_string(),
        ModuleScope::Named(path) => render_module_path(path),
    }
}

#[cfg(test)]
mod tests {
    use chumsky::error::{LabelError, RichPattern};

    use super::*;
    use crate::{
        ast::{Ident, Type},
        lexer::Token,
        span::Span,
    };

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn module_path(path: &[&str]) -> ModulePath {
        ModulePath::new(path.iter().map(ToString::to_string).collect()).unwrap()
    }

    fn module_scope(path: &[&str]) -> ModuleScope {
        ModuleScope::Named(module_path(path))
    }

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn assert_msg(diagnostic: impl std::fmt::Display, expected: &str) {
        let message = diagnostic.to_string();
        assert_eq!(message, expected);
        assert_no_debug_artifacts(&message);
    }

    fn assert_no_debug_artifacts(message: &str) {
        for artifact in [
            "Span {",
            "Ident(\"",
            "ModulePath([",
            "NominalType {",
            "TypeMismatch {",
            "Decl(",
        ] {
            assert!(
                !message.contains(artifact),
                "message contains debug artifact {artifact}: {message}"
            );
        }
    }

    #[test]
    fn renders_resolve_errors() {
        let cases = [
            (
                diagnose_resolve_error(&ResolveError::ModuleNotFound {
                    path: module_path(&["foo", "bar"]),
                    span: span(),
                }),
                "Cannot find module file for module 'foo.bar'",
            ),
            (
                diagnose_resolve_error(&ResolveError::ModuleNotFound {
                    path: module_path(&["std", "foo"]),
                    span: span(),
                }),
                "Unknown standard library module 'std.foo'",
            ),
            (
                diagnose_resolve_error(&ResolveError::LoadFailed {
                    path: module_path(&["foo", "bar"]),
                    span: span(),
                    message: "permission denied".to_string(),
                }),
                "Cannot load module 'foo.bar': permission denied",
            ),
            (
                diagnose_resolve_error(&ResolveError::DuplicatePreloadedModule {
                    path: module_path(&["core", "math"]),
                }),
                "module 'core.math' is preloaded more than once",
            ),
        ];

        for (diagnostic, expected) in cases {
            assert_msg(diagnostic, expected);
        }
    }

    #[test]
    fn renders_decl_errors() {
        let cases = [
            (
                diagnose_type_error(&TypeError::Decl(DeclError::DuplicateType {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    span: span(),
                })),
                "type 'Point' is already defined",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::MissingImportMember {
                    module: ModuleScope::Root,
                    imported: module_scope(&["tools"]),
                    name: ident("Point"),
                    span: span(),
                })),
                "Unknown member 'Point' in module 'tools'",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::PrivateImportMember {
                    module: ModuleScope::Root,
                    imported: module_scope(&["tools"]),
                    name: ident("secret"),
                    span: span(),
                })),
                "member 'secret' in module 'tools' is private",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::DuplicateModuleBinding {
                    module: ModuleScope::Root,
                    name: ident("tools"),
                    first: BindingOrigin::Local,
                    second: BindingOrigin::Import {
                        source: module_scope(&["tools"]),
                    },
                    span: span(),
                })),
                "module binding 'tools' is already in use",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::DuplicateGenericParam {
                    module: ModuleScope::Root,
                    name: ident("T"),
                    span: span(),
                })),
                "duplicate generic parameter 'T'",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::ImportConflict {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    namespace: BindingNamespace::Type,
                    first: BindingOrigin::Local,
                    second: BindingOrigin::Import {
                        source: module_scope(&["shapes"]),
                    },
                    span: span(),
                })),
                "imported type 'Point' conflicts with a locally defined type",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::ImportConflict {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    namespace: BindingNamespace::Type,
                    first: BindingOrigin::Import {
                        source: module_scope(&["alpha"]),
                    },
                    second: BindingOrigin::Import {
                        source: module_scope(&["beta"]),
                    },
                    span: span(),
                })),
                "imported type 'Point' conflicts with a previously imported name",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::ReexportConflict {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    namespace: BindingNamespace::Type,
                    first: BindingOrigin::Reexport {
                        source: module_scope(&["alpha"]),
                    },
                    second: BindingOrigin::Reexport {
                        source: module_scope(&["beta"]),
                    },
                    span: span(),
                })),
                "type 'Point' is re-exported by both 'alpha' and 'beta'",
            ),
        ];

        for (diagnostic, expected) in cases {
            assert_msg(diagnostic, expected);
        }
    }

    #[test]
    fn renders_type_errors() {
        let cases = [
            (
                diagnose_type_error(&TypeError::TypeMismatch {
                    expected: Type::Int,
                    found: Type::Bool,
                    span: span(),
                }),
                "Mismatched types: expected 'int', found 'bool'",
            ),
            (
                diagnose_type_error(&TypeError::UndefinedVariable {
                    name: ident("x"),
                    span: span(),
                }),
                "Unknown variable 'x'",
            ),
            (
                diagnose_type_error(&TypeError::InvalidOperand {
                    op: "-".to_string(),
                    operand_type: Type::Bool,
                    span: span(),
                }),
                "Invalid operand type: operator '-' cannot be applied to 'bool'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownMember {
                    ty: Type::UnresolvedName(ident("Point")),
                    member: ident("z"),
                    kind: MemberAccessKind::Field,
                    span: span(),
                }),
                "Unknown field 'z' for type 'Point'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownMember {
                    ty: Type::UnresolvedName(ident("Counter")),
                    member: ident("reset"),
                    kind: MemberAccessKind::Method,
                    span: span(),
                }),
                "Unknown method 'reset' for type 'Counter'",
            ),
            (
                diagnose_type_error(&TypeError::MemberAccessOnNonAggregate {
                    ty: Type::Int,
                    member: ident("y"),
                    kind: MemberAccessKind::Field,
                    span: span(),
                }),
                "Unknown field 'y' for type 'int'",
            ),
            (
                diagnose_type_error(&TypeError::GenericArity(ArityError::TypeArgs {
                    expected: 1,
                    found: 2,
                })),
                "wrong number of type parameters: expected 1, found 2",
            ),
            (
                diagnose_type_error(&TypeError::UnboundGenericParam {
                    name: ident("T"),
                    span: span(),
                }),
                "Could not infer type parameter 'T'",
            ),
            (
                diagnose_type_error(&TypeError::DuplicateGenericParam {
                    name: ident("T"),
                    span: span(),
                }),
                "duplicate generic parameter 'T'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownStructLiteral {
                    qualifier: None,
                    name: ident("Point"),
                    span: span(),
                }),
                "Unknown struct 'Point'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownStructLiteral {
                    qualifier: Some(ident("shapes")),
                    name: ident("Point"),
                    span: span(),
                }),
                "Unknown struct 'shapes.Point'",
            ),
        ];

        for (diagnostic, expected) in cases {
            assert_msg(diagnostic, expected);
        }
    }

    #[test]
    fn renders_lex_errors() {
        assert_msg(
            diagnose_lex_error(&Rich::custom((0..1).into(), "invalid escape sequence")),
            "invalid escape sequence",
        );

        let error =
            <Rich<'_, char> as LabelError<'_, &'_ str, RichPattern<'_, char>>>::expected_found(
                [],
                Some('!'.into()),
                (0..1).into(),
            );
        assert_msg(diagnose_lex_error(&error), "Unexpected character '!'");
    }

    #[test]
    fn renders_parse_errors() {
        assert_msg(
            diagnose_parse_error(&Rich::custom((0..1).into(), "custom parser message")),
            "custom parser message",
        );

        let token = (Token::Semicolon, span());
        let error = <Rich<'_, SpannedToken> as LabelError<
            '_,
            &'_ [SpannedToken],
            RichPattern<'_, SpannedToken>,
        >>::expected_found([], Some(token.into()), (0..1).into());
        assert_msg(diagnose_parse_error(&error), "Unexpected token ';'");

        let error = <Rich<'_, SpannedToken> as LabelError<
            '_,
            &'_ [SpannedToken],
            RichPattern<'_, SpannedToken>,
        >>::expected_found([], None, (0..1).into());
        assert_msg(diagnose_parse_error(&error), "Unexpected end of input");
    }
}
