use anvyx_externs::{
    ExternDescriptorError, ExternOperator, ExternTypeExpr, ExternTypeKey,
    ModulePath as ExternModulePath, NameKind, OperatorReturn, TypeContext,
};
use chumsky::error::{Rich, RichReason};

use crate::{
    ast::{ConstArg, ConstValue, FuncParam, Ident, ModuleOrigin, Type},
    externs::{
        ExternInputError, ExternProvenance, RawExternDecl, RawExternFunctionKey,
        RawExternIdentityKey, RawExternMemberKey, RawExternScope, RawExternTypeKey,
        UnsupportedSourceKind, UnsupportedSourceParamReason,
        catalog::{
            ExternCatalogContext, ExternCatalogError, ExternContextItem, InvalidExternTypeReason,
        },
        raw_module_scope,
    },
    lexer::SpannedToken,
    resolve::{ModuleId, ModulePath, PackageId, PackageModulePath, ResolveError, SourceFileId},
    typecheck::{
        ArityError, BindingNamespace, BindingOrigin, ConstDiagnostic, DeclError, DeprecatedUseKind,
        MemberAccessKind, ModuleScope, TypeError, TypeWarning, VariantShape,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    severity: DiagnosticSeverity,
    message: String,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self::new(DiagnosticSeverity::Error, message)
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(DiagnosticSeverity::Warning, message)
    }

    fn new(severity: DiagnosticSeverity, message: impl Into<String>) -> Self {
        Self {
            severity,
            message: message.into(),
        }
    }

    pub fn severity(&self) -> DiagnosticSeverity {
        self.severity
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
        ResolveError::ModuleNotFound { module, .. } => {
            format!(
                "Cannot find module file for module '{}'",
                render_module_id(module)
            )
        }
        ResolveError::SourceImportNotFound {
            importer,
            path,
            candidate,
            ..
        } => match candidate {
            Some(candidate) => format!(
                "Cannot find source import '{}' from '{}' at '{}'",
                render_module_path(path),
                importer,
                candidate.display()
            ),
            None => format!(
                "Cannot find source import '{}' from '{}'",
                render_module_path(path),
                importer
            ),
        },
        ResolveError::LoadFailed {
            module, message, ..
        } => {
            format!(
                "Cannot load module '{}': {message}",
                render_module_id(module)
            )
        }
        ResolveError::DuplicatePreloadedModule { module } => {
            format!(
                "module '{}' is preloaded more than once",
                render_module_id(module)
            )
        }
        ResolveError::UnknownDependency { alias, package, .. } => {
            format!("package '{package}' has no dependency named '{alias}'")
        }
        ResolveError::PackageImportUnavailable { file, alias, .. } => {
            format!("source file '{file}' has no package dependency named '{alias}'")
        }
        ResolveError::UnsupportedImportRoot { root, .. } => {
            format!("import root '{root}' is not supported yet")
        }
        ResolveError::NativeProviderUnavailable { package, .. } => match package {
            Some(package) => format!("package '{package}' has no native provider modules"),
            None => "native provider imports require a package context".to_string(),
        },
        ResolveError::UnknownNativeProviderModule {
            package, module, ..
        } => format!(
            "package '{package}' has no native provider module '{}'",
            render_module_path(module)
        ),
        ResolveError::UnknownNativeDepProviderModule {
            package,
            alias,
            module,
            ..
        } => format!(
            "native-only dependency '{alias}' ({package}) has no native provider module '{}'",
            render_module_path(module)
        ),
        ResolveError::NativeOnlyPkgRootImport { package, alias, .. } => {
            format!("native-only dependency '{alias}' ({package}) has no source root to import")
        }
    };
    Diagnostic::error(message)
}

pub(super) fn diagnose_extern_input_error(error: &ExternInputError) -> Diagnostic {
    let message = match error {
        ExternInputError::InvalidProviderDescriptor {
            package,
            provider,
            error,
        } => format!(
            "invalid extern descriptor from provider '{}' in package '{}': {}",
            provider.name,
            package,
            render_extern_descriptor_error(error, None)
        ),
        ExternInputError::DuplicateProviderModule {
            package,
            module,
            first,
            duplicate,
        } => format!(
            "duplicate provider module '{}' in package '{}' declared by providers '{}' and '{}'",
            render_extern_module_path(module),
            package,
            first.name,
            duplicate.name
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
            &render_extern_identity_label(key, first, duplicate),
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
        UnsupportedSourceKind::InferReturn => {
            "inferred return type is not allowed in extern declarations".to_string()
        }
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

fn render_extern_identity_label(
    key: &RawExternIdentityKey,
    first: &RawExternDecl,
    duplicate: &RawExternDecl,
) -> String {
    if is_source_raw_identity(key, first, duplicate) {
        render_source_raw_identity_key(key)
    } else {
        render_raw_identity_key(key)
    }
}

fn is_source_raw_identity(
    key: &RawExternIdentityKey,
    first: &RawExternDecl,
    duplicate: &RawExternDecl,
) -> bool {
    let Some(first_scope) = source_provenance_scope(&first.provenance) else {
        return false;
    };
    let Some(duplicate_scope) = source_provenance_scope(&duplicate.provenance) else {
        return false;
    };

    first_scope == duplicate_scope && raw_identity_scope(key) == first_scope
}

fn source_provenance_scope(provenance: &ExternProvenance) -> Option<&RawExternScope> {
    match provenance {
        ExternProvenance::Source { module } => Some(module),
        ExternProvenance::Provider { .. } => None,
    }
}

fn raw_identity_scope(key: &RawExternIdentityKey) -> &RawExternScope {
    match key {
        RawExternIdentityKey::Function(key) => &key.module,
        RawExternIdentityKey::Type(key) => &key.module,
        RawExternIdentityKey::Member(key) => &key.owner.module,
    }
}

fn render_source_raw_identity_key(key: &RawExternIdentityKey) -> String {
    match key {
        RawExternIdentityKey::Function(key) => key.name.clone(),
        RawExternIdentityKey::Type(key) => key.name.clone(),
        RawExternIdentityKey::Member(key) => format!(
            "{}.{}",
            key.owner.name.as_str(),
            render_extern_member_selector(&key.selector)
        ),
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
        ExternProvenance::Provider { package, provider } => {
            format!("provider '{}' in package '{}'", provider.name, package)
        }
        ExternProvenance::Source { module } if is_raw_root_scope(module) => {
            "source root".to_string()
        }
        ExternProvenance::Source { module } => {
            format!("source module '{}'", render_raw_scope(module))
        }
    }
}

fn render_raw_function_key(key: &RawExternFunctionKey) -> String {
    render_raw_scoped_name(&key.module, &key.name)
}

fn render_raw_type_key(key: &RawExternTypeKey) -> String {
    render_raw_scoped_name(&key.module, &key.name)
}

fn render_raw_member_key(key: &RawExternMemberKey) -> String {
    format!(
        "{}.{}",
        render_raw_type_key(&key.owner),
        render_extern_member_selector(&key.selector)
    )
}

fn render_raw_scoped_name(scope: &RawExternScope, name: &str) -> String {
    match scope {
        RawExternScope::Module(module) if is_root_module_id(module) => name.to_string(),
        RawExternScope::Module(module) => format!("{}.{name}", render_module_id(module)),
    }
}

fn render_raw_scope(scope: &RawExternScope) -> String {
    match scope {
        RawExternScope::Module(module) => render_module_id(module),
    }
}

fn is_raw_root_scope(scope: &RawExternScope) -> bool {
    matches!(scope, RawExternScope::Module(module) if is_root_module_id(module))
}

fn is_root_module_id(module: &ModuleId) -> bool {
    module.package_context() == Some(&PackageId::synthetic_root())
        && matches!(module.path(), PackageModulePath::Root)
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

pub(super) fn diagnose_type_warning(warning: &TypeWarning) -> Diagnostic {
    Diagnostic::warning(match warning {
        TypeWarning::DeprecatedAccess {
            kind, name, reason, ..
        } => render_deprecated_access(*kind, *name, reason.as_deref()),
        TypeWarning::InternalAccess {
            kind,
            name,
            owner,
            reason,
            ..
        } => render_internal_access(*kind, *name, owner, reason.as_deref()),
        TypeWarning::CompileMessage { message, .. } => message.clone(),
    })
}

pub(super) fn diagnose_type_error(error: &TypeError) -> Diagnostic {
    Diagnostic::error(match error {
        TypeError::Decl(error) => render_decl_error(error),
        TypeError::ExternCatalog(error) => render_extern_catalog_error(error),
        TypeError::UndefinedVariable { name, .. } => format!("Unknown variable '{name}'"),
        TypeError::TypeUsedAsValue { ty, .. } => {
            format!("type '{ty}' cannot be used as a value")
        }
        TypeError::TypeMismatch {
            expected, found, ..
        } => render_type_mismatch(expected, found),
        TypeError::ConstMismatch {
            expected, found, ..
        } => format!(
            "Mismatched types: expected const '{}', found '{}'",
            render_const_diagnostic(expected),
            render_const_diagnostic(found)
        ),
        TypeError::ExternAnyEscape { .. } => {
            "extern 'any' value cannot escape an extern boundary".to_string()
        }
        TypeError::AnyOutsideExternBoundary { .. } => {
            "any is only allowed in extern boundary signatures".to_string()
        }
        TypeError::RecursiveInference { .. } => {
            "recursive type inference is not allowed".to_string()
        }
        TypeError::CannotInferType { .. } => "Could not infer type".to_string(),
        TypeError::InferReturnNonGeneric { .. } => {
            "inferred return type is only allowed on generic callables".to_string()
        }
        TypeError::InferReturnExtern { .. } => {
            "inferred return type is not allowed in extern declarations".to_string()
        }
        TypeError::InferReturnValue { .. } => {
            "generic inferred-return callables cannot be used as values".to_string()
        }
        TypeError::InferReturnMismatch {
            expected, found, ..
        } => format!("inferred return type mismatch: expected '{expected}', found '{found}'"),
        TypeError::InferReturnRecursive { .. } => {
            "recursive inferred return type requires an explicit return type".to_string()
        }
        TypeError::UnknownType {
            qualifier, name, ..
        } => format!(
            "Unknown type '{}'",
            render_qualified_name(*qualifier, *name)
        ),
        TypeError::CannotInferConst { .. } => "Could not infer const value".to_string(),
        TypeError::AllNilArrayLiteral { .. } => {
            "cannot infer element type for all-nil array literal".to_string()
        }
        TypeError::ArrayFillLengthNotConst { .. } => {
            "array fill length must be a compile-time constant".to_string()
        }
        TypeError::NotCallable { ty, .. } => format!("type '{ty}' is not callable"),
        TypeError::WrongArgCount {
            expected, found, ..
        } => {
            format!("Wrong number of arguments: expected {expected}, found {found}")
        }
        TypeError::WrongArgRange {
            min, max, found, ..
        } => format!("Wrong number of arguments: expected between {min} and {max}, found {found}"),
        TypeError::LambdaParamCountMismatch {
            expected, found, ..
        } => format!("parameter count mismatch: expected {expected}, found {found}"),
        TypeError::RequiredParamAfterDefault { name, .. } => {
            format!("required parameter '{name}' cannot follow a default parameter")
        }
        TypeError::EnumVariantArgCount {
            enum_name,
            variant,
            expected,
            found,
            ..
        } => format!(
            "Wrong number of arguments for variant '{enum_name}.{variant}': expected {expected}, found {found}"
        ),
        TypeError::DuplicateName { name, .. } => format!("name '{name}' is already declared"),
        TypeError::ImmutableAssignment { name, .. } => {
            format!("cannot assign to immutable value '{name}'")
        }
        TypeError::CannotMutateCapturedVariable { name, .. } => {
            format!("cannot mutate captured variable '{name}'")
        }
        TypeError::RequiresMutablePlace { name, .. } => {
            format!("cannot mutably borrow non-storage place '{name}'")
        }
        TypeError::VarPatternRequiresMutablePlace { .. } => {
            "var pattern requires a mutable place".to_string()
        }
        TypeError::InvalidOperand {
            op, operand_type, ..
        } => format!("Invalid operand type: operator '{op}' cannot be applied to '{operand_type}'"),
        TypeError::MissingReturn { expected, .. } => {
            format!("Mismatched types: expected '{expected}', found 'void'")
        }
        TypeError::IfWithoutElseValue { .. } => {
            "if expression used as value must have an else branch".to_string()
        }
        TypeError::UnusedValue { .. } => "unused value".to_string(),
        TypeError::IfConditionNotBool { found, .. } => {
            format!("Condition of if expression must be bool: found '{found}'")
        }
        TypeError::TernaryConditionNotBool { found, .. } => {
            format!("Condition of ternary expression must be bool: found '{found}'")
        }
        TypeError::WhileConditionNotBool { found, .. } => {
            format!("Condition of while must be bool: found '{found}'")
        }
        TypeError::BreakOutsideLoop { .. } => "break outside of loop".to_string(),
        TypeError::ContinueOutsideLoop { .. } => "continue outside of loop".to_string(),
        TypeError::ReturnInsideDefer { .. } => "return inside defer".to_string(),
        TypeError::BreakInsideDefer { .. } => "break inside defer".to_string(),
        TypeError::ContinueInsideDefer { .. } => "continue inside defer".to_string(),
        TypeError::TryOnNonResult { found, .. } => {
            format!("try requires Result, found '{found}'")
        }
        TypeError::TryOutsideResultFunction { found: None, .. } => {
            "try requires an enclosing Result-returning function".to_string()
        }
        TypeError::TryOutsideResultFunction {
            found: Some(found), ..
        } => format!("try requires enclosing function to return Result, found '{found}'"),
        TypeError::TryErrorMismatch {
            expected, found, ..
        } => format!("try error type mismatch: expected '{expected}', found '{found}'"),
        TypeError::TryInsideDefer { .. } => "try inside defer".to_string(),
        TypeError::ForIterableNotSupported { found, .. } => {
            format!("type '{found}' cannot be iterated")
        }
        TypeError::ForIterationModifier { message, .. } => (*message).to_string(),
        TypeError::UnsupportedPattern { pattern, .. } => format!("Unsupported pattern: {pattern}"),
        TypeError::TuplePatternArityMismatch {
            expected, found, ..
        } => {
            format!("tuple pattern arity mismatch: expected {expected}, found {found}")
        }
        TypeError::TuplePatternOnNonTuple { ty, .. } => {
            format!("cannot destructure non-tuple type '{ty}'")
        }
        TypeError::OrPatternUnsupported { .. } => "or-patterns are not supported".to_string(),
        TypeError::EmptyMatch { .. } => "match expression must have at least one arm".to_string(),
        TypeError::NonExhaustiveMatch { .. } => "non-exhaustive match".to_string(),
        TypeError::UnsupportedMatchScrutinee { found, .. } => {
            format!("unsupported match scrutinee: '{found}'")
        }
        TypeError::InvalidLiteralPattern {
            expected, found, ..
        } => format!("invalid literal pattern: mismatch expected '{expected}', found '{found}'"),
        TypeError::OptionalPatternOnNonOptional { .. } => {
            "optional pattern requires an optional scrutinee".to_string()
        }
        TypeError::OptionalChainingOnNonOptional { .. } => {
            "optional chaining requires an optional base type".to_string()
        }
        TypeError::NestedOptionalPattern { .. } => {
            "nested optional patterns are not supported".to_string()
        }
        TypeError::MatchArmTypeMismatch {
            expected, found, ..
        } => format!("match arm type mismatch: expected '{expected}', found '{found}'"),
        TypeError::RequiresUnwrappingPattern { .. } => {
            "optional value requires an unwrapping pattern".to_string()
        }
        TypeError::IrrefutableLetElse { .. } => "irrefutable pattern in let-else".to_string(),
        TypeError::LetElseMustDiverge { .. } => {
            "the else block of a let-else must always return, break, or continue".to_string()
        }
        TypeError::MemberAccessOnNonAggregate {
            ty, member, kind, ..
        }
        | TypeError::UnknownMember {
            ty, member, kind, ..
        } => {
            let kind = render_member_access_kind(*kind);
            format!("Unknown {kind} '{member}' for type '{ty}'")
        }
        TypeError::InstanceMethodOnType { ty, method, .. } => {
            format!("instance method '{method}' requires a value of type '{ty}'")
        }
        TypeError::StaticMethodOnValue { ty, method, .. } => {
            format!("static method '{method}' must be called on type '{ty}'")
        }
        TypeError::ReadonlyMethodMutation { .. } => {
            "readonly method cannot mutate self".to_string()
        }
        TypeError::InternalAccess {
            kind,
            name,
            owner,
            reason,
            ..
        } => render_internal_access(*kind, *name, owner, reason.as_deref()),
        TypeError::UnknownIntrinsic { name, .. } => format!("unknown intrinsic '#{name}'"),
        TypeError::IntrinsicArgCount {
            name,
            expected,
            found,
            ..
        } => format!("intrinsic '#{name}' expects {expected} argument(s), found {found}"),
        TypeError::IntrinsicExpectedIdent { name, .. } => {
            format!("intrinsic '#{name}' expects an identifier argument")
        }
        TypeError::IntrinsicExpectedString { name, .. } => {
            format!("intrinsic '#{name}' expects a string literal argument")
        }
        TypeError::UnknownIntrinsicValue {
            predicate, value, ..
        } => format!("unknown {predicate} '{value}'"),
        TypeError::CompileError { message, .. } => message.clone(),
        TypeError::MethodGenericShadow {
            owner_kind,
            method_param,
            owner_param,
            name,
            ..
        } => format!(
            "method {} parameter shadows {} {} parameter '{name}'",
            method_param.keyword(),
            owner_kind.keyword(),
            owner_param.keyword()
        ),
        TypeError::TupleIndexOnNonTuple { ty, index, .. } => {
            format!("cannot index non-tuple type with .{index}: found '{ty}'")
        }
        TypeError::TupleIndexOutOfBounds { index, len, .. } => {
            format!("tuple index {index} is out of bounds for tuple of length {len}")
        }
        TypeError::IndexNotInt { found, .. } => {
            format!("index must be an integer: found '{found}'")
        }
        TypeError::IndexOnNonIndexable { found, .. } => {
            format!("cannot index non-array type '{found}'")
        }
        TypeError::RangeIndexNotInt { found, .. } => {
            format!("range index bounds must be int, found '{found}'")
        }
        TypeError::RangeIndexUnsupported { found, .. } => {
            format!("range indexing is not supported for type '{found}'")
        }
        TypeError::NonKeyableMapKey { ty, field, .. } => match field {
            Some(field) => {
                format!("field '{field}' is not keyable: type '{ty}' cannot be used as map key")
            }
            None if ty.is_option() => {
                format!("optional type '{ty}' is not keyable and cannot be used as map key")
            }
            None => format!("type '{ty}' is not keyable and cannot be used as map key"),
        },
        TypeError::DuplicateMapKey { .. } => "duplicate key in map literal".to_string(),
        TypeError::UndefinedModuleMember { module, name, .. } => {
            format!(
                "Unknown member '{name}' in module '{}'",
                render_module_scope(module)
            )
        }
        TypeError::PrivateModuleMember { module, name, .. } => {
            format!(
                "member '{name}' in module '{}' is private",
                render_module_scope(module)
            )
        }
        TypeError::AmbiguousExtendMethod { receiver, name, .. } => {
            format!("ambiguous method '{name}' for type '{receiver}'")
        }
        TypeError::DuplicateField { name, .. } => format!("Duplicate field '{name}'"),
        TypeError::MissingField { name, .. } => format!("Missing field '{name}'"),
        TypeError::UnknownVariantField {
            enum_name,
            variant,
            field,
            ..
        } => format!("Unknown field '{field}' for variant '{enum_name}.{variant}'"),
        TypeError::MissingVariantField {
            enum_name,
            variant,
            field,
            ..
        } => format!("Missing field '{field}' in variant '{enum_name}.{variant}'"),
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
        TypeError::EnumPatternTypeMismatch {
            expected, found, ..
        } => {
            format!("pattern does not match scrutinee enum: expected '{expected}', found '{found}'")
        }
        TypeError::EnumVariantShapeMismatch {
            enum_name,
            variant,
            expected,
            ..
        } => format!(
            "variant '{enum_name}.{variant}' is not a {} variant",
            render_variant_shape(*expected)
        ),
        TypeError::GenericArity(ArityError::TypeArgs { expected, found }) => {
            format!("wrong number of type parameters: expected {expected}, found {found}")
        }
        TypeError::UnboundGenericParam { name, .. } => {
            format!("Could not infer type parameter '{name}'")
        }
        TypeError::UnknownConst { name, .. } => format!("unknown constant '{name}'"),
        TypeError::ConstCycle { name, .. } => format!("constant '{name}' depends on itself"),
        TypeError::NonConstExpression { .. } => "not a constant expression".to_string(),
        TypeError::GenericFieldDefault { .. } => {
            "field default cannot depend on generic parameters".to_string()
        }
        TypeError::ConstTypeMismatch {
            expected, found, ..
        } => {
            format!("constant type mismatch: expected '{expected}', found '{found}'")
        }
        TypeError::InvalidConstCast { from, to, .. } => {
            format!("cannot cast constant from '{from}' to '{to}'")
        }
        TypeError::InvalidCast { from, to, .. } => {
            format!("Invalid cast: cannot cast from '{from}' to '{to}'")
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

fn render_variant_shape(shape: VariantShape) -> &'static str {
    match shape {
        VariantShape::Unit => "unit",
        VariantShape::Tuple => "tuple",
        VariantShape::Struct => "struct",
    }
}

fn render_type_mismatch(expected: &Type, found: &Type) -> String {
    let ambiguous = expected != found && expected.to_string() == found.to_string();
    let expected = render_mismatch_type(expected, ambiguous);
    let found = render_mismatch_type(found, ambiguous);
    format!("Mismatched types: expected '{expected}', found '{found}'")
}

fn render_mismatch_type(ty: &Type, detailed: bool) -> String {
    if !detailed {
        return ty.to_string();
    }
    render_detailed_type(ty)
}

fn render_detailed_type(ty: &Type) -> String {
    match ty {
        Type::Func { params, ret } => render_detailed_func(params, ret),
        Type::Tuple(elems) => render_wrapped_types("(", ")", elems),
        Type::Nominal(nominal) => {
            let mut rendered = nominal.origin.as_ref().map_or_else(
                || nominal.name.to_string(),
                |origin| format!("{}.{}", render_nominal_origin(origin), nominal.name),
            );
            render_detailed_generic_args(&mut rendered, &nominal.type_args, &nominal.const_args);
            rendered
        }
        Type::List { elem } => format!("[{}]", render_detailed_type(elem)),
        Type::Array { elem, len } => format!("[{}; {len}]", render_detailed_type(elem)),
        Type::Map { key, value } => format!(
            "[{}: {}]",
            render_detailed_type(key),
            render_detailed_type(value)
        ),
        Type::Slice { elem } => format!("slice[{}]", render_detailed_type(elem)),
        Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => {
            let mut rendered = render_qualified_name(*qualifier, *name);
            if !generic_args.is_empty() {
                let args = generic_args
                    .iter()
                    .map(render_detailed_generic_arg)
                    .collect::<Vec<_>>()
                    .join(", ");
                rendered.push('<');
                rendered.push_str(&args);
                rendered.push('>');
            }
            rendered
        }
        _ => ty.to_string(),
    }
}

fn render_detailed_func(params: &[FuncParam], ret: &Type) -> String {
    let params = params
        .iter()
        .map(|param| {
            let ty = render_detailed_type(&param.ty);
            if param.mutable {
                format!("mut {ty}")
            } else {
                ty
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    let mut rendered = format!("fn({params})");
    if !ret.is_void() {
        rendered.push_str(" -> ");
        rendered.push_str(&render_detailed_type(ret));
    }
    rendered
}

fn render_wrapped_types(open: &str, close: &str, elems: &[Type]) -> String {
    let elems = elems
        .iter()
        .map(render_detailed_type)
        .collect::<Vec<_>>()
        .join(", ");
    format!("{open}{elems}{close}")
}

fn render_detailed_generic_args(rendered: &mut String, types: &[Type], consts: &[ConstArg]) {
    if types.is_empty() && consts.is_empty() {
        return;
    }
    let mut args = types.iter().map(render_detailed_type).collect::<Vec<_>>();
    args.extend(consts.iter().map(ToString::to_string));
    rendered.push('<');
    rendered.push_str(&args.join(", "));
    rendered.push('>');
}

fn render_detailed_generic_arg(arg: &crate::ast::GenericArg) -> String {
    match arg {
        crate::ast::GenericArg::Type(ty) => render_detailed_type(ty),
        crate::ast::GenericArg::Const(arg) => arg.to_string(),
    }
}

fn render_nominal_origin(origin: &ModuleOrigin) -> String {
    match origin {
        ModuleOrigin::Module(path) => path.join("."),
        ModuleOrigin::SourceFile { path, .. } => path.clone(),
        ModuleOrigin::Package { package, path } => {
            let path = path
                .as_ref()
                .map_or_else(|| "<root>".to_string(), |path| path.join("."));
            format!("{package}:{path}")
        }
        ModuleOrigin::Provider { package, path } => format!("{package}:ext:{}", path.join(".")),
    }
}

fn render_extern_catalog_error(error: &ExternCatalogError) -> String {
    let (context, message) = match error {
        ExternCatalogError::UnknownType {
            context,
            module,
            name,
            ..
        } => {
            let ty = module.as_ref().map_or_else(
                || name.to_string(),
                |module| render_scoped_name(module, *name),
            );
            (
                context,
                format!(
                    "Unknown extern type '{ty}' in {}",
                    render_extern_item(context)
                ),
            )
        }
        ExternCatalogError::GenericArity {
            context,
            name,
            expected,
            found,
            ..
        } => (
            context,
            format!(
                "wrong number of extern type arguments for '{name}' in {}: expected {expected}, found {found}",
                render_extern_item(context)
            ),
        ),
        ExternCatalogError::GenericArgKindMismatch {
            context,
            name,
            expected,
            ..
        } => (
            context,
            format!(
                "expected {expected} extern generic argument for '{name}' in {}",
                render_extern_item(context)
            ),
        ),
        ExternCatalogError::InvalidType {
            context,
            ty,
            reason,
            ..
        } => {
            let item = render_extern_item(context);
            let message = match reason {
                InvalidExternTypeReason::Unresolved => {
                    format!("unresolved extern type '{ty}' in {item}")
                }
                InvalidExternTypeReason::Infer => {
                    format!("extern type '{ty}' contains inference in {item}")
                }
                InvalidExternTypeReason::Void => {
                    format!("void type is not allowed in extern type position '{ty}' in {item}")
                }
                InvalidExternTypeReason::UnresolvedConst => {
                    format!("extern type '{ty}' contains an unresolved const argument in {item}")
                }
            };
            (context, message)
        }
        ExternCatalogError::UnknownInitField { context, field, .. } => (
            context,
            format!(
                "{} references unknown field '{field}'",
                render_extern_item(context)
            ),
        ),
        ExternCatalogError::ComputedInitField { context, field, .. } => (
            context,
            format!(
                "{} cannot initialize computed field '{field}'",
                render_extern_item(context)
            ),
        ),
        ExternCatalogError::UnsupportedInitParams { context, count, .. } => (
            context,
            format!(
                "{} does not support parameters: found {count} parameter(s)",
                render_extern_item(context)
            ),
        ),
        ExternCatalogError::InvalidOperatorReturn {
            context,
            found,
            expected,
            ..
        } => (
            context,
            format!(
                "invalid {}: expected {} return type, found '{found}'",
                render_extern_item(context),
                render_operator_return(*expected)
            ),
        ),
    };
    format!(
        "{message} from {}",
        render_extern_provenance(&context.provenance)
    )
}

fn render_extern_item(context: &ExternCatalogContext) -> String {
    match &context.item {
        ExternContextItem::Function { name } => {
            format!(
                "extern function {}",
                render_extern_context_name(context, *name)
            )
        }
        ExternContextItem::Type { name } => {
            format!("extern type {}", render_extern_context_name(context, *name))
        }
        ExternContextItem::Field { ty, field } => {
            format!(
                "extern field {}",
                render_extern_context_member(context, *ty, *field)
            )
        }
        ExternContextItem::Init { ty } => {
            format!("extern init {}", render_extern_context_name(context, *ty))
        }
        ExternContextItem::Method { ty, method } => {
            format!(
                "extern method {}",
                render_extern_context_member(context, *ty, *method)
            )
        }
        ExternContextItem::Static { ty, method } => {
            format!(
                "extern static {}",
                render_extern_context_member(context, *ty, *method)
            )
        }
        ExternContextItem::Operator { ty, op } => {
            let owner = render_extern_context_name(context, *ty);
            format!("extern operator {owner}.{}", render_extern_operator(*op))
        }
    }
}

fn render_extern_context_name(context: &ExternCatalogContext, name: Ident) -> String {
    if is_source_extern_context(context) {
        name.to_string()
    } else {
        render_scoped_name(&context.module, name)
    }
}

fn render_extern_context_member(
    context: &ExternCatalogContext,
    ty: Ident,
    member: Ident,
) -> String {
    format!("{}.{}", render_extern_context_name(context, ty), member)
}

fn is_source_extern_context(context: &ExternCatalogContext) -> bool {
    match &context.provenance {
        ExternProvenance::Source { module } => raw_module_scope(module) == context.module,
        ExternProvenance::Provider { .. } => false,
    }
}

fn render_scoped_name(module: &ModuleScope, name: Ident) -> String {
    match module {
        ModuleScope::Root => name.to_string(),
        ModuleScope::Named(path) => format!("{}.{name}", render_module_path(path)),
        ModuleScope::Package(module) => format!("{}.{name}", render_module_id(module)),
    }
}

fn render_member_access_kind(kind: MemberAccessKind) -> &'static str {
    match kind {
        MemberAccessKind::Field => "field",
        MemberAccessKind::Method => "method",
    }
}

fn render_internal_access(
    kind: MemberAccessKind,
    name: Ident,
    owner: &Type,
    reason: Option<&str>,
) -> String {
    let kind = render_member_access_kind(kind);
    match reason {
        Some(reason) => format!("accessing internal {kind} '{name}' of type '{owner}': {reason}"),
        None => format!("accessing internal {kind} '{name}' of type '{owner}'"),
    }
}

fn render_deprecated_access(kind: DeprecatedUseKind, name: Ident, reason: Option<&str>) -> String {
    let kind = render_deprecated_use_kind(kind);
    match reason {
        Some(reason) => format!("use of deprecated {kind} '{name}': {reason}"),
        None => format!("use of deprecated {kind} '{name}'"),
    }
}

fn render_deprecated_use_kind(kind: DeprecatedUseKind) -> &'static str {
    match kind {
        DeprecatedUseKind::Function => "function",
        DeprecatedUseKind::ExternFunction => "extern function",
        DeprecatedUseKind::Const => "const",
        DeprecatedUseKind::ExternType => "extern type",
        DeprecatedUseKind::Struct => "struct",
        DeprecatedUseKind::DataRef => "dataref",
        DeprecatedUseKind::Enum => "enum",
        DeprecatedUseKind::EnumVariant => "variant",
        DeprecatedUseKind::Field => "field",
        DeprecatedUseKind::Method => "method",
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
        DeclError::DuplicateAggregateMethod {
            owner,
            name,
            surface,
            ..
        } => format!(
            "duplicate {} method '{name}' on type '{}'",
            surface.label(),
            owner.name
        ),
        DeclError::DuplicateExtendMethod { name, surface, .. } => {
            format!("duplicate extend {} method '{name}'", surface.label())
        }
        DeclError::DuplicateCastFrom { source, target, .. } => {
            format!("duplicate cast from '{source}' to '{target}'")
        }
        DeclError::PointlessCastFrom { ty, .. } => {
            format!("pointless cast from '{ty}' to itself")
        }
        DeclError::CastFromReturnMismatch {
            expected, found, ..
        } => {
            format!("cast from return type mismatch: expected '{expected}', found '{found}'")
        }
        DeclError::UnsupportedExtendTarget { ty, .. } => {
            format!("cannot extend type '{ty}'")
        }
        DeclError::UnusedExtendTypeParam { name, .. } => {
            format!("unused type parameter '{name}' in extend target")
        }
        DeclError::UnusedExtendConstParam { name, .. } => {
            format!("unused const parameter '{name}' in extend target")
        }
        DeclError::ExtendMethodConflict {
            ty, name, surface, ..
        } => format!(
            "{} method '{name}' already exists for type '{ty}'",
            surface.label()
        ),
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
        DeclError::UnknownAnnotation { name, .. } => format!("unknown annotation `@{name}`"),
        DeclError::InvalidAnnotationTarget {
            name,
            target,
            valid_targets,
            ..
        } => format!(
            "`@{name}` is not valid on {target} declarations: can only be applied to {valid_targets}"
        ),
        DeclError::DuplicateAnnotation { name, .. } => format!("duplicate annotation `@{name}`"),
        DeclError::InvalidAnnotationArgs { name, message, .. } => {
            format!("invalid arguments for `@{name}`: {message}")
        }
        DeclError::InternalOnToString { .. } => {
            "`@internal` cannot be applied to `to_string` methods".to_string()
        }
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
        ExternDescriptorError::UnsupportedInitParams { ty, count } => format!(
            "extern init parameters are not supported on type '{}': found {count} parameter(s)",
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
    match raw_scope {
        Some(scope) => render_raw_scoped_name(scope, &key.name),
        None => format!("{}.{}", render_extern_module_path(&key.module), key.name),
    }
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
                rendered.push('<');
                rendered.push_str(&args);
                rendered.push('>');
            }
            rendered
        }
        ExternTypeExpr::Callback(_) => "callback".to_string(),
    }
}

fn render_module_path(path: &ModulePath) -> String {
    path.segments().join(".")
}

fn render_source_file(file: &SourceFileId) -> String {
    file.path()
        .file_stem()
        .unwrap_or_else(|| file.path().as_os_str())
        .to_string_lossy()
        .into_owned()
}

fn render_module_id(module: &ModuleId) -> String {
    let path = match module.path() {
        PackageModulePath::Root => "<root>".to_string(),
        PackageModulePath::Named(path) => render_module_path(path),
        PackageModulePath::Provider(path) => format!("ext:{}", render_module_path(path)),
        PackageModulePath::Source(file) => render_source_file(file),
    };
    match module.package_context() {
        Some(package) if package != &PackageId::synthetic_root() => {
            format!("{package}:{path}")
        }
        _ => path,
    }
}

fn render_module_scope(scope: &ModuleScope) -> String {
    match scope {
        ModuleScope::Root => "<root>".to_string(),
        ModuleScope::Named(path) => render_module_path(path),
        ModuleScope::Package(module) => render_module_id(module),
    }
}

#[cfg(test)]
mod tests {
    use anvyx_externs::{BinaryOp, ExternMemberSelector, ProviderId};
    use chumsky::error::{LabelError, RichPattern};

    use super::*;
    use crate::{
        ast::{Ident, Type},
        externs::RawExternSite,
        lexer::Token,
        span::Span,
    };

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn module_path(path: &[&str]) -> ModulePath {
        ModulePath::new(path.iter().map(ToString::to_string).collect()).unwrap()
    }

    fn module_id(path: &[&str]) -> ModuleId {
        ModuleId::named(PackageId::synthetic_root(), module_path(path))
    }

    fn root_module_id() -> ModuleId {
        ModuleId::root(PackageId::synthetic_root())
    }

    fn raw_root_scope() -> RawExternScope {
        RawExternScope::Module(root_module_id())
    }

    fn module_scope(path: &[&str]) -> ModuleScope {
        ModuleScope::Named(module_path(path))
    }

    fn source_module_scope(path: &str) -> ModuleScope {
        ModuleScope::Package(ModuleId::source_without_package(
            SourceFileId::new(path).unwrap(),
        ))
    }

    fn catalog_context(module: &[&str], item: ExternContextItem) -> ExternCatalogContext {
        ExternCatalogContext {
            provenance: ExternProvenance::Source {
                module: raw_root_scope(),
            },
            module: module_scope(module),
            item,
        }
    }

    fn source_decl(module: RawExternScope) -> RawExternDecl {
        RawExternDecl {
            provenance: ExternProvenance::Source { module },
            site: RawExternSite::default(),
        }
    }

    fn provider_decl(name: &str) -> RawExternDecl {
        RawExternDecl {
            provenance: ExternProvenance::Provider {
                package: PackageId::synthetic_root(),
                provider: ProviderId {
                    name: name.to_string(),
                },
            },
            site: RawExternSite::default(),
        }
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
    fn renders_deprecated_warnings() {
        let cases = [
            (
                DeprecatedUseKind::Function,
                "use of deprecated function 'old'",
            ),
            (
                DeprecatedUseKind::ExternFunction,
                "use of deprecated extern function 'old'",
            ),
            (DeprecatedUseKind::Const, "use of deprecated const 'old'"),
            (
                DeprecatedUseKind::ExternType,
                "use of deprecated extern type 'old'",
            ),
            (DeprecatedUseKind::Struct, "use of deprecated struct 'old'"),
            (DeprecatedUseKind::Enum, "use of deprecated enum 'old'"),
            (
                DeprecatedUseKind::EnumVariant,
                "use of deprecated variant 'old'",
            ),
            (DeprecatedUseKind::Field, "use of deprecated field 'old'"),
            (DeprecatedUseKind::Method, "use of deprecated method 'old'"),
        ];

        for (kind, expected) in cases {
            assert_msg(
                diagnose_type_warning(&TypeWarning::DeprecatedAccess {
                    kind,
                    name: ident("old"),
                    reason: None,
                    span: span(),
                }),
                expected,
            );
        }
        assert_msg(
            diagnose_type_warning(&TypeWarning::DeprecatedAccess {
                kind: DeprecatedUseKind::DataRef,
                name: ident("old"),
                reason: Some("use new".to_string()),
                span: span(),
            }),
            "use of deprecated dataref 'old': use new",
        );
    }

    #[test]
    fn renders_resolve_errors() {
        let cases = [
            (
                diagnose_resolve_error(&ResolveError::ModuleNotFound {
                    module: module_id(&["foo", "bar"]),
                    span: span(),
                }),
                "Cannot find module file for module 'foo.bar'",
            ),
            (
                diagnose_resolve_error(&ResolveError::LoadFailed {
                    module: module_id(&["foo", "bar"]),
                    span: span(),
                    message: "permission denied".to_string(),
                }),
                "Cannot load module 'foo.bar': permission denied",
            ),
            (
                diagnose_resolve_error(&ResolveError::DuplicatePreloadedModule {
                    module: module_id(&["core", "math"]),
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
                diagnose_type_error(&TypeError::Decl(DeclError::MissingImportMember {
                    module: ModuleScope::Root,
                    imported: source_module_scope("/tmp/lib.anv"),
                    name: ident("missing"),
                    span: span(),
                })),
                "Unknown member 'missing' in module 'lib'",
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

    fn package_nominal(package: &str, name: &str) -> Type {
        Type::nominal_with_origin(
            crate::ast::NominalKind::Struct,
            ident(name),
            vec![],
            vec![],
            Some(ModuleOrigin::Package {
                package: package.to_string(),
                path: None,
            }),
        )
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
                diagnose_type_error(&TypeError::TypeMismatch {
                    expected: package_nominal("left", "Vec2"),
                    found: package_nominal("right", "Vec2"),
                    span: span(),
                }),
                "Mismatched types: expected 'left:<root>.Vec2', found 'right:<root>.Vec2'",
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
            (
                diagnose_type_error(&TypeError::AnyOutsideExternBoundary { span: span() }),
                "any is only allowed in extern boundary signatures",
            ),
            (
                diagnose_type_error(&TypeError::ExternAnyEscape { span: span() }),
                "extern 'any' value cannot escape an extern boundary",
            ),
        ];

        for (diagnostic, expected) in cases {
            assert_msg(diagnostic, expected);
        }
    }

    #[test]
    fn renders_extern_catalog_errors() {
        let cases = [
            (
                diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
                    context: catalog_context(
                        &["host"],
                        ExternContextItem::Function {
                            name: ident("tick"),
                        },
                    ),
                    module: None,
                    name: ident("Missing"),
                    site: RawExternSite::default(),
                })),
                "Unknown extern type 'Missing' in extern function host.tick from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::InvalidType {
                    context: catalog_context(
                        &["host"],
                        ExternContextItem::Type {
                            name: ident("Handle"),
                        },
                    ),
                    ty: Type::Infer,
                    reason: InvalidExternTypeReason::Infer,
                    site: RawExternSite::default(),
                })),
                "extern type '_' contains inference in extern type host.Handle from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(
                    ExternCatalogError::UnknownInitField {
                        context: catalog_context(
                            &["host"],
                            ExternContextItem::Init {
                                ty: ident("Handle"),
                            },
                        ),
                        field: ident("missing"),
                        site: RawExternSite::default(),
                    },
                )),
                "extern init host.Handle references unknown field 'missing' from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(
                    ExternCatalogError::ComputedInitField {
                        context: catalog_context(
                            &["host"],
                            ExternContextItem::Init {
                                ty: ident("Handle"),
                            },
                        ),
                        field: ident("x"),
                        site: RawExternSite::default(),
                    },
                )),
                "extern init host.Handle cannot initialize computed field 'x' from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(
                    ExternCatalogError::UnsupportedInitParams {
                        context: catalog_context(
                            &["host"],
                            ExternContextItem::Init {
                                ty: ident("Handle"),
                            },
                        ),
                        count: 2,
                        site: RawExternSite::default(),
                    },
                )),
                "extern init host.Handle does not support parameters: found 2 parameter(s) from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
                    context: catalog_context(
                        &["host"],
                        ExternContextItem::Method {
                            ty: ident("Handle"),
                            method: ident("move_by"),
                        },
                    ),
                    module: None,
                    name: ident("Missing"),
                    site: RawExternSite::default(),
                })),
                "Unknown extern type 'Missing' in extern method host.Handle.move_by from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
                    context: catalog_context(
                        &["host"],
                        ExternContextItem::Static {
                            ty: ident("Handle"),
                            method: ident("make"),
                        },
                    ),
                    module: None,
                    name: ident("Missing"),
                    site: RawExternSite::default(),
                })),
                "Unknown extern type 'Missing' in extern static host.Handle.make from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(
                    ExternCatalogError::InvalidOperatorReturn {
                        context: catalog_context(
                            &["math"],
                            ExternContextItem::Operator {
                                ty: ident("Vec2"),
                                op: ExternOperator::Binary {
                                    op: BinaryOp::Eq,
                                    self_on_right: false,
                                },
                            },
                        ),
                        found: Type::Int,
                        expected: OperatorReturn::Bool,
                        site: RawExternSite::default(),
                    },
                )),
                "invalid extern operator math.Vec2.==: expected bool return type, found 'int' from source root",
            ),
            (
                diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::InvalidType {
                    context: catalog_context(
                        &["host"],
                        ExternContextItem::Field {
                            ty: ident("Handle"),
                            field: ident("id"),
                        },
                    ),
                    ty: Type::Void,
                    reason: InvalidExternTypeReason::Void,
                    site: RawExternSite::default(),
                })),
                "void type is not allowed in extern type position 'void' in extern field host.Handle.id from source root",
            ),
        ];

        for (diagnostic, expected) in cases {
            assert_msg(diagnostic, expected);
        }
    }

    #[test]
    fn renders_root_extern_catalog_context_without_fake_module() {
        let context = ExternCatalogContext {
            provenance: ExternProvenance::Source {
                module: raw_root_scope(),
            },
            module: ModuleScope::Root,
            item: ExternContextItem::Function {
                name: ident("tick"),
            },
        };

        assert_msg(
            diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
                context,
                module: Some(ModuleScope::Root),
                name: ident("Missing"),
                site: RawExternSite::default(),
            })),
            "Unknown extern type 'Missing' in extern function tick from source root",
        );
    }

    #[test]
    fn renders_root_raw_identity_without_fake_module() {
        let decl = source_decl(raw_root_scope());

        assert_msg(
            diagnose_extern_input_error(&ExternInputError::DuplicateRawIdentity {
                key: RawExternIdentityKey::Function(RawExternFunctionKey {
                    module: raw_root_scope(),
                    name: "tick".to_string(),
                }),
                first: decl.clone(),
                duplicate: decl,
            }),
            "duplicate extern function 'tick' declared in source root and source root",
        );
    }

    #[test]
    fn renders_source_raw_identities_without_source_module_prefix() {
        let module = RawExternScope::Module(module_id(&["host"]));
        let decl = source_decl(module.clone());
        let op = ExternOperator::Binary {
            op: BinaryOp::Add,
            self_on_right: false,
        };
        let cases = vec![
            (
                RawExternIdentityKey::Function(RawExternFunctionKey {
                    module: module.clone(),
                    name: "ping".to_string(),
                }),
                "duplicate extern function 'ping' declared in source module 'host' and source module 'host'",
            ),
            (
                RawExternIdentityKey::Type(RawExternTypeKey {
                    module: module.clone(),
                    name: "Handle".to_string(),
                }),
                "duplicate extern type 'Handle' declared in source module 'host' and source module 'host'",
            ),
            (
                RawExternIdentityKey::Member(RawExternMemberKey {
                    owner: RawExternTypeKey {
                        module: module.clone(),
                        name: "Handle".to_string(),
                    },
                    selector: ExternMemberSelector::Field("id".to_string()),
                }),
                "duplicate extern field 'Handle.id' declared in source module 'host' and source module 'host'",
            ),
            (
                RawExternIdentityKey::Member(RawExternMemberKey {
                    owner: RawExternTypeKey {
                        module: module.clone(),
                        name: "Handle".to_string(),
                    },
                    selector: ExternMemberSelector::Method("get".to_string()),
                }),
                "duplicate extern method 'Handle.get' declared in source module 'host' and source module 'host'",
            ),
            (
                RawExternIdentityKey::Member(RawExternMemberKey {
                    owner: RawExternTypeKey {
                        module: module.clone(),
                        name: "Handle".to_string(),
                    },
                    selector: ExternMemberSelector::Static("make".to_string()),
                }),
                "duplicate extern static method 'Handle.make' declared in source module 'host' and source module 'host'",
            ),
            (
                RawExternIdentityKey::Member(RawExternMemberKey {
                    owner: RawExternTypeKey {
                        module,
                        name: "Vec2".to_string(),
                    },
                    selector: ExternMemberSelector::Operator(op),
                }),
                "duplicate extern operator 'Vec2.+' declared in source module 'host' and source module 'host'",
            ),
        ];

        for (key, expected) in cases {
            assert_msg(
                diagnose_extern_input_error(&ExternInputError::DuplicateRawIdentity {
                    key,
                    first: decl.clone(),
                    duplicate: decl.clone(),
                }),
                expected,
            );
        }
    }

    #[test]
    fn renders_provider_raw_identity_with_module_prefix() {
        let module = RawExternScope::Module(module_id(&["host"]));
        let decl = provider_decl("native");

        assert_msg(
            diagnose_extern_input_error(&ExternInputError::DuplicateRawIdentity {
                key: RawExternIdentityKey::Member(RawExternMemberKey {
                    owner: RawExternTypeKey {
                        module,
                        name: "Handle".to_string(),
                    },
                    selector: ExternMemberSelector::Field("id".to_string()),
                }),
                first: decl.clone(),
                duplicate: decl,
            }),
            "duplicate extern field 'host.Handle.id' declared in provider 'native' in package '<root>' and provider 'native' in package '<root>'",
        );
    }

    #[test]
    fn renders_source_extern_catalog_context_without_source_module_prefix() {
        let module = ModuleId::source_without_package(
            SourceFileId::new("/tmp/unresolved_callback_param_err.anv").unwrap(),
        );
        let context = ExternCatalogContext {
            provenance: ExternProvenance::Source {
                module: RawExternScope::Module(module.clone()),
            },
            module: ModuleScope::from_module_id(&module),
            item: ExternContextItem::Function {
                name: ident("apply"),
            },
        };

        assert_msg(
            diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
                context,
                module: None,
                name: ident("Missing"),
                site: RawExternSite::default(),
            })),
            "Unknown extern type 'Missing' in extern function apply from source module 'unresolved_callback_param_err'",
        );
    }

    #[test]
    fn renders_provider_extern_catalog_context_with_module_prefix() {
        let context = ExternCatalogContext {
            provenance: ExternProvenance::Provider {
                package: PackageId::synthetic_root(),
                provider: ProviderId {
                    name: "native".to_string(),
                },
            },
            module: module_scope(&["host"]),
            item: ExternContextItem::Field {
                ty: ident("Handle"),
                field: ident("id"),
            },
        };

        assert_msg(
            diagnose_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
                context,
                module: None,
                name: ident("Missing"),
                site: RawExternSite::default(),
            })),
            "Unknown extern type 'Missing' in extern field host.Handle.id from provider 'native' in package '<root>'",
        );
    }

    #[test]
    fn operator_return_messages_match() {
        let op = ExternOperator::Binary {
            op: BinaryOp::Eq,
            self_on_right: false,
        };
        let descriptor =
            diagnose_extern_input_error(&ExternInputError::InvalidProviderDescriptor {
                package: PackageId::synthetic_root(),
                provider: anvyx_externs::ProviderId {
                    name: "host".to_string(),
                },
                error: ExternDescriptorError::InvalidOperatorReturn {
                    ty: ExternTypeKey {
                        module: ExternModulePath {
                            segments: vec!["math".to_string()],
                        },
                        name: "Vec2".to_string(),
                    },
                    op,
                    expected: OperatorReturn::Bool,
                    actual: ExternTypeExpr::Int,
                },
            });
        let catalog = diagnose_type_error(&TypeError::ExternCatalog(
            ExternCatalogError::InvalidOperatorReturn {
                context: catalog_context(
                    &["math"],
                    ExternContextItem::Operator {
                        ty: ident("Vec2"),
                        op,
                    },
                ),
                found: Type::Int,
                expected: OperatorReturn::Bool,
                site: RawExternSite::default(),
            },
        ));

        assert_msg(
            descriptor,
            "invalid extern descriptor from provider 'host' in package '<root>': invalid operator '==' on extern type 'math.Vec2': expected bool return type, found 'int'",
        );
        assert_msg(
            catalog,
            "invalid extern operator math.Vec2.==: expected bool return type, found 'int' from source root",
        );
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
