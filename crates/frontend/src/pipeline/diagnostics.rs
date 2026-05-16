use anvyx_externs::{
    ExternDescriptorError, ExternOperator, ExternTypeExpr, ExternTypeKey,
    ModulePath as ExternModulePath, NameKind, OperatorReturn, TypeContext,
};
use chumsky::error::{Rich, RichPattern, RichReason};

use crate::{
    ast::{
        self, ConstArg, ConstValue, EscapeMode, FuncParam, Ident, ModuleOrigin, ReturnSpec, Type,
    },
    conditional::ConditionalError,
    diagnostic::Diagnostic,
    externs::{
        ExternInputError, ExternProvenance, RawExternDecl, RawExternFunctionKey,
        RawExternIdentityKey, RawExternMemberKey, RawExternScope, RawExternTypeKey,
        UnsupportedSourceKind, UnsupportedSourceParamReason,
        catalog::{
            ExternCatalogContext, ExternCatalogError, ExternContextItem, InvalidExternTypeReason,
        },
        raw_module_scope,
    },
    lexer::{Delimiter, Op, Token},
    resolve::{ModuleId, ModulePath, PackageId, PackageModulePath, ResolveError, SourceFileId},
    source::SourceId,
    span::SourceSpan,
    typecheck::{
        ArityError, BindingNamespace, BindingOrigin, CaptureStorageOrigin, CompileWarning,
        ConstDiagnostic, DeclError, DynContainerConversionKind, ModuleScope, TryCarrierKind,
        TypeError, VariantShape,
    },
};

pub(super) fn diagnose_lex_error(
    source: SourceId,
    source_len: usize,
    error: &Rich<'_, char>,
) -> Diagnostic {
    let (message, label) = match error.reason() {
        RichReason::Custom(message) => (message.clone(), message.clone()),
        RichReason::ExpectedFound { found, .. } => match found.as_deref() {
            Some(found) => (
                format!("Unexpected character '{found}'"),
                "unexpected character".to_string(),
            ),
            None => (
                "Unexpected end of input".to_string(),
                "end of file".to_string(),
            ),
        },
    };
    let diagnostic = Diagnostic::error(message);
    let span = error.span();
    if span.start <= span.end && span.end <= source_len {
        diagnostic.with_primary_message(SourceSpan::new(source, span.start, span.end), label)
    } else {
        diagnostic
    }
}

pub(super) fn diagnose_parse_error(error: &Rich<'_, Token, SourceSpan>) -> Diagnostic {
    let (message, label) = match error.reason() {
        RichReason::Custom(message) => (message.clone(), message.clone()),
        RichReason::ExpectedFound { expected, found } => {
            match parse_expected_context(expected, found.as_deref()) {
                Some((message, label)) => (message.to_string(), label.to_string()),
                None => match found.as_deref() {
                    Some(token) => (
                        format!("Unexpected token '{token}'"),
                        "unexpected token".to_string(),
                    ),
                    None => (
                        "Unexpected end of input".to_string(),
                        "end of file".to_string(),
                    ),
                },
            }
        }
    };
    Diagnostic::error(message).with_primary_message(*error.span(), label)
}

fn parse_expected_context(
    expected: &[RichPattern<'_, Token>],
    found: Option<&Token>,
) -> Option<(&'static str, &'static str)> {
    let expects_type = expected_label(expected, "type");
    let expects_return_type = expected_label(expected, "return type");
    let expects_name =
        expected_label(expected, "identifier") || expected_label(expected, "parameter");
    let expects_expression = expected_label(expected, "expression");

    if expects_return_type && matches!(found, None | Some(Token::Open(Delimiter::Brace))) {
        return Some(("expected return type", "expected a return type here"));
    }
    if expects_type
        && matches!(
            found,
            Some(Token::Comma | Token::Close(Delimiter::Parent) | Token::Op(Op::ThinArrow))
        )
    {
        return Some(("expected type after :", "expected a type here"));
    }
    if expects_type && matches!(found, Some(Token::Open(Delimiter::Brace))) {
        return Some(("expected type", "expected a type here"));
    }
    if expects_name && matches!(found, Some(Token::Colon | Token::Comma)) {
        return Some(("expected parameter name", "expected a parameter name here"));
    }
    if expects_expression {
        return Some(("expected expression", "expected an expression here"));
    }

    None
}

fn expected_label(expected: &[RichPattern<'_, Token>], label: &str) -> bool {
    expected.iter().any(|expected| match expected {
        RichPattern::Label(found) => found == label,
        _ => false,
    })
}

pub(super) fn diagnose_conditional_error(source: SourceId, error: &ConditionalError) -> Diagnostic {
    Diagnostic::error(error.message.clone()).with_primary_message(
        SourceSpan::from_byte_span(source, error.span),
        error.label.clone(),
    )
}

pub(super) fn diagnose_resolve_error(error: &ResolveError) -> Diagnostic {
    let (message, note) = match error {
        ResolveError::ModuleNotFound { module, .. } => (
            format!(
                "Cannot find module file for module '{}'",
                render_module_id(module)
            ),
            None,
        ),
        ResolveError::SourceImportNotFound {
            importer,
            path,
            candidate,
            ..
        } => (
            format!(
                "Cannot find source import '{}' from '{}'",
                render_module_path(path),
                importer
            ),
            candidate
                .as_ref()
                .map(|candidate| format!("looked for '{}'", candidate.display())),
        ),
        ResolveError::LoadFailed {
            module, message, ..
        } => (
            format!(
                "Cannot load module '{}': {message}",
                render_module_id(module)
            ),
            None,
        ),
        ResolveError::DuplicatePreloadedModule { module } => (
            format!(
                "module '{}' is preloaded more than once",
                render_module_id(module)
            ),
            None,
        ),
        ResolveError::UnknownDependency { alias, package, .. } => (
            format!("package '{package}' has no dependency named '{alias}'"),
            None,
        ),
        ResolveError::PackageImportUnavailable { file, alias, .. } => (
            format!("source file '{file}' has no package dependency named '{alias}'"),
            None,
        ),
        ResolveError::UnsupportedImportRoot { root, .. } => {
            (format!("import root '{root}' is not supported yet"), None)
        }
        ResolveError::NativeProviderUnavailable { package, .. } => (
            match package {
                Some(package) => format!("package '{package}' has no native provider modules"),
                None => "native provider imports require a package context".to_string(),
            },
            None,
        ),
        ResolveError::UnknownNativeProviderModule {
            package, module, ..
        } => (
            format!(
                "package '{package}' has no native provider module '{}'",
                render_module_path(module)
            ),
            None,
        ),
        ResolveError::UnknownNativeDepProviderModule {
            package,
            alias,
            module,
            ..
        } => (
            format!(
                "native-only dependency '{alias}' ({package}) has no native provider module '{}'",
                render_module_path(module)
            ),
            None,
        ),
        ResolveError::NativeOnlyPkgRootImport { package, alias, .. } => (
            format!("native-only dependency '{alias}' ({package}) has no source root to import"),
            None,
        ),
    };
    let diagnostic = Diagnostic::error(message);
    let diagnostic = match note {
        Some(note) => diagnostic.with_note(note),
        None => diagnostic,
    };
    match error.span() {
        Some(span) => diagnostic.with_primary_message(span, resolve_error_label(error)),
        None => diagnostic,
    }
}

fn resolve_error_label(error: &ResolveError) -> &'static str {
    match error {
        ResolveError::ModuleNotFound { .. } => "module file not found",
        ResolveError::SourceImportNotFound { .. } => "import resolved to no source file",
        ResolveError::LoadFailed { .. } => "module requested here",
        ResolveError::DuplicatePreloadedModule { .. } => "duplicate preloaded module",
        ResolveError::UnknownDependency { .. } => "unknown package dependency",
        ResolveError::PackageImportUnavailable { .. } => "package dependency unavailable here",
        ResolveError::UnsupportedImportRoot { .. } => "unsupported import root",
        ResolveError::NativeProviderUnavailable { .. } => "native provider requested here",
        ResolveError::UnknownNativeProviderModule { .. }
        | ResolveError::UnknownNativeDepProviderModule { .. } => {
            "native provider module requested here"
        }
        ResolveError::NativeOnlyPkgRootImport { .. } => "native-only package imported here",
    }
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
    label_extern_input_error(Diagnostic::error(message), error)
}

fn label_extern_input_error(diagnostic: Diagnostic, error: &ExternInputError) -> Diagnostic {
    match error {
        ExternInputError::InvalidRawDescriptor { decl, .. } => match decl.site.span {
            Some(span) => diagnostic.with_primary_message(span, "invalid extern declaration here"),
            None => diagnostic,
        },
        ExternInputError::DuplicateRawIdentity {
            first, duplicate, ..
        } => match (first.site.span, duplicate.site.span) {
            (Some(first), Some(duplicate)) => diagnostic
                .with_primary_message(duplicate, "duplicate extern declaration")
                .with_secondary_message(first, "first declared here"),
            (Some(span), None) | (None, Some(span)) => {
                diagnostic.with_primary_message(span, "duplicate extern declaration")
            }
            (None, None) => diagnostic,
        },
        ExternInputError::UnsupportedSource { kind, span } => {
            diagnostic.with_primary_message(*span, unsupported_source_label(kind))
        }
        ExternInputError::InvalidProviderDescriptor { .. }
        | ExternInputError::DuplicateProviderModule { .. } => diagnostic,
    }
}

fn unsupported_source_label(kind: &UnsupportedSourceKind) -> &'static str {
    match kind {
        UnsupportedSourceKind::Type(_) => "unsupported source extern type here",
        UnsupportedSourceKind::InferReturn => "inferred return type used here",
        UnsupportedSourceKind::Operator(_) => "unsupported source extern operator here",
        UnsupportedSourceKind::Param { .. } => "unsupported source extern parameter here",
        UnsupportedSourceKind::CallbackParam { .. } => {
            "unsupported source extern callback parameter here"
        }
    }
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

pub(super) fn diagnose_compile_warning(warning: &CompileWarning) -> Diagnostic {
    Diagnostic::warning(warning.message.clone())
        .with_primary_message(warning.span, "compile warning emitted here")
}

pub(super) fn diagnose_type_error(error: &TypeError) -> Diagnostic {
    let span = type_error_span(error);
    if let Some((message, label)) = type_error_rich_message(error) {
        let diagnostic = Diagnostic::error(message);
        return match span {
            Some(span) => diagnostic.with_primary_message(span, label),
            None => Diagnostic::error(format!("{message}: {label}")),
        };
    }

    let diagnostic = Diagnostic::error(match error {
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
        TypeError::ContractUnsatisfied {
            ty,
            contract,
            detail,
            ..
        } => format!("{ty} does not satisfy {contract}: {detail}"),
        TypeError::DynamicMethodMissing {
            contract, method, ..
        } => format!("dynamic contract '{contract}' has no method '{method}'"),
        TypeError::BorrowedDynReassign { name, .. } => {
            format!("cannot reassign borrowed dynamic parameter '{name}'")
        }
        TypeError::DynContainerConversion { kind, .. } => render_dyn_container_conversion(*kind),
        TypeError::RecursiveInference { .. } => {
            "recursive type inference is not allowed".to_string()
        }
        TypeError::CannotInferType { .. } => "Could not infer type".to_string(),
        TypeError::CannotInferEnum { .. } => "cannot infer enum type".to_string(),
        TypeError::NamedFunctionCapture { name, .. } => {
            format!("named functions cannot capture local value '{name}'")
        }
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
        TypeError::UnsupportedPlaceReturn { message, .. }
        | TypeError::ForIterationModifier { message, .. } => (*message).to_string(),
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
        TypeError::ConstAssignment { name, .. } => format!("cannot assign to constant '{name}'"),
        TypeError::VarArgNonLvalue { .. } => {
            "non-lvalue cannot be passed to var parameter".to_string()
        }
        TypeError::VarArgImmutableBinding { name, .. } => {
            format!("immutable binding '{name}' cannot be passed to var parameter")
        }
        TypeError::MutatingMethodImmutableReceiver { name, .. } => {
            format!("mutating method requires mutable receiver '{name}'")
        }
        TypeError::MutableAlias { .. } => {
            "var arguments must not alias the same variable".to_string()
        }
        TypeError::InvalidFormatSpec { reason, .. } => {
            format!("invalid format specifier: {reason}")
        }
        TypeError::NonEscapingCallbackEscapes { name, .. } => {
            format!("callback parameter '{name}' is non-escaping by default, but it escapes here")
        }
        TypeError::BorrowedCaptureEscapes { name, origin, .. } => {
            borrowed_capture_escape_message(*name, *origin)
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
        TypeError::TryOnInvalidCarrier {
            expected, found, ..
        } => {
            format!("try requires {}, found '{found}'", expected.label())
        }
        TypeError::TryOutsideCarrierFunction { found: None, .. } => {
            format!(
                "try requires enclosing function to return {}",
                TryCarrierKind::any_label()
            )
        }
        TypeError::TryOutsideCarrierFunction {
            found: Some(found), ..
        } => format!(
            "try requires enclosing function to return {}, found '{found}'",
            TryCarrierKind::any_label()
        ),
        TypeError::TryResultErrorMismatch {
            expected, found, ..
        } => format!("try error type mismatch: expected '{expected}', found '{found}'"),
        TypeError::TryInsideDefer { .. } => "try inside defer".to_string(),
        TypeError::ForIterableNotSupported { found, .. } => {
            format!("type '{found}' cannot be iterated")
        }
        TypeError::ForVarRequiresMutableIterable { .. } => {
            "mutable iteration requires a mutable iterable place".to_string()
        }
        TypeError::ForMutableMapKey { .. } => {
            "map keys cannot be mutable; use `for k, var v in map` to mutate values".to_string()
        }
        TypeError::ForMutableMapEntry { .. } => {
            "mutable map entry iteration is not supported; use `for k, var v in map` to mutate values".to_string()
        }
        TypeError::InfiniteSize { name, .. } => {
            format!("type '{name}' has infinite size")
        }
        TypeError::NotEquatable { ty, .. } => format!("type '{ty}' is not equatable"),
        TypeError::UnsupportedPattern { pattern, .. } => format!("Unsupported pattern: {pattern}"),
        TypeError::TuplePatternArityMismatch {
            expected, found, ..
        } => {
            format!("tuple pattern arity mismatch: expected {expected}, found {found}")
        }
        TypeError::TuplePatternOnNonTuple { ty, .. } => {
            format!("cannot destructure non-tuple type '{ty}'")
        }
        TypeError::OrPatternBindingMismatch { .. } => {
            "or-pattern alternatives must bind the same variables".to_string()
        }
        TypeError::OrPatternBindingTypeMismatch {
            name,
            expected,
            found,
            ..
        } => format!(
            "or-pattern binding '{name}' type mismatch: expected '{expected}', found '{found}'"
        ),
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
            let kind = kind.diagnostic_name();
            format!("Unknown {kind} '{member}' for type '{ty}'")
        }
        TypeError::AmbiguousPromotedField {
            ty,
            member,
            candidates,
            ..
        } => {
            let candidates = render_ident_paths(candidates);
            format!("promoted field '{member}' for type '{ty}' is ambiguous: {candidates}")
        }
        TypeError::AmbiguousPromotedMethod {
            ty,
            member,
            candidates,
            ..
        } => {
            let candidates = render_ident_paths(candidates);
            format!("promoted method '{member}' for type '{ty}' is ambiguous: {candidates}")
        }
        TypeError::PromotedFieldNotStored {
            ty, field, paths, ..
        } => {
            let paths = render_ident_paths(paths);
            format!(
                "promoted field '{field}' is not a stored field of '{ty}'; use stored path {paths}"
            )
        }
        TypeError::DuplicateProjectionTarget {
            source,
            target,
            paths,
            ..
        } => {
            let paths = render_ident_paths(paths);
            format!(
                "projection from '{source}' to '{target}' is ambiguous; direct @as embed paths: {paths}"
            )
        }
        TypeError::ChainedProjection {
            source,
            target,
            via,
            ..
        } => {
            let via = render_ident_path(via);
            format!(
                "projection from '{source}' to '{target}' would require chaining through '{via}'; declare a direct `@as embed` field"
            )
        }
        TypeError::MissingProjection {
            source,
            target,
            paths,
            ..
        } => render_missing_projection(source, target, paths),
        TypeError::InstanceMethodOnType { ty, method, .. } => {
            format!("instance method '{method}' requires a value of type '{ty}'")
        }
        TypeError::StaticMethodOnValue { ty, method, .. } => {
            format!("static method '{method}' must be called on type '{ty}'")
        }
        TypeError::ReadonlyMethodMutation { .. } => {
            "readonly method cannot mutate self".to_string()
        }
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
        TypeError::RuntimeGlobalInConstPosition { global, .. } => format!(
            "runtime global '{}' cannot be used where a compile-time constant is required",
            global.name
        ),
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
    });
    let diagnostic = match error {
        TypeError::NonEscapingCallbackEscapes {
            help: Some(help), ..
        } => diagnostic.with_help(help.clone()),
        _ => diagnostic,
    };
    match span {
        Some(span) => diagnostic.with_primary(span),
        None => diagnostic,
    }
}

fn borrowed_capture_escape_message(name: Ident, origin: CaptureStorageOrigin) -> String {
    match origin {
        CaptureStorageOrigin::BorrowedParam => {
            format!("cannot capture borrowed parameter '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::VarSelf => {
            "cannot capture mutable receiver 'self' in an escaping lambda".to_string()
        }
        CaptureStorageOrigin::DynView => {
            format!("cannot capture borrowed dynamic view '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::PatternAlias => {
            format!("cannot capture mutable pattern alias '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::MutableDowncastAlias => {
            format!("cannot capture mutable downcast alias '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::ForVarAlias => {
            format!("cannot capture mutable loop alias '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::Owned
        | CaptureStorageOrigin::Const
        | CaptureStorageOrigin::ReadonlySelf => {
            format!("cannot capture '{name}' in an escaping lambda")
        }
    }
}

fn type_error_rich_message(error: &TypeError) -> Option<(&'static str, String)> {
    let message = match error {
        TypeError::TypeMismatch {
            expected, found, ..
        } => {
            let (expected, found) = render_type_mismatch_parts(expected, found);
            ("mismatched types", expected_found_label(&expected, &found))
        }
        TypeError::ConstMismatch {
            expected, found, ..
        } => (
            "mismatched types",
            format!(
                "expected const `{}`, found `{}`",
                render_const_diagnostic(expected),
                render_const_diagnostic(found)
            ),
        ),
        TypeError::UndefinedVariable { name, .. } => {
            ("unknown variable", format!("unknown variable `{name}`"))
        }
        TypeError::InvalidOperand {
            op, operand_type, ..
        } => (
            "invalid operand type",
            format!(
                "operator `{op}` cannot be applied to `{}`",
                render_surface_type(operand_type)
            ),
        ),
        TypeError::WrongArgCount {
            expected, found, ..
        } => (
            "Wrong number of arguments",
            format!("expected {expected}, found {found}"),
        ),
        TypeError::WrongArgRange {
            min, max, found, ..
        } => (
            "Wrong number of arguments",
            format!("expected between {min} and {max}, found {found}"),
        ),
        TypeError::IfConditionNotBool { found, .. } => {
            ("if condition must be bool", condition_type_label(found))
        }
        TypeError::TernaryConditionNotBool { found, .. } => (
            "ternary condition must be bool",
            condition_type_label(found),
        ),
        TypeError::WhileConditionNotBool { found, .. } => {
            ("while condition must be bool", condition_type_label(found))
        }
        TypeError::MissingReturn { expected, .. } => (
            "missing return value",
            format!("expected `{}`, found `void`", render_surface_type(expected)),
        ),
        TypeError::BreakOutsideLoop { .. } => (
            "break outside of loop",
            "`break` can only be used inside a loop".to_string(),
        ),
        TypeError::ContinueOutsideLoop { .. } => (
            "continue outside of loop",
            "`continue` can only be used inside a loop".to_string(),
        ),
        TypeError::ReturnInsideDefer { .. } => (
            "return inside defer",
            "`return` is not allowed inside `defer`".to_string(),
        ),
        TypeError::BreakInsideDefer { .. } => (
            "break inside defer",
            "`break` is not allowed inside `defer`".to_string(),
        ),
        TypeError::ContinueInsideDefer { .. } => (
            "continue inside defer",
            "`continue` is not allowed inside `defer`".to_string(),
        ),
        TypeError::ImmutableAssignment { name, .. } => (
            "Cannot assign to immutable value",
            format!("cannot assign to immutable value '{name}'"),
        ),
        TypeError::ConstAssignment { name, .. } => (
            "Cannot assign to constant",
            format!("cannot assign to constant '{name}'"),
        ),
        TypeError::NotCallable { ty, .. } => (
            "Not callable",
            format!("type '{}' is not callable", render_surface_type(ty)),
        ),
        TypeError::TupleIndexOnNonTuple { ty, index, .. } => (
            "Invalid tuple index",
            format!(
                "cannot index non-tuple type with .{index}; found '{}'",
                render_surface_type(ty)
            ),
        ),
        TypeError::TupleIndexOutOfBounds { index, len, .. } => (
            "Tuple index out of bounds",
            format!("tuple index {index} is out of bounds for length {len}"),
        ),
        TypeError::IndexNotInt { found, .. } => (
            "Invalid index type",
            format!(
                "index must be an integer; found '{}'",
                render_surface_type(found)
            ),
        ),
        TypeError::IndexOnNonIndexable { found, .. } => (
            "Not indexable",
            format!(
                "cannot index non-array type '{}'",
                render_surface_type(found)
            ),
        ),
        TypeError::RangeIndexNotInt { found, .. } => (
            "Invalid range index type",
            format!(
                "range index bounds must be integers; found '{}'",
                render_surface_type(found)
            ),
        ),
        TypeError::RangeIndexUnsupported { found, .. } => (
            "Range indexing unsupported",
            format!(
                "range indexing is not supported for type '{}'",
                render_surface_type(found)
            ),
        ),
        _ => return None,
    };
    Some(message)
}

fn expected_found_label(expected: &str, found: &str) -> String {
    format!("expected `{expected}`, found `{found}`")
}

fn condition_type_label(found: &Type) -> String {
    format!(
        "condition must be `bool`, found `{}`",
        render_surface_type(found)
    )
}

fn type_error_span(error: &TypeError) -> Option<SourceSpan> {
    match error {
        TypeError::Decl(error) => decl_error_span(error),
        TypeError::ExternCatalog(error) => extern_catalog_error_span(error),
        TypeError::UndefinedVariable { span, .. }
        | TypeError::TypeMismatch { span, .. }
        | TypeError::ConstMismatch { span, .. }
        | TypeError::RecursiveInference { span, .. }
        | TypeError::CannotInferType { span, .. }
        | TypeError::CannotInferEnum { span, .. }
        | TypeError::NamedFunctionCapture { span, .. }
        | TypeError::AllNilArrayLiteral { span, .. }
        | TypeError::ArrayFillLengthNotConst { span, .. }
        | TypeError::InferReturnNonGeneric { span, .. }
        | TypeError::InferReturnExtern { span, .. }
        | TypeError::InferReturnValue { span, .. }
        | TypeError::InferReturnMismatch { span, .. }
        | TypeError::InferReturnRecursive { span, .. }
        | TypeError::UnsupportedPlaceReturn { span, .. }
        | TypeError::UnknownType { span, .. }
        | TypeError::TypeUsedAsValue { span, .. }
        | TypeError::CannotInferConst { span, .. }
        | TypeError::NotCallable { span, .. }
        | TypeError::WrongArgCount { span, .. }
        | TypeError::WrongArgRange { span, .. }
        | TypeError::LambdaParamCountMismatch { span, .. }
        | TypeError::RequiredParamAfterDefault { span, .. }
        | TypeError::EnumVariantArgCount { span, .. }
        | TypeError::DuplicateName { span, .. }
        | TypeError::ImmutableAssignment { span, .. }
        | TypeError::ConstAssignment { span, .. }
        | TypeError::VarArgNonLvalue { span, .. }
        | TypeError::VarArgImmutableBinding { span, .. }
        | TypeError::MutatingMethodImmutableReceiver { span, .. }
        | TypeError::MutableAlias { span, .. }
        | TypeError::InvalidFormatSpec { span, .. }
        | TypeError::NonEscapingCallbackEscapes { span, .. }
        | TypeError::BorrowedCaptureEscapes { span, .. }
        | TypeError::RequiresMutablePlace { span, .. }
        | TypeError::VarPatternRequiresMutablePlace { span, .. }
        | TypeError::ForVarRequiresMutableIterable { span, .. }
        | TypeError::ForMutableMapKey { span, .. }
        | TypeError::ForMutableMapEntry { span, .. }
        | TypeError::InvalidOperand { span, .. }
        | TypeError::MissingReturn { span, .. }
        | TypeError::IfWithoutElseValue { span, .. }
        | TypeError::IfConditionNotBool { span, .. }
        | TypeError::TernaryConditionNotBool { span, .. }
        | TypeError::WhileConditionNotBool { span, .. }
        | TypeError::BreakOutsideLoop { span, .. }
        | TypeError::ContinueOutsideLoop { span, .. }
        | TypeError::ReturnInsideDefer { span, .. }
        | TypeError::BreakInsideDefer { span, .. }
        | TypeError::ContinueInsideDefer { span, .. }
        | TypeError::TryOnInvalidCarrier { span, .. }
        | TypeError::TryOutsideCarrierFunction { span, .. }
        | TypeError::TryResultErrorMismatch { span, .. }
        | TypeError::TryInsideDefer { span, .. }
        | TypeError::ForIterableNotSupported { span, .. }
        | TypeError::ForIterationModifier { span, .. }
        | TypeError::InfiniteSize { span, .. }
        | TypeError::NotEquatable { span, .. }
        | TypeError::UnsupportedPattern { span, .. }
        | TypeError::TuplePatternArityMismatch { span, .. }
        | TypeError::TuplePatternOnNonTuple { span, .. }
        | TypeError::OrPatternBindingMismatch { span, .. }
        | TypeError::OrPatternBindingTypeMismatch { span, .. }
        | TypeError::EmptyMatch { span, .. }
        | TypeError::NonExhaustiveMatch { span, .. }
        | TypeError::UnsupportedMatchScrutinee { span, .. }
        | TypeError::InvalidLiteralPattern { span, .. }
        | TypeError::OptionalPatternOnNonOptional { span, .. }
        | TypeError::OptionalChainingOnNonOptional { span, .. }
        | TypeError::NestedOptionalPattern { span, .. }
        | TypeError::MatchArmTypeMismatch { span, .. }
        | TypeError::RequiresUnwrappingPattern { span, .. }
        | TypeError::IrrefutableLetElse { span, .. }
        | TypeError::LetElseMustDiverge { span, .. }
        | TypeError::MemberAccessOnNonAggregate { span, .. }
        | TypeError::UnknownMember { span, .. }
        | TypeError::AmbiguousPromotedField { span, .. }
        | TypeError::AmbiguousPromotedMethod { span, .. }
        | TypeError::PromotedFieldNotStored { span, .. }
        | TypeError::DuplicateProjectionTarget { span, .. }
        | TypeError::ChainedProjection { span, .. }
        | TypeError::MissingProjection { span, .. }
        | TypeError::InstanceMethodOnType { span, .. }
        | TypeError::StaticMethodOnValue { span, .. }
        | TypeError::ReadonlyMethodMutation { span, .. }
        | TypeError::UnknownIntrinsic { span, .. }
        | TypeError::IntrinsicArgCount { span, .. }
        | TypeError::IntrinsicExpectedIdent { span, .. }
        | TypeError::IntrinsicExpectedString { span, .. }
        | TypeError::UnknownIntrinsicValue { span, .. }
        | TypeError::CompileError { span, .. }
        | TypeError::MethodGenericShadow { span, .. }
        | TypeError::TupleIndexOnNonTuple { span, .. }
        | TypeError::TupleIndexOutOfBounds { span, .. }
        | TypeError::IndexNotInt { span, .. }
        | TypeError::IndexOnNonIndexable { span, .. }
        | TypeError::RangeIndexNotInt { span, .. }
        | TypeError::RangeIndexUnsupported { span, .. }
        | TypeError::NonKeyableMapKey { span, .. }
        | TypeError::DuplicateMapKey { span, .. }
        | TypeError::UndefinedModuleMember { span, .. }
        | TypeError::PrivateModuleMember { span, .. }
        | TypeError::AmbiguousExtendMethod { span, .. }
        | TypeError::DuplicateField { span, .. }
        | TypeError::MissingField { span, .. }
        | TypeError::UnknownVariantField { span, .. }
        | TypeError::MissingVariantField { span, .. }
        | TypeError::InvalidStructLiteral { span, .. }
        | TypeError::UnknownStructLiteral { span, .. }
        | TypeError::UnknownEnumVariant { span, .. }
        | TypeError::EnumPatternTypeMismatch { span, .. }
        | TypeError::EnumVariantShapeMismatch { span, .. }
        | TypeError::UnboundGenericParam { span, .. }
        | TypeError::UnknownConst { span, .. }
        | TypeError::RuntimeGlobalInConstPosition { span, .. }
        | TypeError::ConstCycle { span, .. }
        | TypeError::NonConstExpression { span, .. }
        | TypeError::GenericFieldDefault { span, .. }
        | TypeError::ConstTypeMismatch { span, .. }
        | TypeError::InvalidConstCast { span, .. }
        | TypeError::InvalidCast { span, .. }
        | TypeError::ConstDivisionByZero { span, .. }
        | TypeError::ConstOverflow { span, .. }
        | TypeError::ExpectedIntConst { span, .. }
        | TypeError::NegativeArrayLength { span, .. }
        | TypeError::GenericArgKindMismatch { span, .. }
        | TypeError::ExternAnyEscape { span, .. }
        | TypeError::AnyOutsideExternBoundary { span, .. }
        | TypeError::ContractUnsatisfied { span, .. }
        | TypeError::DynamicMethodMissing { span, .. }
        | TypeError::BorrowedDynReassign { span, .. }
        | TypeError::DynContainerConversion { span, .. }
        | TypeError::DuplicateGenericParam { span, .. } => *span,
        TypeError::GenericArity(_) => None,
    }
}

fn decl_error_span(error: &DeclError) -> Option<SourceSpan> {
    match error {
        DeclError::DuplicateValue { span, .. }
        | DeclError::DuplicateType { span, .. }
        | DeclError::MissingImportMember { span, .. }
        | DeclError::PrivateImportMember { span, .. }
        | DeclError::ImportConflict { span, .. }
        | DeclError::DuplicateModuleBinding { span, .. }
        | DeclError::DuplicateGenericParam { span, .. }
        | DeclError::DuplicateAggregateMethod { span, .. }
        | DeclError::DuplicateContractRequirement { span, .. }
        | DeclError::DuplicateExtendMethod { span, .. }
        | DeclError::DuplicateCastFrom { span, .. }
        | DeclError::PointlessCastFrom { span, .. }
        | DeclError::CastFromReturnMismatch { span, .. }
        | DeclError::UnsupportedExtendTarget { span, .. }
        | DeclError::UnusedExtendTypeParam { span, .. }
        | DeclError::UnusedExtendConstParam { span, .. }
        | DeclError::UnusedAliasTypeParam { span, .. }
        | DeclError::UnusedAliasConstParam { span, .. }
        | DeclError::PublicAliasPrivateType { span, .. }
        | DeclError::PublicContractPrivateType { span, .. }
        | DeclError::PublicValuePrivateType { span, .. }
        | DeclError::ExtendMethodConflict { span, .. }
        | DeclError::ReexportConflict { span, .. }
        | DeclError::UnknownType { span, .. }
        | DeclError::UnknownAnnotation { span, .. }
        | DeclError::InvalidAnnotationTarget { span, .. }
        | DeclError::DuplicateAnnotation { span, .. }
        | DeclError::InvalidAnnotationArgs { span, .. }
        | DeclError::AsProjectionWithoutEmbed { span }
        | DeclError::AsProjectionWithArgs { span }
        | DeclError::InternalOnToString { span }
        | DeclError::InvalidToStringMethod { span, .. }
        | DeclError::EmptyEmbedSelector { span }
        | DeclError::DuplicateEmbedSelector { span, .. }
        | DeclError::EmbedSurfaceCycle { span, .. }
        | DeclError::UnknownEmbedFieldSelector { span, .. }
        | DeclError::EmbedFieldSelectorNamesMethod { span, .. }
        | DeclError::AmbiguousEmbedFieldSelector { span, .. }
        | DeclError::EmbedFieldConflictsWithDirect { span, .. }
        | DeclError::DuplicateExplicitEmbedField { span, .. }
        | DeclError::UnknownEmbedMethodSelector { span, .. }
        | DeclError::EmbedMethodSelectorNamesField { span, .. }
        | DeclError::EmbedMethodSelectorNamesStatic { span, .. }
        | DeclError::EmbedMethodSelectorNamesToString { span }
        | DeclError::AmbiguousEmbedMethodSelector { span, .. }
        | DeclError::EmbedMethodConflictsWithDirect { span, .. }
        | DeclError::DuplicateExplicitEmbedMethod { span, .. }
        | DeclError::DuplicateProjectionTarget { span, .. } => *span,
    }
}

fn extern_catalog_error_span(error: &ExternCatalogError) -> Option<SourceSpan> {
    match error {
        ExternCatalogError::UnknownType { site, .. }
        | ExternCatalogError::GenericArity { site, .. }
        | ExternCatalogError::GenericArgKindMismatch { site, .. }
        | ExternCatalogError::InvalidType { site, .. }
        | ExternCatalogError::UnknownInitField { site, .. }
        | ExternCatalogError::ComputedInitField { site, .. }
        | ExternCatalogError::UnsupportedInitParams { site, .. }
        | ExternCatalogError::InvalidOperatorReturn { site, .. } => site.span,
    }
}

fn render_variant_shape(shape: VariantShape) -> &'static str {
    match shape {
        VariantShape::Unit => "unit",
        VariantShape::Tuple => "tuple",
        VariantShape::Struct => "struct",
    }
}

fn render_type_mismatch(expected: &Type, found: &Type) -> String {
    let (expected, found) = render_type_mismatch_parts(expected, found);
    format!(
        "Mismatched types: {}",
        expected_found_label(&expected, &found)
    )
}

fn render_dyn_container_conversion(kind: DynContainerConversionKind) -> String {
    match kind {
        DynContainerConversionKind::Collection => {
            "collection conversion is not implicit; construct a dynamic collection with an expected element type"
        }
        DynContainerConversionKind::FixedArray => {
            "fixed-array conversion is not implicit; construct a dynamic fixed array with an expected element type"
        }
        DynContainerConversionKind::Slice => {
            "slices are invariant and cannot be reinterpreted as dynamic slices"
        }
        DynContainerConversionKind::DynamicWeakening => {
            "dynamic weakening applies to values, not containers"
        }
        DynContainerConversionKind::MapValue => {
            "maps are invariant and no hidden dynamic value conversion is created"
        }
    }
    .to_string()
}

fn render_type_mismatch_parts(expected: &Type, found: &Type) -> (String, String) {
    let ambiguous = expected != found && expected.to_string() == found.to_string();
    (
        render_mismatch_type(expected, ambiguous),
        render_mismatch_type(found, ambiguous),
    )
}

fn render_mismatch_type(ty: &Type, detailed: bool) -> String {
    if !detailed {
        return render_surface_type(ty);
    }
    render_detailed_type(ty)
}

fn render_surface_type(ty: &Type) -> String {
    if let Some(inner) = ty.option_inner() {
        return format!("{}?", render_surface_type(inner));
    }
    match ty {
        Type::List { elem } => format!("[{}]", render_surface_type(elem)),
        Type::Array { elem, len } => format!("[{}; {len}]", render_surface_type(elem)),
        Type::Map { key, value } => {
            format!(
                "[{}: {}]",
                render_surface_type(key),
                render_surface_type(value)
            )
        }
        Type::Slice { elem } => format!("slice[{}]", render_surface_type(elem)),
        Type::Tuple(elems) => render_wrapped_types("(", ")", elems, render_surface_type),
        Type::Func { .. }
        | Type::Dyn(_)
        | Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. }
        | Type::Nominal(_) => ty.to_string(),
    }
}

fn render_detailed_type(ty: &Type) -> String {
    match ty {
        Type::Func { params, ret } => render_detailed_func(params, ret),
        Type::Tuple(elems) => render_wrapped_types("(", ")", elems, render_detailed_type),
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

fn render_param_prefix(mutable: bool, escape: EscapeMode, cast_accept: bool) -> String {
    let mut rendered = String::new();
    if mutable {
        rendered.push_str("var ");
    }
    if escape.is_escaping() {
        rendered.push_str("escaping ");
    }
    if cast_accept {
        rendered.push_str("as ");
    }
    rendered
}

fn render_detailed_func(params: &[FuncParam], ret: &ReturnSpec) -> String {
    let params = params
        .iter()
        .map(|param| {
            let mut rendered = render_param_prefix(param.mutable, param.escape, param.cast_accept);
            rendered.push_str(&render_detailed_type(&param.ty));
            rendered
        })
        .collect::<Vec<_>>()
        .join(", ");
    let mut rendered = format!("fn({params})");
    if !ret.is_implicit_void() {
        rendered.push_str(" -> ");
        if ret.is_place() {
            rendered.push_str("var ");
        }
        rendered.push_str(&render_detailed_type(&ret.ty));
    }
    rendered
}

fn render_wrapped_types(
    open: &str,
    close: &str,
    elems: &[Type],
    render: fn(&Type) -> String,
) -> String {
    let elems = elems.iter().map(render).collect::<Vec<_>>().join(", ");
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

fn render_detailed_generic_arg(arg: &ast::GenericArg) -> String {
    match arg {
        ast::GenericArg::Type(ty) => render_detailed_type(ty),
        ast::GenericArg::Const(arg) => arg.to_string(),
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

fn render_qualified_name(qualifier: Option<Ident>, name: Ident) -> String {
    match qualifier {
        Some(qualifier) => format!("{qualifier}.{name}"),
        None => name.to_string(),
    }
}

fn render_ident_path(path: &[Ident]) -> String {
    path.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(".")
}

fn render_ident_paths(paths: &[Vec<Ident>]) -> String {
    paths
        .iter()
        .map(|path| render_ident_path(path))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_missing_projection(source: &Type, target: &Type, paths: &[Vec<Ident>]) -> String {
    if paths.is_empty() {
        return format!(
            "no direct `@as embed` projection from '{source}' to '{target}'; pass a mutable '{target}' place or declare a direct `@as embed` field"
        );
    }

    format!(
        "no direct `@as embed` projection from '{source}' to '{target}'; pass stored path {} or mark it with `@as embed`",
        render_ident_paths(paths)
    )
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
        DeclError::DuplicateContractRequirement { contract, name, .. } => format!(
            "duplicate contract requirement '{name}' in contract '{}'",
            contract.name
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
        DeclError::UnusedAliasTypeParam { name, .. } => {
            format!("unused type parameter '{name}' in type alias")
        }
        DeclError::UnusedAliasConstParam { name, .. } => {
            format!("unused const parameter '{name}' in type alias")
        }
        DeclError::PublicAliasPrivateType { name, ty, .. } => {
            format!("public type alias '{name}' exposes private type '{ty}'")
        }
        DeclError::PublicContractPrivateType { name, ty, .. } => {
            format!("public contract '{name}' exposes private type '{ty}'")
        }
        DeclError::PublicValuePrivateType { kind, name, ty, .. } => {
            let kind = kind.label();
            format!("public {kind} '{name}' exposes private type '{ty}'")
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
        DeclError::AsProjectionWithoutEmbed { .. } => {
            "`@as` is not valid on field declarations: can only be applied to embedded fields"
                .to_string()
        }
        DeclError::AsProjectionWithArgs { .. } => {
            "invalid arguments for `@as`: this annotation does not accept arguments".to_string()
        }
        DeclError::InternalOnToString { .. } => {
            "`@internal` cannot be applied to `to_string` methods".to_string()
        }
        DeclError::InvalidToStringMethod { message, .. } => message.to_string(),
        DeclError::EmptyEmbedSelector { .. } => "embed selector cannot be empty".to_string(),
        DeclError::DuplicateEmbedSelector { name, kind, .. } => {
            let namespace = match kind {
                ast::EmbedSelectorKind::Field => "field",
                ast::EmbedSelectorKind::Method => "method",
            };
            format!("duplicate embed {namespace} selector '{name}'")
        }
        DeclError::EmbedSurfaceCycle { owner, .. } => {
            format!("embedded field surface for '{}' is recursive", owner.name)
        }
        DeclError::UnknownEmbedFieldSelector { name, .. } => {
            format!("unknown embed field selector '{name}'")
        }
        DeclError::EmbedFieldSelectorNamesMethod { name, .. } => {
            format!("embed field selector '{name}' names a method; use `fn {name}`")
        }
        DeclError::AmbiguousEmbedFieldSelector { name, .. } => {
            format!("embed field selector '{name}' is ambiguous")
        }
        DeclError::EmbedFieldConflictsWithDirect { owner, name, .. } => {
            format!(
                "explicit promoted field '{name}' conflicts with direct field on '{}'",
                owner.name
            )
        }
        DeclError::DuplicateExplicitEmbedField { owner, name, .. } => {
            format!(
                "duplicate explicit promoted field '{name}' on '{}'",
                owner.name
            )
        }
        DeclError::UnknownEmbedMethodSelector { name, .. } => {
            format!("unknown embed method selector '{name}'")
        }
        DeclError::EmbedMethodSelectorNamesField { name, .. } => {
            format!("embed method selector 'fn {name}' names a field; remove `fn`")
        }
        DeclError::EmbedMethodSelectorNamesStatic { name, .. } => {
            format!("embed method selector 'fn {name}' names a static method")
        }
        DeclError::EmbedMethodSelectorNamesToString { .. } => {
            "`to_string` is not promoted; use the explicit path or define it on the outer type"
                .to_string()
        }
        DeclError::AmbiguousEmbedMethodSelector { name, .. } => {
            format!("embed method selector 'fn {name}' is ambiguous")
        }
        DeclError::EmbedMethodConflictsWithDirect { owner, name, .. } => {
            format!(
                "explicit promoted method '{name}' conflicts with direct method on '{}'",
                owner.name
            )
        }
        DeclError::DuplicateExplicitEmbedMethod { owner, name, .. } => {
            format!(
                "duplicate explicit promoted method '{name}' on '{}'",
                owner.name
            )
        }
        DeclError::DuplicateProjectionTarget { owner, target, .. } => {
            format!(
                "duplicate `@as embed` projection target '{target}' on '{}'",
                owner.name
            )
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
        BindingOrigin::Import { source, .. }
        | BindingOrigin::Reexport { source, .. }
        | BindingOrigin::ImplicitImport { source } => {
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
        lexer::{Keyword, Token, TokenStream},
        parser,
        source::{SourceKind, SourceTable},
        span::SourceSpan,
        typecheck::MemberAccessKind,
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

    fn source_decl_with_span(module: RawExternScope, span: SourceSpan) -> RawExternDecl {
        RawExternDecl {
            provenance: ExternProvenance::Source { module },
            site: RawExternSite { span: Some(span) },
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

    fn test_source(text: &str) -> SourceId {
        let mut sources = SourceTable::default();
        sources.add(SourceKind::Virtual, "test", None, text)
    }

    fn token_span() -> SourceSpan {
        SourceSpan::new(test_source(";"), 0, 1)
    }

    fn lex_source() -> SourceId {
        test_source("!")
    }

    fn resolve_span() -> SourceSpan {
        SourceSpan::new(test_source("x"), 0, 1)
    }

    fn type_span() -> SourceSpan {
        resolve_span()
    }

    fn token_stream(tokens: Vec<(Token, SourceSpan)>) -> TokenStream {
        let source = tokens
            .first()
            .map_or_else(|| test_source(""), |(_, span)| span.source());
        let len = tokens.iter().map(|(_, span)| span.end()).max().unwrap_or(0);
        TokenStream {
            source,
            eoi: SourceSpan::empty(source, len),
            tokens,
        }
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
    fn renders_compile_warnings() {
        let span = resolve_span();
        let diagnostic = diagnose_compile_warning(&CompileWarning {
            message: "careful".to_string(),
            span,
        });
        assert_eq!(diagnostic.labels()[0].span, span);
        assert_eq!(diagnostic.message(), "careful");
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("compile warning emitted here")
        );
    }

    #[test]
    fn renders_resolve_errors() {
        let cases = [
            (
                diagnose_resolve_error(&ResolveError::ModuleNotFound {
                    module: module_id(&["foo", "bar"]),
                    span: resolve_span(),
                }),
                "Cannot find module file for module 'foo.bar'",
            ),
            (
                diagnose_resolve_error(&ResolveError::LoadFailed {
                    module: module_id(&["foo", "bar"]),
                    span: resolve_span(),
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

        let span = resolve_span();
        let diagnostic = diagnose_resolve_error(&ResolveError::ModuleNotFound {
            module: module_id(&["foo"]),
            span,
        });
        assert_eq!(diagnostic.labels()[0].span, span);
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("module file not found")
        );

        let diagnostic = diagnose_resolve_error(&ResolveError::SourceImportNotFound {
            importer: SourceFileId::new("/tmp/main.anv").unwrap(),
            path: module_path(&["missing"]),
            candidate: Some(std::path::PathBuf::from("missing.anv")),
            span,
        });
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("import resolved to no source file")
        );
        assert_eq!(diagnostic.notes(), &["looked for 'missing.anv'"]);
    }

    #[test]
    fn renders_decl_errors() {
        let cases = [
            (
                diagnose_type_error(&TypeError::Decl(DeclError::DuplicateType {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    span: Some(resolve_span()),
                })),
                "type 'Point' is already defined",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::MissingImportMember {
                    module: ModuleScope::Root,
                    imported: module_scope(&["tools"]),
                    name: ident("Point"),
                    span: Some(resolve_span()),
                })),
                "Unknown member 'Point' in module 'tools'",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::PrivateImportMember {
                    module: ModuleScope::Root,
                    imported: module_scope(&["tools"]),
                    name: ident("secret"),
                    span: Some(resolve_span()),
                })),
                "member 'secret' in module 'tools' is private",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::MissingImportMember {
                    module: ModuleScope::Root,
                    imported: source_module_scope("/tmp/lib.anv"),
                    name: ident("missing"),
                    span: Some(resolve_span()),
                })),
                "Unknown member 'missing' in module 'lib'",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::DuplicateModuleBinding {
                    module: ModuleScope::Root,
                    name: ident("tools"),
                    first: BindingOrigin::Local,
                    second: BindingOrigin::ImplicitImport {
                        source: module_scope(&["tools"]),
                    },
                    span: Some(resolve_span()),
                })),
                "module binding 'tools' is already in use",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::DuplicateGenericParam {
                    module: ModuleScope::Root,
                    name: ident("T"),
                    span: Some(resolve_span()),
                })),
                "duplicate generic parameter 'T'",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::ImportConflict {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    namespace: BindingNamespace::Type,
                    first: BindingOrigin::Local,
                    second: BindingOrigin::ImplicitImport {
                        source: module_scope(&["shapes"]),
                    },
                    span: Some(resolve_span()),
                })),
                "imported type 'Point' conflicts with a locally defined type",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::ImportConflict {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    namespace: BindingNamespace::Type,
                    first: BindingOrigin::ImplicitImport {
                        source: module_scope(&["alpha"]),
                    },
                    second: BindingOrigin::ImplicitImport {
                        source: module_scope(&["beta"]),
                    },
                    span: Some(resolve_span()),
                })),
                "imported type 'Point' conflicts with a previously imported name",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::ReexportConflict {
                    module: ModuleScope::Root,
                    name: ident("Point"),
                    namespace: BindingNamespace::Type,
                    first: BindingOrigin::ImplicitImport {
                        source: module_scope(&["alpha"]),
                    },
                    second: BindingOrigin::ImplicitImport {
                        source: module_scope(&["beta"]),
                    },
                    span: Some(resolve_span()),
                })),
                "type 'Point' is re-exported by both 'alpha' and 'beta'",
            ),
            (
                diagnose_type_error(&TypeError::Decl(DeclError::InvalidToStringMethod {
                    message: "to_string method must return 'string'",
                    span: Some(resolve_span()),
                })),
                "to_string method must return 'string'",
            ),
        ];

        for (diagnostic, expected) in cases {
            assert_msg(diagnostic, expected);
        }
    }

    fn package_nominal(package: &str, name: &str) -> Type {
        Type::nominal_with_origin(
            ast::NominalKind::Struct,
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
                    span: Some(type_span()),
                }),
                "mismatched types",
            ),
            (
                diagnose_type_error(&TypeError::TypeMismatch {
                    expected: Type::Tuple(vec![Type::option_of(Type::Int)]),
                    found: Type::Tuple(vec![Type::option_of(Type::String)]),
                    span: Some(type_span()),
                }),
                "mismatched types",
            ),
            (
                diagnose_type_error(&TypeError::TypeMismatch {
                    expected: package_nominal("left", "Vec2"),
                    found: package_nominal("right", "Vec2"),
                    span: Some(type_span()),
                }),
                "mismatched types",
            ),
            (
                diagnose_type_error(&TypeError::UndefinedVariable {
                    name: ident("x"),
                    span: Some(type_span()),
                }),
                "unknown variable",
            ),
            (
                diagnose_type_error(&TypeError::InvalidOperand {
                    op: "-".to_string(),
                    operand_type: Type::Bool,
                    span: Some(type_span()),
                }),
                "invalid operand type",
            ),
            (
                diagnose_type_error(&TypeError::ConstAssignment {
                    name: ident("LIMIT"),
                    span: Some(type_span()),
                }),
                "Cannot assign to constant",
            ),
            (
                diagnose_type_error(&TypeError::CannotInferEnum {
                    span: Some(type_span()),
                }),
                "cannot infer enum type",
            ),
            (
                diagnose_type_error(&TypeError::NamedFunctionCapture {
                    name: ident("x"),
                    span: Some(type_span()),
                }),
                "named functions cannot capture local value 'x'",
            ),
            (
                diagnose_type_error(&TypeError::VarArgNonLvalue {
                    span: Some(type_span()),
                }),
                "non-lvalue cannot be passed to var parameter",
            ),
            (
                diagnose_type_error(&TypeError::VarArgImmutableBinding {
                    name: ident("x"),
                    span: Some(type_span()),
                }),
                "immutable binding 'x' cannot be passed to var parameter",
            ),
            (
                diagnose_type_error(&TypeError::MutatingMethodImmutableReceiver {
                    name: ident("self"),
                    span: Some(type_span()),
                }),
                "mutating method requires mutable receiver 'self'",
            ),
            (
                diagnose_type_error(&TypeError::MutableAlias {
                    span: Some(type_span()),
                }),
                "var arguments must not alias the same variable",
            ),
            (
                diagnose_type_error(&TypeError::InvalidFormatSpec {
                    reason: "hex format requires int",
                    span: Some(type_span()),
                }),
                "invalid format specifier: hex format requires int",
            ),
            (
                diagnose_type_error(&TypeError::InfiniteSize {
                    name: ident("Node"),
                    span: Some(type_span()),
                }),
                "type 'Node' has infinite size",
            ),
            (
                diagnose_type_error(&TypeError::NotEquatable {
                    ty: Type::Slice {
                        elem: Box::new(Type::Int),
                    },
                    span: Some(type_span()),
                }),
                "type 'slice[int]' is not equatable",
            ),
            (
                diagnose_type_error(&TypeError::OrPatternBindingMismatch {
                    span: Some(type_span()),
                }),
                "or-pattern alternatives must bind the same variables",
            ),
            (
                diagnose_type_error(&TypeError::OrPatternBindingTypeMismatch {
                    name: ident("x"),
                    expected: Type::Int,
                    found: Type::String,
                    span: Some(type_span()),
                }),
                "or-pattern binding 'x' type mismatch: expected 'int', found 'string'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownMember {
                    ty: Type::UnresolvedName(ident("Point")),
                    member: ident("z"),
                    kind: MemberAccessKind::Field,
                    span: Some(type_span()),
                }),
                "Unknown field 'z' for type 'Point'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownMember {
                    ty: Type::UnresolvedName(ident("Counter")),
                    member: ident("reset"),
                    kind: MemberAccessKind::Method,
                    span: Some(type_span()),
                }),
                "Unknown method 'reset' for type 'Counter'",
            ),
            (
                diagnose_type_error(&TypeError::MemberAccessOnNonAggregate {
                    ty: Type::Int,
                    member: ident("y"),
                    kind: MemberAccessKind::Field,
                    span: Some(type_span()),
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
                    span: Some(type_span()),
                }),
                "Could not infer type parameter 'T'",
            ),
            (
                diagnose_type_error(&TypeError::DuplicateGenericParam {
                    name: ident("T"),
                    span: Some(type_span()),
                }),
                "duplicate generic parameter 'T'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownStructLiteral {
                    qualifier: None,
                    name: ident("Point"),
                    span: Some(type_span()),
                }),
                "Unknown struct 'Point'",
            ),
            (
                diagnose_type_error(&TypeError::UnknownStructLiteral {
                    qualifier: Some(ident("shapes")),
                    name: ident("Point"),
                    span: Some(type_span()),
                }),
                "Unknown struct 'shapes.Point'",
            ),
            (
                diagnose_type_error(&TypeError::AnyOutsideExternBoundary {
                    span: Some(type_span()),
                }),
                "any is only allowed in extern boundary signatures",
            ),
            (
                diagnose_type_error(&TypeError::ExternAnyEscape {
                    span: Some(type_span()),
                }),
                "extern 'any' value cannot escape an extern boundary",
            ),
        ];

        for (diagnostic, expected) in cases {
            assert_msg(diagnostic, expected);
        }

        let span = resolve_span();
        let diagnostic = diagnose_type_error(&TypeError::UndefinedVariable {
            name: ident("x"),
            span: Some(span),
        });
        assert_eq!(diagnostic.labels()[0].span, span);
        assert_eq!(
            diagnostic.labels()[0].message.as_deref(),
            Some("unknown variable `x`")
        );

        let diagnostic = diagnose_type_error(&TypeError::TypeMismatch {
            expected: Type::Int,
            found: Type::Bool,
            span: Some(type_span()),
        });
        assert_eq!(diagnostic.message(), "mismatched types");
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("expected `int`, found `bool`")
        );

        let diagnostic = diagnose_type_error(&TypeError::InvalidOperand {
            op: "-".to_string(),
            operand_type: Type::Bool,
            span: Some(type_span()),
        });
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("operator `-` cannot be applied to `bool`")
        );

        let diagnostic = diagnose_type_error(&TypeError::WrongArgRange {
            min: 1,
            max: 3,
            found: 4,
            span: Some(type_span()),
        });
        assert_eq!(diagnostic.message(), "Wrong number of arguments");
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("expected between 1 and 3, found 4")
        );

        let diagnostic = diagnose_type_error(&TypeError::IfConditionNotBool {
            found: Type::Int,
            span: Some(type_span()),
        });
        assert_eq!(diagnostic.message(), "if condition must be bool");
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("condition must be `bool`, found `int`")
        );
    }

    #[test]
    fn omits_labels_without_source_span() {
        let diagnostic = diagnose_type_error(&TypeError::CannotInferType { span: None });
        assert!(diagnostic.labels().is_empty());

        let diagnostic = diagnose_type_error(&TypeError::TypeMismatch {
            expected: Type::Int,
            found: Type::Bool,
            span: None,
        });
        assert_msg(
            &diagnostic,
            "mismatched types: expected `int`, found `bool`",
        );
        assert!(diagnostic.labels().is_empty());

        let diagnostic = diagnose_type_error(&TypeError::Decl(DeclError::DuplicateType {
            module: ModuleScope::Root,
            name: ident("Point"),
            span: None,
        }));
        assert!(diagnostic.labels().is_empty());

        let diagnostic =
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
            }));
        assert!(diagnostic.labels().is_empty());
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
    fn extern_input_errors_label_source_spans() {
        let span = resolve_span();
        let decl = source_decl_with_span(raw_root_scope(), span);
        let diagnostic = diagnose_extern_input_error(&ExternInputError::InvalidRawDescriptor {
            decl,
            scope: raw_root_scope(),
            error: ExternDescriptorError::VoidType {
                context: TypeContext::Param,
            },
        });
        assert_eq!(diagnostic.labels()[0].span, span);
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("invalid extern declaration here")
        );

        let diagnostic = diagnose_extern_input_error(&ExternInputError::UnsupportedSource {
            kind: UnsupportedSourceKind::Operator("+".to_string()),
            span,
        });
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("unsupported source extern operator here")
        );
    }

    #[test]
    fn duplicate_source_extern_identities_use_related_labels() {
        let first = resolve_span();
        let duplicate = SourceSpan::new(first.source(), 2, 3);
        let diagnostic = diagnose_extern_input_error(&ExternInputError::DuplicateRawIdentity {
            key: RawExternIdentityKey::Function(RawExternFunctionKey {
                module: raw_root_scope(),
                name: "tick".to_string(),
            }),
            first: source_decl_with_span(raw_root_scope(), first),
            duplicate: source_decl_with_span(raw_root_scope(), duplicate),
        });

        assert_eq!(diagnostic.labels()[0].span, duplicate);
        assert_eq!(
            diagnostic.labels()[0].message.as_deref(),
            Some("duplicate extern declaration")
        );
        assert_eq!(diagnostic.labels()[1].span, first);
        assert_eq!(
            diagnostic.labels()[1].message.as_deref(),
            Some("first declared here")
        );
    }

    #[test]
    fn provider_extern_input_errors_stay_message_only() {
        let diagnostic =
            diagnose_extern_input_error(&ExternInputError::InvalidProviderDescriptor {
                package: PackageId::synthetic_root(),
                provider: ProviderId {
                    name: "host".to_string(),
                },
                error: ExternDescriptorError::EmptyModulePath,
            });

        assert!(diagnostic.labels().is_empty());
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
                provider: ProviderId {
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
            diagnose_lex_error(
                lex_source(),
                1,
                &Rich::custom((0..1).into(), "invalid escape sequence"),
            ),
            "invalid escape sequence",
        );

        let error =
            <Rich<'_, char> as LabelError<'_, &'_ str, RichPattern<'_, char>>>::expected_found(
                [],
                Some('!'.into()),
                (0..1).into(),
            );
        let source = lex_source();
        let diagnostic = diagnose_lex_error(source, 1, &error);
        assert_msg(&diagnostic, "Unexpected character '!'");
        assert_eq!(diagnostic.labels()[0].span, SourceSpan::new(source, 0, 1));
        assert_eq!(
            diagnostic.labels()[0].message.as_deref(),
            Some("unexpected character")
        );

        let error =
            <Rich<'_, char> as LabelError<'_, &'_ str, RichPattern<'_, char>>>::expected_found(
                [],
                None,
                (1..1).into(),
            );
        let diagnostic = diagnose_lex_error(source, 1, &error);
        assert_msg(&diagnostic, "Unexpected end of input");
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("end of file")
        );
    }

    #[test]
    fn parser_type_context_distinguishes_colon_sites() {
        assert_eq!(
            parse_expected_context(
                &[RichPattern::Label("type".into())],
                Some(&Token::Close(Delimiter::Parent)),
            ),
            Some(("expected type after :", "expected a type here"))
        );
        assert_eq!(
            parse_expected_context(
                &[RichPattern::Label("type".into())],
                Some(&Token::Open(Delimiter::Brace)),
            ),
            Some(("expected type", "expected a type here"))
        );
    }

    #[test]
    fn renders_parse_errors() {
        assert_msg(
            diagnose_parse_error(&Rich::custom(token_span(), "custom parser message")),
            "custom parser message",
        );

        let stream = token_stream(vec![(Token::Semicolon, token_span())]);
        let errors = parser::parse_ast(&stream).expect_err("expected parse error");
        let diagnostic = diagnose_parse_error(&errors[0]);
        assert_msg(&diagnostic, "Unexpected token ';'");
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("unexpected token")
        );

        let source = test_source("fn");
        let stream = token_stream(vec![(
            Token::Keyword(Keyword::Fn),
            SourceSpan::new(source, 0, 2),
        )]);
        let errors = parser::parse_ast(&stream).expect_err("expected parse error");
        let diagnostic = diagnose_parse_error(&errors[0]);
        assert_msg(&diagnostic, "Unexpected end of input");
        assert_eq!(
            diagnostic.primary_label().unwrap().message.as_deref(),
            Some("end of file")
        );
    }
}
