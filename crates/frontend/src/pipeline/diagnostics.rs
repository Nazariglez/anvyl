use anvyx_externs::{
    AbiPosition, AbiTypeError, CallbackEscape, ExternDescriptorError, ExternTypeKey,
};
use anvyx_semantics::FloatToIntError;
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
    resolve::{ModuleId, PackageId, PackageModulePath, ResolveError},
    semantic_id::{NominalId, SourceDeclId},
    source::SourceId,
    span::SourceSpan,
    typecheck::{
        ArityError, BindingNamespace, BindingOrigin, CaptureStorageOrigin, CompileWarning,
        ConstDiagnostic, DeclError, DynContainerConversionKind, ModuleScope, RawEnumValue,
        TryCarrierKind, TypeDiagnosticContext, TypeError, VariantShape, nominal_id_for_type,
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
            format!("Cannot find module file for module '{module}'"),
            None,
        ),
        ResolveError::SourceImportNotFound {
            importer,
            path,
            candidate,
            ..
        } => (
            format!("Cannot find source import '{path}' from '{importer}'"),
            candidate
                .as_ref()
                .map(|candidate| format!("looked for '{}'", candidate.display())),
        ),
        ResolveError::LoadFailed {
            module, message, ..
        } => (format!("Cannot load module '{module}': {message}"), None),
        ResolveError::DuplicatePreloadedModule { module } => (
            format!("module '{module}' is preloaded more than once"),
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
            format!("package '{package}' has no native provider module '{module}'"),
            None,
        ),
        ResolveError::UnknownNativeDepProviderModule {
            package,
            alias,
            module,
            ..
        } => (
            format!(
                "native-only dependency '{alias}' ({package}) has no native provider module '{module}'"
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
            "duplicate provider module '{module}' in package '{}' declared by providers '{}' and '{}'",
            package, first.name, duplicate.name
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
        UnsupportedSourceKind::ReturnPlace => "mutable place return used here",
        UnsupportedSourceKind::CallbackReturn => "callback return used here",
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
        UnsupportedSourceKind::ReturnPlace => {
            "extern functions cannot return mutable places".to_string()
        }
        UnsupportedSourceKind::CallbackReturn => {
            "extern functions cannot return callbacks".to_string()
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
    let prefix = render_unsupported_param_reason(reason);
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

fn render_unsupported_param_reason(reason: UnsupportedSourceParamReason) -> &'static str {
    match reason {
        UnsupportedSourceParamReason::Mutable => "ref",
        UnsupportedSourceParamReason::CastAccept => "cast-accepting",
        UnsupportedSourceParamReason::Default => "default",
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
        RawExternScope::Module(module) => format!("{module}.{name}"),
    }
}

fn render_raw_scope(scope: &RawExternScope) -> String {
    match scope {
        RawExternScope::Module(module) => format!("{module}"),
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
        anvyx_externs::ExternMemberSelector::Operator(op) => op.to_string(),
    }
}

pub(super) fn diagnose_compile_warning(warning: &CompileWarning) -> Diagnostic {
    Diagnostic::warning(warning.message.clone())
        .with_primary_message(warning.span, "compile warning emitted here")
}

fn simple_type_error_message(error: &TypeError) -> Option<String> {
    let message = match error {
        TypeError::UndefinedVariable { name, .. } => format!("Unknown variable '{name}'"),
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
        TypeError::CannotInferEnum { .. } => "cannot infer enum type".to_string(),
        TypeError::InferReturnNonGeneric { .. } => {
            "inferred return type is only allowed on generic callables".to_string()
        }
        TypeError::InferReturnExtern { .. } => {
            "inferred return type is not allowed in extern declarations".to_string()
        }
        TypeError::InferReturnValue { .. } => {
            "generic inferred-return callables cannot be used as values".to_string()
        }
        TypeError::IteratorPlanAsValue { .. } => {
            "iterator plans cannot be used as values".to_string()
        }
        TypeError::InferReturnRecursive { .. } => {
            "recursive inferred return type requires an explicit return type".to_string()
        }
        TypeError::CannotInferConst { .. } => "Could not infer const value".to_string(),
        TypeError::AllNilArrayLiteral { .. } => {
            "cannot infer element type for all-nil array literal".to_string()
        }
        TypeError::ArrayFillLengthNotConst { .. } => {
            "array fill length must be a compile-time constant".to_string()
        }
        _ => return None,
    };
    Some(message)
}

pub(super) fn diagnose_type_error(
    error: &TypeError,
    type_ctx: &TypeDiagnosticContext,
) -> Diagnostic {
    let span = error.span();
    if let Some((message, label)) = type_error_rich_message(error, type_ctx) {
        let diagnostic = Diagnostic::error(message);
        return match span {
            Some(span) => diagnostic.with_primary_message(span, label),
            None => Diagnostic::error(format!("{message}: {label}")),
        };
    }
    if let Some(message) = simple_type_error_message(error) {
        let diagnostic = Diagnostic::error(message);
        return match span {
            Some(span) => diagnostic.with_primary(span),
            None => diagnostic,
        };
    }

    let diagnostic = Diagnostic::error(match error {
        TypeError::Decl(error) => render_decl_error(error, type_ctx),
        TypeError::ExternCatalog(error) => render_extern_catalog_error(error, type_ctx),
        TypeError::UndefinedVariable { .. }
        | TypeError::ExternAnyEscape { .. }
        | TypeError::AnyOutsideExternBoundary { .. }
        | TypeError::RecursiveInference { .. }
        | TypeError::CannotInferType { .. }
        | TypeError::CannotInferEnum { .. }
        | TypeError::InferReturnNonGeneric { .. }
        | TypeError::InferReturnExtern { .. }
        | TypeError::InferReturnValue { .. }
        | TypeError::IteratorPlanAsValue { .. }
        | TypeError::InferReturnRecursive { .. }
        | TypeError::CannotInferConst { .. }
        | TypeError::AllNilArrayLiteral { .. }
        | TypeError::ArrayFillLengthNotConst { .. } => {
            unreachable!("simple type diagnostic handled before message rendering")
        },
        TypeError::TypeUsedAsValue { ty, .. } => {
            format!("type '{}' cannot be used as a value", render_surface_type(ty, type_ctx))
        }
        TypeError::TypeMismatch {
            expected, found, ..
        } => render_type_mismatch(expected, found, type_ctx),
        TypeError::ConstMismatch {
            expected, found, ..
        } => format!(
            "Mismatched types: expected const '{}', found '{}'",
            render_const_diagnostic(expected),
            render_const_diagnostic(found)
        ),
        TypeError::ContractUnsatisfied {
            ty,
            contract,
            detail,
            ..
        } => format!("{} does not satisfy {contract}: {detail}", render_surface_type(ty, type_ctx)),
        TypeError::DynamicMethodMissing {
            contract, method, ..
        } => format!("dynamic contract '{contract}' has no method '{method}'"),
        TypeError::BorrowedDynReassign { name, .. } => {
            format!("cannot reassign borrowed dynamic parameter '{name}'")
        }
        TypeError::DynContainerConversion { kind, .. } => render_dyn_container_conversion(*kind),
        TypeError::NamedFunctionCapture { name, .. } => {
            format!("named functions cannot capture local value '{name}'")
        }
        TypeError::InferReturnMismatch {
            expected, found, ..
        } => format!("inferred return type mismatch: expected '{}', found '{}'", render_surface_type(expected, type_ctx), render_surface_type(found, type_ctx)),
        TypeError::UnsupportedPlaceReturn { message, .. }
        | TypeError::ForIterationModifier { message, .. } => (*message).to_string(),
        TypeError::UnknownType {
            qualifier, name, ..
        } => format!(
            "Unknown type '{}'",
            render_qualified_name(*qualifier, *name)
        ),
        TypeError::NotCallable { ty, .. } => format!("type '{}' is not callable", render_surface_type(ty, type_ctx)),
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
        TypeError::RefArgNonLvalue { .. } => {
            "non-lvalue cannot be passed to ref parameter".to_string()
        }
        TypeError::RefArgImmutableBinding { name, .. } => {
            format!("immutable binding '{name}' cannot be passed to ref parameter")
        }
        TypeError::SequenceStructuralMutationDuringLoan { .. } => {
            "cannot change list shape while iterating it".to_string()
        }
        TypeError::MapStructuralMutationDuringLoan { .. } => {
            "cannot change map shape while iterating it".to_string()
        }
        TypeError::ActiveCollectionRebind { .. } => {
            "cannot rebind iterated collection".to_string()
        }
        TypeError::ActiveCollectionMutableArg { .. } => {
            "cannot pass iterated collection as mutable collection parameter".to_string()
        }
        TypeError::StoredSliceLocal { .. } => "slice values cannot be stored".to_string(),
        TypeError::MutatingMethodImmutableReceiver { name, .. } => {
            format!("mutating method requires mutable receiver '{name}'")
        }
        TypeError::MutableAlias { .. } => {
            "ref arguments must not alias the same variable".to_string()
        }
        TypeError::InvalidFormatSpec { reason, .. } => {
            format!("invalid format specifier: {reason}")
        }
        TypeError::NonEscapingCallbackEscapes { name, .. } => {
            format!("callback parameter '{name}' is non-escaping by default, but it escapes here")
        }
        TypeError::UnknownFunctionValueEscapes { .. } => {
            "cannot retain this function value because its callback ownership is unknown".to_string()
        }
        TypeError::BorrowedCaptureEscapes { name, origin, .. } => {
            borrowed_capture_escape_message(*name, *origin)
        }
        TypeError::RequiresMutablePlace { name, .. } => {
            format!("cannot mutably borrow non-storage place '{name}'")
        }
        TypeError::RefPatternRequiresMutablePlace { .. } => {
            "ref pattern requires a mutable place".to_string()
        }
        TypeError::InvalidOperand {
            op, operand_type, ..
        } => format!("Invalid operand type: operator '{op}' cannot be applied to '{}'", render_surface_type(operand_type, type_ctx)),
        TypeError::MissingReturn { expected, .. } => {
            format!("Mismatched types: expected '{}', found 'void'", render_surface_type(expected, type_ctx))
        }
        TypeError::IfWithoutElseValue { .. } => {
            "if expression used as value must have an else branch".to_string()
        }
        TypeError::IfConditionNotBool { found, .. } => {
            format!("Condition of if expression must be bool: found '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::TernaryConditionNotBool { found, .. } => {
            format!("Condition of ternary expression must be bool: found '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::WhileConditionNotBool { found, .. } => {
            format!("Condition of while must be bool: found '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::BreakOutsideLoop { .. } => "break outside of loop".to_string(),
        TypeError::ContinueOutsideLoop { .. } => "continue outside of loop".to_string(),
        TypeError::ReturnInsideDefer { .. } => "return inside defer".to_string(),
        TypeError::BreakInsideDefer { .. } => "break inside defer".to_string(),
        TypeError::ContinueInsideDefer { .. } => "continue inside defer".to_string(),
        TypeError::TryOnInvalidCarrier {
            expected, found, ..
        } => {
            format!("try requires {}, found '{}'", expected.label(), render_surface_type(found, type_ctx))
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
            "try requires enclosing function to return {}, found '{}'",
            TryCarrierKind::any_label(),
            render_surface_type(found, type_ctx)
        ),
        TypeError::TryResultErrorMismatch {
            expected, found, ..
        } => format!("try error type mismatch: expected '{}', found '{}'", render_surface_type(expected, type_ctx), render_surface_type(found, type_ctx)),
        TypeError::TryInsideDefer { .. } => "try inside defer".to_string(),
        TypeError::ForIterableNotSupported { found, .. } => {
            format!("type '{}' cannot be iterated", render_surface_type(found, type_ctx))
        }
        TypeError::ForRefRequiresMutableIterable { .. } => {
            "ref iteration requires a mutable iterable place".to_string()
        }
        TypeError::ForMutableMapKey { .. } => {
            "map keys cannot be mutable; use `for k, ref v in map` to mutate values".to_string()
        }
        TypeError::ForMutableMapEntry { .. } => {
            "mutable map entry iteration is not supported; use `for k, ref v in map` to mutate values".to_string()
        }
        TypeError::RefutableForPattern { .. } => {
            "refutable pattern is not allowed in for binding".to_string()
        }
        TypeError::InfiniteSize { name, .. } => {
            format!("recursive value type '{name}' has infinite size")
        }
        TypeError::NotEquatable { ty, .. } => format!("type '{}' is not equatable", render_surface_type(ty, type_ctx)),
        TypeError::UnsupportedPattern { pattern, .. } => format!("Unsupported pattern: {pattern}"),
        TypeError::TuplePatternArityMismatch {
            expected, found, ..
        } => {
            format!("tuple pattern arity mismatch: expected {expected}, found {found}")
        }
        TypeError::TuplePatternOnNonTuple { ty, .. } => {
            format!("cannot destructure non-tuple type '{}'", render_surface_type(ty, type_ctx))
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
            "or-pattern binding '{name}' type mismatch: expected '{}', found '{}'",
            render_surface_type(expected, type_ctx),
            render_surface_type(found, type_ctx)
        ),
        TypeError::EmptyMatch { .. } => "match expression must have at least one arm".to_string(),
        TypeError::NonExhaustiveMatch { .. } => "non-exhaustive match".to_string(),
        TypeError::UnsupportedMatchScrutinee { found, .. } => {
            format!("unsupported match scrutinee: '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::InvalidLiteralPattern {
            expected, found, ..
        } => format!("invalid literal pattern: mismatch expected '{}', found '{}'", render_surface_type(expected, type_ctx), render_surface_type(found, type_ctx)),
        TypeError::OptionalPatternOnNonOptional { .. } => {
            "optional pattern requires an optional scrutinee".to_string()
        }
        TypeError::OptionalChainingOnNonOptional { .. } => {
            "optional chaining requires an optional base type".to_string()
        }
        TypeError::NestedOptionalPattern { .. } => {
            "nested optional patterns are not supported".to_string()
        }
        TypeError::UnsupportedOptionalPayloadPattern { .. } => {
            "optional payload patterns only support identifiers and wildcards".to_string()
        }
        TypeError::MatchArmTypeMismatch {
            expected, found, ..
        } => format!("match arm type mismatch: expected '{}', found '{}'", render_surface_type(expected, type_ctx), render_surface_type(found, type_ctx)),
        TypeError::RequiresUnwrappingPattern { .. } => {
            "optional value requires an unwrapping pattern".to_string()
        }
        TypeError::IrrefutableLetElse { .. } => "irrefutable pattern in let-else".to_string(),
        TypeError::LetElseMustDiverge { .. } => {
            "let-else fallback must always return, break, or continue".to_string()
        }
        TypeError::MemberAccessOnNonAggregate {
            ty, member, kind, ..
        }
        | TypeError::UnknownMember {
            ty, member, kind, ..
        } => {
            let kind = kind.diagnostic_name();
            format!("Unknown {kind} '{member}' for type '{}'", render_surface_type(ty, type_ctx))
        }
        TypeError::AmbiguousPromotedField {
            ty,
            member,
            candidates,
            ..
        } => {
            let candidates = render_ident_paths(candidates);
            format!("promoted field '{member}' for type '{}' is ambiguous: {candidates}", render_surface_type(ty, type_ctx))
        }
        TypeError::AmbiguousPromotedMethod {
            ty,
            member,
            candidates,
            ..
        } => {
            let candidates = render_ident_paths(candidates);
            format!("promoted method '{member}' for type '{}' is ambiguous: {candidates}", render_surface_type(ty, type_ctx))
        }
        TypeError::PromotedFieldNotStored {
            ty, field, paths, ..
        } => {
            let paths = render_ident_paths(paths);
            format!(
                "promoted field '{field}' is not a stored field of '{}'; use stored path {paths}",
                render_surface_type(ty, type_ctx)
            )
        }
        TypeError::AmbiguousProjection {
            source,
            target,
            paths,
            ..
        } => {
            let paths = render_ident_paths(paths);
            format!("projection from '{}' to '{}' is ambiguous; @as embed paths: {paths}", render_surface_type(source, type_ctx), render_surface_type(target, type_ctx))
        }
        TypeError::MissingProjection {
            source,
            target,
            paths,
            ..
        } => render_missing_projection(source, target, paths, type_ctx),
        TypeError::InstanceMethodOnType { ty, method, .. } => {
            format!("instance method '{method}' requires a value of type '{}'", render_surface_type(ty, type_ctx))
        }
        TypeError::StaticMethodOnValue { ty, method, .. } => {
            format!("static method '{method}' must be called on type '{}'", render_surface_type(ty, type_ctx))
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
            format!("cannot index non-tuple type with .{index}: found '{}'", render_surface_type(ty, type_ctx))
        }
        TypeError::TupleIndexOutOfBounds { index, len, .. } => {
            format!("tuple index {index} is out of bounds for tuple of length {len}")
        }
        TypeError::ArrayIndexOutOfBounds { index, len, .. } => {
            format!("array index {index} is out of bounds for array of length {len}")
        }
        TypeError::IndexNotInt { found, .. } => {
            format!("index must be an integer: found '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::IndexOnNonIndexable { found, .. } => {
            format!("cannot index non-array type '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::RangeIndexNotInt { found, .. } => {
            format!("range index bounds must be int, found '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::RangeIndexUnsupported { found, .. } => {
            format!("range indexing is not supported for type '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::NonKeyableMapKey { ty, field, .. } => match field {
            Some(field) => {
                format!("field '{field}' is not keyable: type '{}' cannot be used as map key", render_surface_type(ty, type_ctx))
            }
            None if semantic_option_inner(ty, type_ctx).is_some() => {
                format!("optional type '{}' is not keyable and cannot be used as map key", render_surface_type(ty, type_ctx))
            }
            None => format!("type '{}' is not keyable and cannot be used as map key", render_surface_type(ty, type_ctx)),
        },
        TypeError::DuplicateMapKey { .. } => "duplicate key in map literal".to_string(),
        TypeError::UndefinedModuleMember { module, name, .. } => {
            format!("Unknown member '{name}' in module '{module}'")
        }
        TypeError::PrivateModuleMember { module, name, .. } => {
            format!("member '{name}' in module '{module}' is private")
        }
        TypeError::AmbiguousExtendMethod { receiver, name, .. } => {
            format!("ambiguous method '{name}' for type '{}'", render_surface_type(receiver, type_ctx))
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
            format!("pattern does not match scrutinee enum: expected '{}', found '{}'", render_surface_type(expected, type_ctx), render_surface_type(found, type_ctx))
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
        TypeError::InvalidDefaultExpression { kind, .. } => {
            format!("{kind} expressions cannot be used as default initializers")
        }
        TypeError::DefaultReferencesParameter { name, .. } => {
            format!("default initializer cannot reference parameter '{name}'")
        }
        TypeError::DefaultReferencesSelf { .. } => {
            "default initializer cannot reference self".to_string()
        }
        TypeError::DefaultReferencesField { name, .. } => {
            format!("field default initializer cannot reference sibling field '{name}'")
        }
        TypeError::RefParamDefault { name, .. } => {
            format!("ref parameter '{name}' cannot have a default initializer")
        }
        TypeError::ConstTypeMismatch {
            expected, found, ..
        } => {
            format!("constant type mismatch: expected '{}', found '{}'", render_surface_type(expected, type_ctx), render_surface_type(found, type_ctx))
        }
        TypeError::RawEnumExpectedIntValue { found, .. } => {
            format!("raw int enum value must be an int constant, found '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::RawEnumExpectedStringValue { found, .. } => {
            format!("raw string enum value must be a string constant, found '{}'", render_surface_type(found, type_ctx))
        }
        TypeError::InvalidConstCast { from, to, .. } => {
            format!("cannot cast constant from '{}' to '{}'", render_surface_type(from, type_ctx), render_surface_type(to, type_ctx))
        }
        TypeError::ConstFloatToInt { error, .. } => match error {
            FloatToIntError::NonFinite => "cannot convert non-finite float to int".to_string(),
            FloatToIntError::OutOfRange => "float is out of range for int".to_string(),
        },
        TypeError::InvalidCast { from, to, .. } => {
            format!("Invalid cast: cannot cast from '{}' to '{}'", render_surface_type(from, type_ctx), render_surface_type(to, type_ctx))
        }
        TypeError::AmbiguousCast { from, to, .. } => {
            format!("ambiguous cast from '{}' to '{}'", render_surface_type(from, type_ctx), render_surface_type(to, type_ctx))
        }
        TypeError::RawEnumWrongRawCast { enum_ty, expected, found, .. } => {
            format!("raw enum '{}' casts only to '{}', not '{}'", render_surface_type(enum_ty, type_ctx), render_surface_type(expected, type_ctx), render_surface_type(found, type_ctx))
        }
        TypeError::NonRawEnumRawCast { enum_ty, raw_ty, .. } => {
            format!("non-raw enum '{}' cannot be cast to '{}'", render_surface_type(enum_ty, type_ctx), render_surface_type(raw_ty, type_ctx))
        }
        TypeError::ConstDivisionByZero { .. } => {
            "division by zero in constant expression".to_string()
        }
        TypeError::ConstOverflow { .. } => "constant expression overflow".to_string(),
        TypeError::ExpectedIntConst { found, .. } => {
            format!("expected integer constant, found '{}'", render_surface_type(found, type_ctx))
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
        TypeError::InfiniteSize { .. } => diagnostic.with_help(
            "use `dataref` for shared nodes, or `[T]` / `[K: V]` for indirect value containers",
        ),
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
        CaptureStorageOrigin::RefSelf => {
            "cannot capture mutable receiver 'self' in an escaping lambda".to_string()
        }
        CaptureStorageOrigin::DynView => {
            format!("cannot capture borrowed dynamic view '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::PatternAlias => {
            format!("cannot capture mutable pattern alias '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::ForRefAlias => {
            format!("cannot capture mutable loop alias '{name}' in an escaping lambda")
        }
        CaptureStorageOrigin::Owned
        | CaptureStorageOrigin::Const
        | CaptureStorageOrigin::ReadonlySelf => {
            format!("cannot capture '{name}' in an escaping lambda")
        }
    }
}

fn type_error_rich_message(
    error: &TypeError,
    type_ctx: &TypeDiagnosticContext,
) -> Option<(&'static str, String)> {
    let message = match error {
        TypeError::TypeMismatch {
            expected, found, ..
        } => {
            let (expected, found) = render_type_mismatch_parts(expected, found, type_ctx);
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
                render_surface_type(operand_type, type_ctx)
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
        TypeError::IfConditionNotBool { found, .. } => (
            "if condition must be bool",
            condition_type_label(found, type_ctx),
        ),
        TypeError::TernaryConditionNotBool { found, .. } => (
            "ternary condition must be bool",
            condition_type_label(found, type_ctx),
        ),
        TypeError::WhileConditionNotBool { found, .. } => (
            "while condition must be bool",
            condition_type_label(found, type_ctx),
        ),
        TypeError::MissingReturn { expected, .. } => (
            "missing return value",
            format!(
                "expected `{}`, found `void`",
                render_surface_type(expected, type_ctx)
            ),
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
            format!(
                "type '{}' is not callable",
                render_surface_type(ty, type_ctx)
            ),
        ),
        TypeError::TupleIndexOnNonTuple { ty, index, .. } => (
            "Invalid tuple index",
            format!(
                "cannot index non-tuple type with .{index}; found '{}'",
                render_surface_type(ty, type_ctx)
            ),
        ),
        TypeError::TupleIndexOutOfBounds { index, len, .. } => (
            "Tuple index out of bounds",
            format!("tuple index {index} is out of bounds for length {len}"),
        ),
        TypeError::ArrayIndexOutOfBounds { index, len, .. } => (
            "Array index out of bounds",
            format!("array index {index} is out of bounds for length {len}"),
        ),
        TypeError::IndexNotInt { found, .. } => (
            "Invalid index type",
            format!(
                "index must be an integer; found '{}'",
                render_surface_type(found, type_ctx)
            ),
        ),
        TypeError::IndexOnNonIndexable { found, .. } => (
            "Not indexable",
            format!(
                "cannot index non-array type '{}'",
                render_surface_type(found, type_ctx)
            ),
        ),
        TypeError::RangeIndexNotInt { found, .. } => (
            "Invalid range index type",
            format!(
                "range index bounds must be integers; found '{}'",
                render_surface_type(found, type_ctx)
            ),
        ),
        TypeError::RangeIndexUnsupported { found, .. } => (
            "Range indexing unsupported",
            format!(
                "range indexing is not supported for type '{}'",
                render_surface_type(found, type_ctx)
            ),
        ),
        _ => return None,
    };
    Some(message)
}

fn expected_found_label(expected: &str, found: &str) -> String {
    format!("expected `{expected}`, found `{found}`")
}

fn condition_type_label(found: &Type, type_ctx: &TypeDiagnosticContext) -> String {
    format!(
        "condition must be `bool`, found `{}`",
        render_surface_type(found, type_ctx)
    )
}

fn render_raw_enum_value(value: &RawEnumValue) -> String {
    match value {
        RawEnumValue::Int(value) => value.to_string(),
        RawEnumValue::String(value) => format!("{value:?}"),
    }
}

fn render_variant_shape(shape: VariantShape) -> &'static str {
    match shape {
        VariantShape::Unit => "unit",
        VariantShape::Tuple => "tuple",
        VariantShape::Struct => "struct",
    }
}

fn render_type_mismatch(expected: &Type, found: &Type, type_ctx: &TypeDiagnosticContext) -> String {
    let (expected, found) = render_type_mismatch_parts(expected, found, type_ctx);
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

fn render_type_mismatch_parts(
    expected: &Type,
    found: &Type,
    type_ctx: &TypeDiagnosticContext,
) -> (String, String) {
    let expected_surface = render_surface_type(expected, type_ctx);
    let found_surface = render_surface_type(found, type_ctx);
    if expected != found && expected_surface == found_surface {
        let expected_detail = render_detailed_type(expected);
        let found_detail = render_detailed_type(found);
        let (expected_sites, found_sites) = differing_nominal_sites(expected, found);
        if !expected_sites.is_empty() || !found_sites.is_empty() {
            return (
                render_type_with_sites(expected_detail, expected_sites, type_ctx),
                render_type_with_sites(found_detail, found_sites, type_ctx),
            );
        }
        return (expected_detail, found_detail);
    }
    (expected_surface, found_surface)
}

fn render_surface_type(ty: &Type, type_ctx: &TypeDiagnosticContext) -> String {
    surface_type(ty, type_ctx).to_string()
}

fn surface_type(ty: &Type, type_ctx: &TypeDiagnosticContext) -> Type {
    if let Some(inner) = semantic_option_inner(ty, type_ctx) {
        return Type::optional_syntax(surface_type(inner, type_ctx));
    }
    match ty {
        Type::Func { params, ret } => Type::func(
            params
                .iter()
                .map(|param| FuncParam {
                    ty: surface_type(&param.ty, type_ctx),
                    mutable: param.mutable,
                    cast_accept: param.cast_accept,
                    escape: param.escape,
                })
                .collect(),
            ret.with_ty(surface_type(&ret.ty(), type_ctx)),
        ),
        Type::Dyn(contract) => Type::Dyn(surface_contract(contract, type_ctx)),
        Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => Type::UnresolvedNominal {
            qualifier: *qualifier,
            name: *name,
            generic_args: generic_args
                .iter()
                .map(|arg| match arg {
                    ast::GenericArg::Type(ty) => ast::GenericArg::Type(surface_type(ty, type_ctx)),
                    ast::GenericArg::Const(arg) => ast::GenericArg::Const(arg.clone()),
                })
                .collect(),
        },
        Type::Tuple(elems) => Type::Tuple(surface_types(elems, type_ctx)),
        Type::Nominal(nominal) => nominal.with_args(
            surface_types(&nominal.type_args, type_ctx),
            nominal.const_args.clone(),
        ),
        Type::List { elem } => Type::List {
            elem: Box::new(surface_type(elem, type_ctx)),
        },
        Type::Array { elem, len } => Type::Array {
            elem: Box::new(surface_type(elem, type_ctx)),
            len: len.clone(),
        },
        Type::Map { key, value } => Type::Map {
            key: Box::new(surface_type(key, type_ctx)),
            value: Box::new(surface_type(value, type_ctx)),
        },
        Type::Slice { elem } => Type::Slice {
            elem: Box::new(surface_type(elem, type_ctx)),
        },
        Type::Optional { inner } => Type::optional_syntax(surface_type(inner, type_ctx)),
        Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Char
        | Type::Void
        | Type::Var(_)
        | Type::UnresolvedName(_) => ty.clone(),
    }
}

fn surface_types(types: &[Type], type_ctx: &TypeDiagnosticContext) -> Vec<Type> {
    types.iter().map(|ty| surface_type(ty, type_ctx)).collect()
}

fn surface_contract(
    contract: &ast::ContractRef,
    type_ctx: &TypeDiagnosticContext,
) -> ast::ContractRef {
    match contract {
        ast::ContractRef::Anonymous(surface) => {
            ast::ContractRef::Anonymous(ast::AnonymousContract {
                requirements: surface
                    .requirements
                    .iter()
                    .map(|requirement| ast::AnonymousContractRequirement {
                        receiver: requirement.receiver,
                        name: requirement.name,
                        params: requirement
                            .params
                            .iter()
                            .map(|param| ast::AnonymousContractParam {
                                mutable: param.mutable,
                                escape: param.escape,
                                name: param.name,
                                ty: surface_type(&param.ty, type_ctx),
                            })
                            .collect(),
                        ret: requirement
                            .ret
                            .with_ty(surface_type(&requirement.ret.ty(), type_ctx)),
                    })
                    .collect(),
            })
        }
        ast::ContractRef::Intersection(contracts) => ast::ContractRef::Intersection(
            contracts
                .iter()
                .map(|contract| surface_contract(contract, type_ctx))
                .collect(),
        ),
        ast::ContractRef::Named { .. } | ast::ContractRef::Infer | ast::ContractRef::Hole(_) => {
            contract.clone()
        }
    }
}

fn semantic_option_inner<'a>(ty: &'a Type, type_ctx: &TypeDiagnosticContext) -> Option<&'a Type> {
    let Type::Nominal(nominal) = ty else {
        return None;
    };
    if nominal_id_for_type(ty)? != &type_ctx.core_option()?.id {
        return None;
    }
    let [inner] = nominal.type_args.as_slice() else {
        return None;
    };
    nominal.const_args.is_empty().then_some(inner)
}

fn differing_nominal_sites(
    expected: &Type,
    found: &Type,
) -> (Vec<SourceDeclId>, Vec<SourceDeclId>) {
    let mut expected_sites = vec![];
    let mut found_sites = vec![];
    collect_differing_nominal_sites(expected, found, &mut expected_sites, &mut found_sites);
    expected_sites.sort_by_key(source_decl_sort_key);
    expected_sites.dedup();
    found_sites.sort_by_key(source_decl_sort_key);
    found_sites.dedup();
    (expected_sites, found_sites)
}

fn collect_differing_nominal_sites(
    expected: &Type,
    found: &Type,
    expected_sites: &mut Vec<SourceDeclId>,
    found_sites: &mut Vec<SourceDeclId>,
) {
    match (expected, found) {
        (Type::Nominal(expected), Type::Nominal(found)) => {
            if expected.id != found.id {
                if let NominalId::Source(site) = &expected.id {
                    expected_sites.push(*site);
                }
                if let NominalId::Source(site) = &found.id {
                    found_sites.push(*site);
                }
            }
            collect_type_pairs(
                &expected.type_args,
                &found.type_args,
                expected_sites,
                found_sites,
            );
        }
        (
            Type::Func {
                params: expected_params,
                ret: expected_ret,
            },
            Type::Func {
                params: found_params,
                ret: found_ret,
            },
        ) => {
            for (expected, found) in expected_params.iter().zip(found_params) {
                collect_differing_nominal_sites(
                    &expected.ty,
                    &found.ty,
                    expected_sites,
                    found_sites,
                );
            }
            collect_differing_nominal_sites(
                &expected_ret.ty(),
                &found_ret.ty(),
                expected_sites,
                found_sites,
            );
        }
        (Type::Dyn(expected), Type::Dyn(found)) => {
            collect_contract_nominal_sites(expected, found, expected_sites, found_sites);
        }
        (
            Type::UnresolvedNominal {
                generic_args: expected,
                ..
            },
            Type::UnresolvedNominal {
                generic_args: found,
                ..
            },
        ) => {
            for (expected, found) in expected.iter().zip(found) {
                if let (ast::GenericArg::Type(expected), ast::GenericArg::Type(found)) =
                    (expected, found)
                {
                    collect_differing_nominal_sites(expected, found, expected_sites, found_sites);
                }
            }
        }
        (Type::Tuple(expected), Type::Tuple(found)) => {
            collect_type_pairs(expected, found, expected_sites, found_sites);
        }
        (Type::List { elem: expected }, Type::List { elem: found })
        | (Type::Array { elem: expected, .. }, Type::Array { elem: found, .. })
        | (Type::Slice { elem: expected }, Type::Slice { elem: found })
        | (Type::Optional { inner: expected }, Type::Optional { inner: found }) => {
            collect_differing_nominal_sites(expected, found, expected_sites, found_sites);
        }
        (
            Type::Map {
                key: expected_key,
                value: expected_value,
            },
            Type::Map {
                key: found_key,
                value: found_value,
            },
        ) => {
            collect_differing_nominal_sites(expected_key, found_key, expected_sites, found_sites);
            collect_differing_nominal_sites(
                expected_value,
                found_value,
                expected_sites,
                found_sites,
            );
        }
        _ => {}
    }
}

fn collect_contract_nominal_sites(
    expected: &ast::ContractRef,
    found: &ast::ContractRef,
    expected_sites: &mut Vec<SourceDeclId>,
    found_sites: &mut Vec<SourceDeclId>,
) {
    match (expected, found) {
        (ast::ContractRef::Anonymous(expected), ast::ContractRef::Anonymous(found)) => {
            for (expected, found) in expected.requirements.iter().zip(&found.requirements) {
                for (expected, found) in expected.params.iter().zip(&found.params) {
                    collect_differing_nominal_sites(
                        &expected.ty,
                        &found.ty,
                        expected_sites,
                        found_sites,
                    );
                }
                collect_differing_nominal_sites(
                    &expected.ret.ty(),
                    &found.ret.ty(),
                    expected_sites,
                    found_sites,
                );
            }
        }
        (ast::ContractRef::Intersection(expected), ast::ContractRef::Intersection(found)) => {
            for (expected, found) in expected.iter().zip(found) {
                collect_contract_nominal_sites(expected, found, expected_sites, found_sites);
            }
        }
        _ => {}
    }
}

fn collect_type_pairs(
    expected: &[Type],
    found: &[Type],
    expected_sites: &mut Vec<SourceDeclId>,
    found_sites: &mut Vec<SourceDeclId>,
) {
    for (expected, found) in expected.iter().zip(found) {
        collect_differing_nominal_sites(expected, found, expected_sites, found_sites);
    }
}

fn source_decl_sort_key(site: &SourceDeclId) -> (usize, usize, usize) {
    let span = site.span();
    (site.source().index(), span.start, span.end)
}

fn render_type_with_sites(
    mut rendered: String,
    sites: Vec<SourceDeclId>,
    type_ctx: &TypeDiagnosticContext,
) -> String {
    let locations = sites
        .into_iter()
        .filter_map(|site| render_source_decl_location(site, type_ctx))
        .collect::<Vec<_>>();
    if !locations.is_empty() {
        rendered.push_str(" (declared at ");
        rendered.push_str(&locations.join(", "));
        rendered.push(')');
    }
    rendered
}

fn render_source_decl_location(
    site: SourceDeclId,
    type_ctx: &TypeDiagnosticContext,
) -> Option<String> {
    let source = type_ctx.sources()?.get(site.source())?;
    let position = source.line_index().byte_to_line_col(site.span().start)?;
    Some(format!(
        "{}:{}:{}",
        source.label(),
        position.line + 1,
        position.column + 1
    ))
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
        rendered.push_str("ref ");
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
            rendered.push_str("ref ");
        }
        rendered.push_str(&render_detailed_type(&ret.ty()));
    }
    rendered
}

fn render_wrapped_types(
    open: &str,
    close: &str,
    elems: &[Type],
    render: impl Fn(&Type) -> String,
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

fn render_extern_catalog_error(
    error: &ExternCatalogError,
    type_ctx: &TypeDiagnosticContext,
) -> String {
    let context = error.context();
    let message = match error {
        ExternCatalogError::UnknownType { module, name, .. } => {
            let ty = module.as_ref().map_or_else(
                || name.to_string(),
                |module| render_scoped_name(module, *name),
            );
            format!(
                "Unknown extern type '{ty}' in {}",
                render_extern_item(context)
            )
        }
        ExternCatalogError::PrivateType { module, name, .. } => format!(
            "extern type '{}' in {} is private",
            render_scoped_name(module, *name),
            render_extern_item(context)
        ),
        ExternCatalogError::GenericArity {
            name,
            expected,
            found,
            ..
        } => format!(
            "wrong number of extern type arguments for '{name}' in {}: expected {expected}, found {found}",
            render_extern_item(context)
        ),
        ExternCatalogError::GenericArgKindMismatch { name, expected, .. } => format!(
            "expected {expected} extern generic argument for '{name}' in {}",
            render_extern_item(context)
        ),
        ExternCatalogError::InvalidType { ty, reason, .. } => {
            let item = render_extern_item(context);
            match reason {
                InvalidExternTypeReason::Unresolved => format!(
                    "unresolved extern type '{}' in {item}",
                    render_surface_type(ty, type_ctx)
                ),
                InvalidExternTypeReason::Infer => format!(
                    "extern type '{}' contains inference in {item}",
                    render_surface_type(ty, type_ctx)
                ),
                InvalidExternTypeReason::UnresolvedConst => format!(
                    "extern type '{}' contains an unresolved const argument in {item}",
                    render_surface_type(ty, type_ctx)
                ),
                InvalidExternTypeReason::MissingCoreOption => format!(
                    "extern type '{}' requires the core Option type in {item}",
                    render_surface_type(ty, type_ctx)
                ),
                InvalidExternTypeReason::MissingCoreResult => format!(
                    "extern type '{}' requires the core Result type in {item}",
                    render_surface_type(ty, type_ctx)
                ),
                InvalidExternTypeReason::NonKeyableMapKey => format!(
                    "extern map key type '{}' is not keyable in {item}",
                    render_surface_type(ty, type_ctx)
                ),
            }
        }
        ExternCatalogError::InvalidAbiType {
            position, reason, ..
        } => format!(
            "invalid native ABI type in {} for {}: {}",
            render_abi_position(*position),
            render_extern_item(context),
            render_abi_type_error(*reason),
        ),
        ExternCatalogError::UnknownInitField { field, .. } => format!(
            "{} references unknown field '{field}'",
            render_extern_item(context)
        ),
        ExternCatalogError::InitFieldTypeMismatch {
            field,
            expected,
            found,
            ..
        } => format!(
            "{} init field '{field}' has type '{}', but the init parameter has type '{}'",
            render_extern_item(context),
            render_surface_type(expected, type_ctx),
            render_surface_type(found, type_ctx)
        ),
        ExternCatalogError::InvalidOperatorReturn {
            found, expected, ..
        } => format!(
            "invalid {}: expected {} return type, found '{}'",
            render_extern_item(context),
            expected,
            render_surface_type(found, type_ctx)
        ),
    };
    format!(
        "{message} from {}",
        render_extern_provenance(&context.provenance)
    )
}

fn render_callback_escape(escape: CallbackEscape) -> &'static str {
    match escape {
        CallbackEscape::NonEscaping => "non-escaping",
        CallbackEscape::Escaping => "escaping",
    }
}

fn render_abi_position(position: AbiPosition) -> &'static str {
    match position {
        AbiPosition::Return => "return position",
        AbiPosition::ParamValue => "value parameter position",
        AbiPosition::ParamBorrow => "borrowed parameter position",
        AbiPosition::ParamMutBorrow => "mutable parameter position",
        AbiPosition::CallbackParam => "callback parameter position",
        AbiPosition::CallbackReturn => "callback return position",
        AbiPosition::Field => "field position",
        AbiPosition::Nested => "nested type position",
    }
}

fn render_abi_type_error(error: AbiTypeError) -> &'static str {
    match error {
        AbiTypeError::VoidOutsideReturn => "void is only valid as a callable return",
        AbiTypeError::SliceOutsideParam => "slice views are parameter-only",
        AbiTypeError::SliceNested => "slice views cannot be nested",
        AbiTypeError::CallbackOutsideParam => "callbacks are parameter-only",
        AbiTypeError::CallbackNested => "callbacks cannot be nested",
        AbiTypeError::CallbackReturnUnsupported => "callbacks cannot be returned from callbacks",
        AbiTypeError::CallbackThreadUnsupported => "callbacks must use the same runtime thread",
        AbiTypeError::GenericNamedArgsUnsupported => {
            "named provider ABI types must be closed and non-generic"
        }
    }
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
            format!("extern operator {owner}.{op}")
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
        ModuleScope::Named(path) => format!("{path}.{name}"),
        ModuleScope::Package(module) => format!("{module}.{name}"),
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

fn render_missing_projection(
    source: &Type,
    target: &Type,
    paths: &[Vec<Ident>],
    type_ctx: &TypeDiagnosticContext,
) -> String {
    format!(
        "no `@as embed` projection path from '{}' to '{}'; use stored path {} explicitly or mark it with `@as embed`",
        render_surface_type(source, type_ctx),
        render_surface_type(target, type_ctx),
        render_ident_paths(paths)
    )
}

fn render_decl_error(error: &DeclError, type_ctx: &TypeDiagnosticContext) -> String {
    match error {
        DeclError::DuplicateValue { name, .. } => format!("value '{name}' is already declared"),
        DeclError::DuplicateType { name, .. } => format!("type '{name}' is already defined"),
        DeclError::MissingImportMember { imported, name, .. } => {
            format!("Unknown member '{name}' in module '{imported}'")
        }
        DeclError::PrivateImportMember { imported, name, .. } => {
            format!("member '{name}' in module '{imported}' is private")
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
        DeclError::DuplicateAggregateField { owner, name, .. } => {
            format!("duplicate field '{name}' in '{}'", owner.name)
        }
        DeclError::DuplicateEnumVariant { owner, name, .. } => {
            format!("duplicate variant '{name}' in enum '{}'", owner.name)
        }
        DeclError::RawEnumInvalidBacking { owner, .. } => {
            format!(
                "raw enum '{}' must use literal int or string backing",
                owner.name
            )
        }
        DeclError::RawEnumGenericParams { owner, .. } => {
            format!("raw enum '{}' cannot be generic", owner.name)
        }
        DeclError::RawEnumOwnerDependency { owner, .. } => {
            format!(
                "raw enum '{}' cannot depend on enclosing generic parameters",
                owner.name
            )
        }
        DeclError::RawEnumValueWithoutBacking { owner, variant, .. } => {
            format!(
                "variant '{}.{variant}' cannot have a raw value without enum backing",
                owner.name
            )
        }
        DeclError::RawEnumPayloadVariant { owner, variant, .. } => {
            format!(
                "raw enum variant '{}.{variant}' must be a unit variant",
                owner.name
            )
        }
        DeclError::RawEnumMissingStringValue { owner, variant, .. } => {
            format!(
                "raw string enum variant '{}.{variant}' must have an explicit string value",
                owner.name
            )
        }
        DeclError::RawEnumDuplicateValue {
            owner,
            variant,
            value,
            ..
        } => {
            format!(
                "duplicate raw enum value {} on variant '{}.{variant}'",
                render_raw_enum_value(value),
                owner.name
            )
        }
        DeclError::RawEnumIntOverflow { owner, variant, .. } => {
            format!(
                "raw int enum auto-increment overflows at variant '{}.{variant}'",
                owner.name
            )
        }
        DeclError::DuplicateVariantField {
            owner,
            variant,
            name,
            ..
        } => format!(
            "duplicate field '{name}' in variant '{}.{variant}'",
            owner.name
        ),
        DeclError::DuplicateContractRequirement { contract, name, .. } => format!(
            "duplicate contract requirement '{name}' in contract '{}'",
            contract.name
        ),
        DeclError::DuplicateExtendMethod { name, surface, .. } => {
            format!("duplicate extend {} method '{name}'", surface.label())
        }
        DeclError::DuplicateCastFrom {
            kind,
            source,
            target,
            ..
        } => format!(
            "duplicate {} '{}' to '{}'",
            kind.syntax(),
            render_surface_type(source, type_ctx),
            render_surface_type(target, type_ctx)
        ),
        DeclError::ConflictingCastFrom { source, target, .. } => format!(
            "cast from and cast? from conflict for '{}' to '{}'",
            render_surface_type(source, type_ctx),
            render_surface_type(target, type_ctx)
        ),
        DeclError::PointlessCastFrom { ty, .. } => {
            format!(
                "pointless cast from '{}' to itself",
                render_surface_type(ty, type_ctx)
            )
        }
        DeclError::CastFromReturnMismatch {
            kind,
            expected,
            found,
            ..
        } => format!(
            "{} return type mismatch: expected '{}', found '{}'",
            kind.syntax(),
            render_surface_type(expected, type_ctx),
            render_surface_type(found, type_ctx)
        ),
        DeclError::UnsupportedExtendTarget { ty, .. } => {
            format!("cannot extend type '{}'", render_surface_type(ty, type_ctx))
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
        DeclError::ExtendMethodConflict {
            ty, name, surface, ..
        } => format!(
            "{} method '{name}' already exists for type '{}'",
            surface.label(),
            render_surface_type(ty, type_ctx)
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
            format!("'{source}'")
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
        ConstValue::Char(value) => value.to_string(),
    }
}

fn render_extern_descriptor_error(
    error: &ExternDescriptorError,
    raw_scope: Option<&RawExternScope>,
) -> String {
    match error {
        ExternDescriptorError::InvalidName { kind, name } => {
            format!("invalid {kind} name '{name}'")
        }
        ExternDescriptorError::EmptyModulePath => "module path must not be empty".to_string(),
        ExternDescriptorError::DuplicateModule(path) => {
            format!("duplicate module '{path}'")
        }
        ExternDescriptorError::DuplicateType { module, name } => {
            format!("duplicate type '{name}' in module '{module}'")
        }
        ExternDescriptorError::DuplicateFunction { module, name } => {
            format!("duplicate function '{name}' in module '{module}'")
        }
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
            "duplicate operator '{op}' on extern type '{}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InvalidOperatorSignature {
            ty,
            op,
            expected_params,
            actual_params,
        } => format!(
            "invalid operator '{op}' on extern type '{}': expected {expected_params} parameter(s), found {actual_params}",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InvalidOperatorReturn {
            ty,
            op,
            expected,
            actual,
        } => format!(
            "invalid operator '{op}' on extern type '{}': expected {expected} return type, found '{actual}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::DuplicateFieldInit { ty, name } => format!(
            "duplicate init field '{name}' on extern type '{}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::UnnamedInitParam { ty, index } => format!(
            "extern init parameter {index} on type '{}' has no source-visible name",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InitParamFieldCountMismatch {
            ty,
            params,
            field_init,
        } => format!(
            "extern init on type '{}' has {params} parameter(s) but {field_init} field initializer(s)",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InitParamFieldMismatch {
            ty,
            index,
            param,
            field,
        } => format!(
            "extern init parameter {index} ('{param}') on type '{}' does not match field initializer '{field}'",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::MixedVariantFields { ty, variant } => format!(
            "extern enum variant '{}.{variant}' cannot mix named and unnamed fields",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InvalidRepresentationMetadata { ty } => format!(
            "extern type '{}' has incomplete or incompatible representation metadata",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::InvalidLayout { ty, size, align } => format!(
            "extern type '{}' has invalid native layout size {size} and alignment {align}",
            render_extern_type_key(ty, raw_scope)
        ),
        ExternDescriptorError::VoidType { context } => {
            format!("void type is not allowed in {context}")
        }
        ExternDescriptorError::InvalidAbiType { position, reason } => format!(
            "invalid native ABI type in {}: {}",
            render_abi_position(*position),
            render_abi_type_error(*reason),
        ),
        ExternDescriptorError::CallbackEscapeMismatch {
            param,
            param_escape,
            policy_escape,
        } => {
            let param = param.as_ref().map_or_else(
                || "callback parameter".to_string(),
                |param| format!("callback parameter '{param}'"),
            );
            format!(
                "{param} has mismatched escape metadata: parameter is {}, callback policy is {}",
                render_callback_escape(*param_escape),
                render_callback_escape(*policy_escape),
            )
        }
    }
}

fn render_extern_type_key(key: &ExternTypeKey, raw_scope: Option<&RawExternScope>) -> String {
    match raw_scope {
        Some(scope) => render_raw_scoped_name(scope, &key.name),
        None => format!("{}.{}", key.module, key.name),
    }
}

#[cfg(test)]
mod tests {
    use anvyx_externs::{
        BinaryOp, ExternMemberSelector, ExternOperator, ExternTypeExpr,
        ModulePath as ExternModulePath, OperatorReturn, ProviderId, TypeContext,
    };
    use chumsky::error::{LabelError, RichPattern};

    use super::*;
    use crate::{
        ast::Type,
        externs::RawExternSite,
        lexer::{Keyword, Token, TokenStream},
        parser,
        resolve::SourceFileId,
        span::SourceSpan,
        test_support::{
            core_option_key, core_option_type, ident, module_path_segments, nominal_test_source_id,
            test_nominal_type, test_source_id,
        },
    };

    fn module_id(path: &[&str]) -> ModuleId {
        ModuleId::named(PackageId::synthetic_root(), module_path_segments(path))
    }

    fn root_module_id() -> ModuleId {
        ModuleId::root(PackageId::synthetic_root())
    }

    fn raw_root_scope() -> RawExternScope {
        RawExternScope::Module(root_module_id())
    }

    fn module_scope(path: &[&str]) -> ModuleScope {
        ModuleScope::Named(module_path_segments(path))
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

    fn token_span() -> SourceSpan {
        SourceSpan::new(test_source_id(), 0, 1)
    }

    fn lex_source() -> SourceId {
        test_source_id()
    }

    fn resolve_span() -> SourceSpan {
        SourceSpan::new(test_source_id(), 0, 1)
    }

    fn token_stream(tokens: Vec<(Token, SourceSpan)>) -> TokenStream {
        let source = tokens
            .first()
            .map_or_else(test_source_id, |(_, span)| span.source());
        let len = tokens.iter().map(|(_, span)| span.end()).max().unwrap_or(0);
        TokenStream {
            source,
            eoi: SourceSpan::empty(source, len),
            tokens,
        }
    }

    fn core_option(inner: Type) -> Type {
        core_option_type(inner)
    }

    fn local_option(inner: Type) -> Type {
        test_nominal_type(
            nominal_test_source_id(),
            30,
            ast::NominalKind::Enum,
            ident("Option"),
            vec![inner],
            vec![],
            None,
        )
    }

    fn module_option(module: &[&str], inner: Type) -> Type {
        test_nominal_type(
            nominal_test_source_id(),
            31,
            ast::NominalKind::Enum,
            ident("Option"),
            vec![inner],
            vec![],
            Some(ModuleOrigin::Module(
                module_path_segments(module).to_ast_path(),
            )),
        )
    }

    fn diagnose_test_type_error(error: &TypeError) -> Diagnostic {
        let type_ctx = TypeDiagnosticContext::from_core_option(Some(core_option_key()));
        diagnose_type_error(error, &type_ctx)
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
            path: module_path_segments(&["missing"]),
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
    fn option_rendering_uses_core_identity() {
        let cases = [
            (
                core_option(Type::Int),
                Type::Bool,
                "mismatched types: expected `int?`, found `bool`",
            ),
            (
                local_option(Type::Int),
                Type::Bool,
                "mismatched types: expected `Option<int>`, found `bool`",
            ),
            (
                core_option(Type::Int),
                local_option(Type::Int),
                "mismatched types: expected `int?`, found `Option<int>`",
            ),
            (
                local_option(Type::Int),
                module_option(&["game"], Type::Int),
                "mismatched types: expected `Option<int>`, found `game.Option<int>`",
            ),
        ];

        for (expected, found, message) in cases {
            let diagnostic = diagnose_test_type_error(&TypeError::TypeMismatch {
                expected,
                found,
                span: None,
            });
            assert_msg(&diagnostic, message);
        }
    }

    #[test]
    fn dyn_contract_rendering_uses_context() {
        let diagnostic = diagnose_test_type_error(&TypeError::TypeMismatch {
            expected: Type::Dyn(ast::ContractRef::Anonymous(ast::AnonymousContract {
                requirements: vec![ast::AnonymousContractRequirement {
                    receiver: ast::MethodReceiver::Value,
                    name: ident("get"),
                    params: vec![],
                    ret: ReturnSpec::value(core_option(Type::Int)),
                }],
            })),
            found: Type::Bool,
            span: None,
        });
        assert_msg(
            &diagnostic,
            "mismatched types: expected `dyn { fn get(self) -> int?; }`, found `bool`",
        );
    }

    #[test]
    fn map_key_optional_message_uses_core_identity() {
        let cases = [
            (
                core_option(Type::Int),
                "optional type 'int?' is not keyable and cannot be used as map key",
            ),
            (
                local_option(Type::Int),
                "type 'Option<int>' is not keyable and cannot be used as map key",
            ),
        ];

        for (ty, message) in cases {
            let diagnostic = diagnose_test_type_error(&TypeError::NonKeyableMapKey {
                ty,
                field: None,
                span: None,
            });
            assert_msg(&diagnostic, message);
        }
    }

    #[test]
    fn decl_type_errors_use_context_rendering() {
        let diagnostic = diagnose_test_type_error(&TypeError::Decl(DeclError::PointlessCastFrom {
            ty: core_option(Type::Int),
            span: None,
        }));
        assert_msg(&diagnostic, "pointless cast from 'int?' to itself");
    }

    #[test]
    fn omits_labels_without_source_span() {
        let diagnostic = diagnose_test_type_error(&TypeError::CannotInferType { span: None });
        assert!(diagnostic.labels().is_empty());

        let diagnostic = diagnose_test_type_error(&TypeError::TypeMismatch {
            expected: Type::Int,
            found: Type::Bool,
            span: None,
        });
        assert_msg(
            &diagnostic,
            "mismatched types: expected `int`, found `bool`",
        );
        assert!(diagnostic.labels().is_empty());

        let diagnostic = diagnose_type_error(
            &TypeError::Decl(DeclError::DuplicateType {
                module: ModuleScope::Root,
                name: ident("Point"),
                span: None,
            }),
            &TypeDiagnosticContext::default(),
        );
        assert!(diagnostic.labels().is_empty());

        let diagnostic =
            diagnose_test_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
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
            diagnose_test_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
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
            diagnose_test_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
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
            diagnose_test_type_error(&TypeError::ExternCatalog(ExternCatalogError::UnknownType {
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
        let catalog = diagnose_test_type_error(&TypeError::ExternCatalog(
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

        let source = test_source_id();
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
