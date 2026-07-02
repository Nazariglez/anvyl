use std::{collections::HashSet, fmt};

use super::{
    MemberAccessKind, ModuleScope, NominalKey, NominalKind, TypeChecker, ValueDecl, VarInfo,
    decls::DeclError, field_check, nominal_type, substitute_aggregate_member,
};
use crate::{
    ast::{AnnotationArgs, AnnotationNode, Ident, Lit, Type},
    diagnostic::DiagnosticTag,
    lint::{LintEvent, LintId},
    source::SourceId,
    span::{SourceSpan, Span},
};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct AccessPolicy {
    internal: Option<AnnotationPolicy>,
    deprecated: Option<AnnotationPolicy>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct AnnotationPolicy {
    reason: Option<String>,
}

impl AccessPolicy {
    pub(crate) fn internal_reason(&self) -> Option<&str> {
        self.internal
            .as_ref()
            .and_then(|policy| policy.reason.as_deref())
    }

    pub(crate) fn deprecated_reason(&self) -> Option<&str> {
        self.deprecated
            .as_ref()
            .and_then(|policy| policy.reason.as_deref())
    }

    pub(crate) fn has_internal(&self) -> bool {
        self.internal.is_some()
    }

    pub(crate) fn has_deprecated(&self) -> bool {
        self.deprecated.is_some()
    }

    fn set_internal(&mut self, reason: Option<String>) {
        self.internal = Some(AnnotationPolicy { reason });
    }

    fn set_deprecated(&mut self, reason: Option<String>) {
        self.deprecated = Some(AnnotationPolicy { reason });
    }
}

impl From<MemberAccessKind> for DeprecatedUseKind {
    fn from(kind: MemberAccessKind) -> Self {
        match kind {
            MemberAccessKind::Field => Self::Field,
            MemberAccessKind::Method => Self::Method,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeprecatedUseKind {
    Function,
    ExternFunction,
    Const,
    Global,
    ExternType,
    TypeAlias,
    Contract,
    Struct,
    DataRef,
    Enum,
    EnumVariant,
    Field,
    Method,
}

impl DeprecatedUseKind {
    pub(crate) fn diagnostic_name(self) -> &'static str {
        match self {
            Self::Function => "function",
            Self::ExternFunction => "extern function",
            Self::Const => "const",
            Self::Global => "runtime global",
            Self::ExternType => "extern type",
            Self::TypeAlias => "type alias",
            Self::Contract => "contract",
            Self::Struct => "struct",
            Self::DataRef => "dataref",
            Self::Enum => "enum",
            Self::EnumVariant => "variant",
            Self::Field => "field",
            Self::Method => "method",
        }
    }
}

impl TypeChecker {
    pub(super) fn check_matched_field_access_policy(
        &mut self,
        owner: &field_check::FieldOwner,
        name: Ident,
        policy: &AccessPolicy,
        span: Span,
    ) {
        match owner {
            field_check::FieldOwner::Nominal(owner_ty) => {
                if let Some(key) = self.decls.key_for_type(owner_ty) {
                    self.check_access_policy(
                        policy,
                        MemberAccessKind::Field,
                        name,
                        owner_ty,
                        &key.module,
                        span,
                    );
                }
            }
            field_check::FieldOwner::Variant { key, .. } => {
                self.check_access_policy(
                    policy,
                    MemberAccessKind::Field,
                    name,
                    &nominal_type(key),
                    &key.module,
                    span,
                );
            }
        }
    }

    pub(super) fn warn_named_value_deprecated(
        &mut self,
        value: &ValueDecl,
        name: Ident,
        span: Span,
    ) {
        if let Some(kind) = value.deprecated_kind() {
            self.warn_deprecated(value.policy(), kind, name, span);
        }
    }

    pub(super) fn warn_named_const_deprecated(&mut self, name: Ident, span: Span) {
        let Some((_, _, ValueDecl::Const(sig))) = self.lookup_named_value(name) else {
            return;
        };
        self.warn_deprecated(&sig.policy, DeprecatedUseKind::Const, name, span);
    }

    pub(super) fn warn_local_const_deprecated(&mut self, info: &VarInfo, name: Ident, span: Span) {
        if info.local_const.is_some() || !info.kind.is_const() {
            return;
        }
        let Some(value) = self.decls.local_value(&self.current_module, name) else {
            return;
        };
        if matches!(value.decl, ValueDecl::Const(_)) {
            self.warn_named_value_deprecated(&value.decl, name, span);
        }
    }

    pub(super) fn warn_extern_type_deprecated(&mut self, key: &NominalKey, span: Span) {
        if key.kind != NominalKind::Extern {
            return;
        }
        let event = match self.decls.extern_type_policy(key) {
            Some(policy) if policy.has_deprecated() => deprecated_lint(
                DeprecatedUseKind::ExternType,
                key.name,
                policy.deprecated_reason(),
                self.source_span(span),
            ),
            _ => return,
        };
        self.push_lint_event(event);
    }

    pub(super) fn warn_deprecated(
        &mut self,
        policy: &AccessPolicy,
        kind: DeprecatedUseKind,
        name: Ident,
        span: Span,
    ) {
        if let Some(event) = deprecated_access_lint(policy, kind, name, self.source_span(span)) {
            self.push_lint_event(event);
        }
    }

    pub(super) fn check_access_policy(
        &mut self,
        policy: &AccessPolicy,
        kind: MemberAccessKind,
        name: Ident,
        owner: &Type,
        origin: &ModuleScope,
        span: Span,
    ) {
        emit_access_policy(
            policy,
            kind,
            name,
            owner,
            origin,
            span,
            &mut AccessPolicyOutput {
                source: self.source_id(),
                current_module: &self.current_module,
                lint_events: &mut self.lint_events,
            },
        );
    }

    pub(super) fn check_stored_field_path_access(
        &mut self,
        owner: &Type,
        path: &[Ident],
        span: Span,
    ) {
        let mut owner = owner.clone();
        for name in path {
            let Some(key) = self.decls.key_for_type(&owner) else {
                return;
            };
            let Some(aggregate) = self.decls.aggregate(&key) else {
                return;
            };
            let Some(field) = aggregate.fields.get(*name) else {
                return;
            };
            let policy = field.policy.clone();
            let field_ty = substitute_aggregate_member(&owner, &aggregate.generics, &field.ty);
            let origin = key.module;
            self.check_access_policy(
                &policy,
                MemberAccessKind::Field,
                *name,
                &owner,
                &origin,
                span,
            );
            owner = field_ty;
        }
    }
}

fn deprecated_access_lint(
    policy: &AccessPolicy,
    kind: DeprecatedUseKind,
    name: Ident,
    span: SourceSpan,
) -> Option<LintEvent> {
    policy
        .has_deprecated()
        .then(|| deprecated_lint(kind, name, policy.deprecated_reason(), span))
}

pub(super) fn deprecated_lint(
    kind: DeprecatedUseKind,
    name: Ident,
    reason: Option<&str>,
    span: SourceSpan,
) -> LintEvent {
    LintEvent {
        id: LintId::Deprecated,
        span,
        message: render_deprecated_access(kind, name, reason),
        label: format!("deprecated {} used here", kind.diagnostic_name()),
        notes: vec![],
        help: None,
        tags: vec![DiagnosticTag::Deprecated],
    }
}

pub(super) fn render_deprecated_access(
    kind: DeprecatedUseKind,
    name: Ident,
    reason: Option<&str>,
) -> String {
    let kind = kind.diagnostic_name();
    match reason {
        Some(reason) => format!("use of deprecated {kind} '{name}': {reason}"),
        None => format!("use of deprecated {kind} '{name}'"),
    }
}

pub(super) struct AccessPolicyOutput<'a> {
    pub(super) source: SourceId,
    pub(super) current_module: &'a ModuleScope,
    pub(super) lint_events: &'a mut Vec<LintEvent>,
}

fn render_internal_access(
    kind: MemberAccessKind,
    name: Ident,
    owner: &Type,
    reason: Option<&str>,
) -> String {
    let kind = kind.diagnostic_name();
    match reason {
        Some(reason) => format!("accessing internal {kind} '{name}' of type '{owner}': {reason}"),
        None => format!("accessing internal {kind} '{name}' of type '{owner}'"),
    }
}

pub(super) fn emit_access_policy(
    policy: &AccessPolicy,
    kind: MemberAccessKind,
    name: Ident,
    owner: &Type,
    origin: &ModuleScope,
    span: Span,
    out: &mut AccessPolicyOutput<'_>,
) {
    if let Some(event) = deprecated_access_lint(
        policy,
        DeprecatedUseKind::from(kind),
        name,
        SourceSpan::from_byte_span(out.source, span),
    ) {
        out.lint_events.push(event);
    }

    if !policy.has_internal() || origin.can_access_internal_from(out.current_module) {
        return;
    }

    let reason = policy.internal_reason().map(str::to_string);
    let span = SourceSpan::from_byte_span(out.source, span);
    out.lint_events.push(LintEvent {
        id: LintId::InternalAccess,
        span,
        message: render_internal_access(kind, name, owner, reason.as_deref()),
        label: format!("internal {} used here", kind.diagnostic_name()),
        notes: vec![],
        help: None,
        tags: vec![],
    });
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AnnotationTarget {
    Func,
    Struct,
    DataRef,
    Enum,
    Field,
    Variant,
    Const,
    Global,
    ExternFunc,
    ExternType,
    TypeAlias,
    Contract,
    InlineMethod,
    ExtendMethod,
}

impl fmt::Display for AnnotationTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let label = match self {
            Self::Func => "function",
            Self::Struct => "struct",
            Self::DataRef => "dataref",
            Self::Enum => "enum",
            Self::Field => "field",
            Self::Variant => "variant",
            Self::Const => "const",
            Self::Global => "runtime global",
            Self::ExternFunc => "extern function",
            Self::ExternType => "extern type",
            Self::TypeAlias => "type alias",
            Self::Contract => "contract",
            Self::InlineMethod => "inline method",
            Self::ExtendMethod => "extend method",
        };
        f.write_str(label)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AnnotationKind {
    Test,
    Deprecated,
    Internal,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArgShape {
    None,
    OptionalString,
}

struct AnnotationSpec {
    kind: AnnotationKind,
    targets: &'static [AnnotationTarget],
    args: ArgShape,
}

const TEST_TARGETS: &[AnnotationTarget] = &[AnnotationTarget::Func];
const DEPRECATED_TARGETS: &[AnnotationTarget] = &[
    AnnotationTarget::Func,
    AnnotationTarget::Struct,
    AnnotationTarget::DataRef,
    AnnotationTarget::Enum,
    AnnotationTarget::Field,
    AnnotationTarget::Variant,
    AnnotationTarget::Const,
    AnnotationTarget::Global,
    AnnotationTarget::ExternFunc,
    AnnotationTarget::ExternType,
    AnnotationTarget::TypeAlias,
    AnnotationTarget::Contract,
    AnnotationTarget::InlineMethod,
    AnnotationTarget::ExtendMethod,
];
const INTERNAL_TARGETS: &[AnnotationTarget] =
    &[AnnotationTarget::Field, AnnotationTarget::InlineMethod];

fn spec(name: Ident) -> Option<AnnotationSpec> {
    let spec = match name.as_str() {
        "test" => AnnotationSpec {
            kind: AnnotationKind::Test,
            targets: TEST_TARGETS,
            args: ArgShape::None,
        },
        "deprecated" => AnnotationSpec {
            kind: AnnotationKind::Deprecated,
            targets: DEPRECATED_TARGETS,
            args: ArgShape::OptionalString,
        },
        "internal" => AnnotationSpec {
            kind: AnnotationKind::Internal,
            targets: INTERNAL_TARGETS,
            args: ArgShape::OptionalString,
        },
        _ => return None,
    };
    Some(spec)
}

#[derive(Default)]
pub(crate) struct FieldAnnotations {
    pub(crate) policy: AccessPolicy,
    pub(crate) as_projection: bool,
}

pub(crate) fn normalize_annotations(
    source: SourceId,
    annotations: &[AnnotationNode],
    target: AnnotationTarget,
    errors: &mut Vec<DeclError>,
) -> AccessPolicy {
    let mut policy = AccessPolicy::default();
    let mut seen = HashSet::new();
    for annotation in annotations {
        apply_policy_annotation(source, annotation, target, &mut seen, &mut policy, errors);
    }
    policy
}

pub(crate) fn normalize_field_annotations(
    source: SourceId,
    annotations: &[AnnotationNode],
    embedded: bool,
    errors: &mut Vec<DeclError>,
) -> FieldAnnotations {
    let mut policy = AccessPolicy::default();
    let mut as_projection = false;
    let mut seen = HashSet::new();

    for annotation in annotations {
        let name = annotation.node.name;
        if name.as_str() != "as" {
            apply_policy_annotation(
                source,
                annotation,
                AnnotationTarget::Field,
                &mut seen,
                &mut policy,
                errors,
            );
            continue;
        }

        let span = Some(SourceSpan::from_byte_span(source, annotation.span));
        if !seen.insert(name) {
            errors.push(DeclError::DuplicateAnnotation { name, span });
        } else if !embedded {
            errors.push(DeclError::AsProjectionWithoutEmbed { span });
        } else if !matches!(annotation.node.args, AnnotationArgs::None) {
            errors.push(DeclError::AsProjectionWithArgs { span });
        } else {
            as_projection = true;
        }
    }

    FieldAnnotations {
        policy,
        as_projection,
    }
}

fn apply_policy_annotation(
    source: SourceId,
    annotation: &AnnotationNode,
    target: AnnotationTarget,
    seen: &mut HashSet<Ident>,
    policy: &mut AccessPolicy,
    errors: &mut Vec<DeclError>,
) {
    let name = annotation.node.name;
    let span = Some(SourceSpan::from_byte_span(source, annotation.span));
    let Some(spec) = spec(name) else {
        errors.push(DeclError::UnknownAnnotation { name, span });
        return;
    };
    if !spec.targets.contains(&target) {
        errors.push(DeclError::InvalidAnnotationTarget {
            name,
            target: target.to_string(),
            valid_targets: valid_targets(spec.targets),
            span,
        });
        return;
    }
    if !seen.insert(name) {
        errors.push(DeclError::DuplicateAnnotation { name, span });
        return;
    }
    let Some(reason) = annotation_reason(
        &annotation.node.args,
        name,
        spec.args,
        source,
        annotation.span,
        errors,
    ) else {
        return;
    };
    match spec.kind {
        AnnotationKind::Test => {}
        AnnotationKind::Deprecated => policy.set_deprecated(reason.reason),
        AnnotationKind::Internal => policy.set_internal(reason.reason),
    }
}

struct AnnotationReason {
    reason: Option<String>,
}

fn annotation_reason(
    args: &AnnotationArgs,
    name: Ident,
    shape: ArgShape,
    source: SourceId,
    span: Span,
    errors: &mut Vec<DeclError>,
) -> Option<AnnotationReason> {
    match (shape, args) {
        (_, AnnotationArgs::None) => Some(AnnotationReason { reason: None }),
        (ArgShape::None, _) => {
            errors.push(DeclError::InvalidAnnotationArgs {
                name,
                message: "this annotation does not accept arguments".to_string(),
                span: Some(SourceSpan::from_byte_span(source, span)),
            });
            None
        }
        (ArgShape::OptionalString, AnnotationArgs::Positional(Lit::String(reason))) => {
            Some(AnnotationReason {
                reason: Some(reason.clone()),
            })
        }
        (ArgShape::OptionalString, _) => {
            errors.push(DeclError::InvalidAnnotationArgs {
                name,
                message: "expected no arguments or a string argument".to_string(),
                span: Some(SourceSpan::from_byte_span(source, span)),
            });
            None
        }
    }
}

fn valid_targets(targets: &[AnnotationTarget]) -> String {
    targets
        .iter()
        .map(|target| format!("{target}s"))
        .collect::<Vec<_>>()
        .join(", ")
}
