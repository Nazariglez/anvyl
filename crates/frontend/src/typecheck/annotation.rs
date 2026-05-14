use std::{collections::HashSet, fmt};

use super::decls::DeclError;
use crate::{
    ast::{AnnotationArgs, AnnotationNode, Ident, Lit},
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
