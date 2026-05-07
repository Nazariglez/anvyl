use std::{collections::HashSet, fmt};

use super::decls::DeclError;
use crate::{
    ast::{AnnotationArgs, AnnotationNode, Ident, Lit},
    span::Span,
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
    ExternFunc,
    ExternType,
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
            Self::ExternFunc => "extern function",
            Self::ExternType => "extern type",
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
    AnnotationTarget::ExternFunc,
    AnnotationTarget::ExternType,
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

pub(crate) fn normalize_annotations(
    annotations: &[AnnotationNode],
    target: AnnotationTarget,
    errors: &mut Vec<DeclError>,
) -> AccessPolicy {
    let mut policy = AccessPolicy::default();
    let mut seen = HashSet::new();

    for annotation in annotations {
        let name = annotation.node.name;
        let Some(spec) = spec(name) else {
            errors.push(DeclError::UnknownAnnotation {
                name,
                span: annotation.span,
            });
            continue;
        };

        if !spec.targets.contains(&target) {
            errors.push(DeclError::InvalidAnnotationTarget {
                name,
                target: target.to_string(),
                valid_targets: valid_targets(spec.targets),
                span: annotation.span,
            });
            continue;
        }

        if !seen.insert(name) {
            errors.push(DeclError::DuplicateAnnotation {
                name,
                span: annotation.span,
            });
            continue;
        }

        let Some(reason) = annotation_reason(
            &annotation.node.args,
            name,
            spec.args,
            annotation.span,
            errors,
        ) else {
            continue;
        };

        match spec.kind {
            AnnotationKind::Test => {}
            AnnotationKind::Deprecated => policy.set_deprecated(reason.reason),
            AnnotationKind::Internal => policy.set_internal(reason.reason),
        }
    }

    policy
}

struct AnnotationReason {
    reason: Option<String>,
}

fn annotation_reason(
    args: &AnnotationArgs,
    name: Ident,
    shape: ArgShape,
    span: Span,
    errors: &mut Vec<DeclError>,
) -> Option<AnnotationReason> {
    match (shape, args) {
        (_, AnnotationArgs::None) => Some(AnnotationReason { reason: None }),
        (ArgShape::None, _) => {
            errors.push(DeclError::InvalidAnnotationArgs {
                name,
                message: "this annotation does not accept arguments".to_string(),
                span,
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
                span,
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
