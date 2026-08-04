use std::{fmt, str::FromStr};

use crate::{
    config::LintConfig,
    diagnostic::{Diagnostic, DiagnosticTag},
    span::SourceSpan,
};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum LintLevel {
    Allow,
    #[default]
    Warn,
    Error,
}

impl LintLevel {
    #[must_use]
    pub fn config_name(self) -> &'static str {
        match self {
            Self::Allow => "allow",
            Self::Warn => "warn",
            Self::Error => "error",
        }
    }

    #[must_use]
    pub fn diagnostic_name(self) -> &'static str {
        match self {
            Self::Allow => "allow",
            Self::Warn => "warning",
            Self::Error => "error",
        }
    }
}

impl fmt::Display for LintLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.config_name())
    }
}

impl FromStr for LintLevel {
    type Err = UnknownLintLevel;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text {
            text if text == Self::Allow.config_name() => Ok(Self::Allow),
            text if text == Self::Warn.config_name() => Ok(Self::Warn),
            text if text == Self::Error.config_name() => Ok(Self::Error),
            _ => Err(UnknownLintLevel),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnknownLintLevel;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LintId {
    InternalAccess,
    Deprecated,
    PublicInferredDynContract,
    UnusedImport,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LintInfo {
    id: LintId,
    name: &'static str,
    default: LintLevel,
    group: &'static str,
    description: &'static str,
}

const IMPLEMENTED: &[LintInfo] = &[
    LintInfo {
        id: LintId::InternalAccess,
        name: "internal_access",
        default: LintLevel::Warn,
        group: "api",
        description: "outside-package access to @internal members",
    },
    LintInfo {
        id: LintId::Deprecated,
        name: "deprecated",
        default: LintLevel::Warn,
        group: "api",
        description: "use of declarations marked @deprecated",
    },
    LintInfo {
        id: LintId::PublicInferredDynContract,
        name: "public_inferred_dyn_contract",
        default: LintLevel::Warn,
        group: "api",
        description: "exported function uses inferred dyn contract",
    },
    LintInfo {
        id: LintId::UnusedImport,
        name: "unused_import",
        default: LintLevel::Warn,
        group: "unused",
        description: "unused private import has no binding or activation use",
    },
];

impl LintId {
    #[must_use]
    pub fn info(self) -> &'static LintInfo {
        IMPLEMENTED
            .iter()
            .find(|info| info.id == self)
            .expect("implemented lint id missing registry entry")
    }

    #[must_use]
    pub fn name(self) -> &'static str {
        self.info().name
    }

    #[must_use]
    pub fn default_level(self) -> LintLevel {
        self.info().default
    }

    #[must_use]
    pub fn group(self) -> &'static str {
        self.info().group
    }

    #[must_use]
    pub fn description(self) -> &'static str {
        self.info().description
    }
}

impl LintInfo {
    #[must_use]
    pub fn id(self) -> LintId {
        self.id
    }

    #[must_use]
    pub fn name(self) -> &'static str {
        self.name
    }

    #[must_use]
    pub fn default_level(self) -> LintLevel {
        self.default
    }

    #[must_use]
    pub fn group(self) -> &'static str {
        self.group
    }

    #[must_use]
    pub fn description(self) -> &'static str {
        self.description
    }
}

#[must_use]
pub fn implemented_lints() -> &'static [LintInfo] {
    IMPLEMENTED
}

#[must_use]
pub fn find_lint(name: &str) -> Option<LintId> {
    implemented_lints()
        .iter()
        .find(|info| info.name == name)
        .map(|info| info.id)
}

#[must_use]
pub fn expand_group(group: &str) -> Option<Vec<LintId>> {
    if group == "all" {
        return Some(implemented_lints().iter().map(|info| info.id).collect());
    }

    let ids = implemented_lints()
        .iter()
        .filter(|info| info.group == group)
        .map(|info| info.id)
        .collect::<Vec<_>>();
    (!ids.is_empty()).then_some(ids)
}

#[must_use]
pub fn available_lint_names() -> Vec<&'static str> {
    implemented_lints().iter().map(|info| info.name).collect()
}

#[must_use]
pub fn available_group_names() -> Vec<&'static str> {
    let mut groups = vec!["all"];
    for info in implemented_lints() {
        if !groups.contains(&info.group) {
            groups.push(info.group);
        }
    }
    groups
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LintParseError {
    MissingEquals,
    EmptyName,
    EmptyLevel,
    UnknownName {
        name: String,
        available: Vec<&'static str>,
    },
    UnknownLevel {
        level: String,
    },
}

impl fmt::Display for LintParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingEquals => f.write_str("lint override must use name=level"),
            Self::EmptyName => f.write_str("lint override name must not be empty"),
            Self::EmptyLevel => f.write_str("lint override level must not be empty"),
            Self::UnknownName { name, available } => write!(
                f,
                "unknown lint or group '{name}'. Available: {}",
                available.join(", ")
            ),
            Self::UnknownLevel { level } => write!(
                f,
                "unknown lint level '{level}'. Available: {}, {}, {}",
                LintLevel::Allow.config_name(),
                LintLevel::Warn.config_name(),
                LintLevel::Error.config_name()
            ),
        }
    }
}

impl std::error::Error for LintParseError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LintEvent {
    pub id: LintId,
    pub span: SourceSpan,
    pub message: String,
    pub label: String,
    pub notes: Vec<String>,
    pub help: Option<String>,
    pub tags: Vec<DiagnosticTag>,
}

pub fn apply_lints(
    config: &LintConfig,
    events: impl IntoIterator<Item = LintEvent>,
) -> Vec<Diagnostic> {
    events
        .into_iter()
        .filter_map(|event| lint_diagnostic(config, event))
        .collect()
}

fn lint_diagnostic(config: &LintConfig, event: LintEvent) -> Option<Diagnostic> {
    let level = config.level_info(event.id);
    let mut diagnostic = match level.level {
        LintLevel::Allow => return None,
        LintLevel::Warn => Diagnostic::warning(event.message),
        LintLevel::Error => Diagnostic::error(event.message),
    }
    .with_lint_code("anvyx", event.id.name(), level.level, level.origin);
    diagnostic = diagnostic.with_primary_message(event.span, event.label);
    for tag in event.tags {
        diagnostic = diagnostic.with_tag(tag);
    }
    for note in event.notes {
        diagnostic = diagnostic.with_note(note);
    }
    if let Some(help) = event.help {
        diagnostic = diagnostic.with_help(help);
    }
    Some(diagnostic)
}

#[must_use]
pub fn available_override_names() -> Vec<&'static str> {
    let mut names = available_lint_names();
    for group in available_group_names() {
        if !names.contains(&group) {
            names.push(group);
        }
    }
    names
}
