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

impl fmt::Display for LintLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Allow => "allow",
            Self::Warn => "warn",
            Self::Error => "error",
        })
    }
}

impl FromStr for LintLevel {
    type Err = UnknownLintLevel;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text {
            "allow" => Ok(Self::Allow),
            "warn" => Ok(Self::Warn),
            "error" => Ok(Self::Error),
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
        description: "cross-module access to @internal members",
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
                "unknown lint level '{level}'. Available: allow, warn, error"
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::{SourceKind, SourceTable};

    fn span() -> SourceSpan {
        let mut sources = SourceTable::default();
        let source = sources.add(SourceKind::Virtual, "test", None, "test");
        SourceSpan::new(source, 0, 4)
    }

    fn event(id: LintId) -> LintEvent {
        LintEvent {
            id,
            span: span(),
            message: "lint message".to_string(),
            label: "lint label".to_string(),
            notes: vec!["lint note".to_string()],
            help: Some("lint help".to_string()),
            tags: vec![DiagnosticTag::Deprecated],
        }
    }

    #[test]
    fn parses_and_displays_levels() {
        assert_eq!("allow".parse::<LintLevel>().unwrap(), LintLevel::Allow);
        assert_eq!("warn".parse::<LintLevel>().unwrap(), LintLevel::Warn);
        assert_eq!("error".parse::<LintLevel>().unwrap(), LintLevel::Error);
        assert!("deny".parse::<LintLevel>().is_err());
        assert_eq!(LintLevel::Error.to_string(), "error");
    }

    #[test]
    fn registry_is_stable() {
        let names = implemented_lints()
            .iter()
            .map(|info| info.name())
            .collect::<Vec<_>>();

        assert_eq!(
            names,
            [
                "internal_access",
                "deprecated",
                "public_inferred_dyn_contract",
                "unused_import"
            ]
        );
    }

    #[test]
    fn ids_read_metadata_from_registry() {
        assert_eq!(LintId::InternalAccess.name(), "internal_access");
        assert_eq!(LintId::Deprecated.default_level(), LintLevel::Warn);
        assert_eq!(LintId::PublicInferredDynContract.group(), "api");
        assert_eq!(LintId::UnusedImport.group(), "unused");
        assert_eq!(
            LintId::PublicInferredDynContract.description(),
            "exported function uses inferred dyn contract"
        );
    }

    #[test]
    fn looks_up_lints_and_groups() {
        assert_eq!(find_lint("deprecated"), Some(LintId::Deprecated));
        assert_eq!(find_lint("unused_import"), Some(LintId::UnusedImport));
        assert_eq!(
            expand_group("api").unwrap(),
            vec![
                LintId::InternalAccess,
                LintId::Deprecated,
                LintId::PublicInferredDynContract,
            ]
        );
        assert_eq!(expand_group("unused").unwrap(), vec![LintId::UnusedImport]);
        assert_eq!(
            expand_group("all").unwrap(),
            vec![
                LintId::InternalAccess,
                LintId::Deprecated,
                LintId::PublicInferredDynContract,
                LintId::UnusedImport,
            ]
        );
    }

    #[test]
    fn available_names_are_deterministic() {
        assert_eq!(
            available_lint_names(),
            vec![
                "internal_access",
                "deprecated",
                "public_inferred_dyn_contract",
                "unused_import"
            ]
        );
        assert_eq!(available_group_names(), vec!["all", "api", "unused"]);
        assert_eq!(
            available_override_names(),
            vec![
                "internal_access",
                "deprecated",
                "public_inferred_dyn_contract",
                "unused_import",
                "all",
                "api",
                "unused",
            ]
        );
    }

    #[test]
    fn parse_errors_are_stable() {
        assert_eq!(
            LintParseError::MissingEquals.to_string(),
            "lint override must use name=level"
        );
        assert_eq!(
            LintParseError::UnknownLevel {
                level: "deny".to_string()
            }
            .to_string(),
            "unknown lint level 'deny'. Available: allow, warn, error"
        );
    }

    #[test]
    fn apply_lints_suppresses_allowed_events() {
        let mut config = LintConfig::default();
        config.set(LintId::Deprecated, LintLevel::Allow);

        let diagnostics = apply_lints(&config, [event(LintId::Deprecated)]);

        assert!(diagnostics.is_empty());
    }

    #[test]
    fn apply_lints_copies_metadata_and_details() {
        let diagnostics = apply_lints(&LintConfig::default(), [event(LintId::Deprecated)]);
        let diagnostic = &diagnostics[0];

        assert_eq!(diagnostic.severity(), crate::diagnostic::Severity::Warning);
        assert_eq!(diagnostic.code().unwrap().source, "anvyx");
        assert_eq!(diagnostic.code().unwrap().code, "deprecated");
        assert_eq!(
            diagnostic.notes_with_metadata(),
            vec![
                "lint note".to_string(),
                "lint `deprecated` is on by default".to_string()
            ]
        );
        assert_eq!(diagnostic.tags(), &[DiagnosticTag::Deprecated]);
        assert_eq!(diagnostic.message(), "lint message");
        assert_eq!(
            diagnostic.labels()[0].message.as_deref(),
            Some("lint label")
        );
        assert_eq!(diagnostic.notes(), &["lint note"]);
        assert_eq!(diagnostic.help(), Some("lint help"));
    }
}
