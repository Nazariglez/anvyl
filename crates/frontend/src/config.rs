use std::collections::{BTreeMap, HashSet};

pub use crate::lint::LintLevel;
use crate::lint::{LintId, LintParseError, available_override_names, expand_group, find_lint};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LintConfig {
    overrides: BTreeMap<LintId, LintLevel>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LintLevelInfo {
    pub level: LintLevel,
    pub origin: LintLevelOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintLevelOrigin {
    Default,
    Configured,
}

impl LintConfig {
    #[must_use]
    pub fn level(&self, id: LintId) -> LintLevel {
        self.level_info(id).level
    }

    #[must_use]
    pub fn level_info(&self, id: LintId) -> LintLevelInfo {
        match self.overrides.get(&id).copied() {
            Some(level) => LintLevelInfo {
                level,
                origin: LintLevelOrigin::Configured,
            },
            None => LintLevelInfo {
                level: id.default_level(),
                origin: LintLevelOrigin::Default,
            },
        }
    }

    pub fn set(&mut self, id: LintId, level: LintLevel) {
        if level == id.default_level() {
            self.overrides.remove(&id);
        } else {
            self.overrides.insert(id, level);
        }
    }

    pub fn apply_override(&mut self, text: &str) -> Result<(), LintParseError> {
        let Some((name, level)) = text.split_once('=') else {
            return Err(LintParseError::MissingEquals);
        };
        if name.is_empty() {
            return Err(LintParseError::EmptyName);
        }
        if level.is_empty() {
            return Err(LintParseError::EmptyLevel);
        }
        let level = level.parse().map_err(|_| LintParseError::UnknownLevel {
            level: level.into(),
        })?;
        if let Some(id) = find_lint(name) {
            self.set(id, level);
            return Ok(());
        }
        if let Some(ids) = expand_group(name) {
            for id in ids {
                self.set(id, level);
            }
            return Ok(());
        }
        Err(LintParseError::UnknownName {
            name: name.into(),
            available: available_override_names(),
        })
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum Profile {
    #[default]
    Debug,
    Release,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PredicateError {
    UnknownPredicate,
    UnknownValue,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TargetOs {
    Macos,
    Linux,
    Windows,
    Wasm,
    Ios,
    Android,
    #[default]
    Unknown,
}

impl TargetOs {
    pub fn host() -> Self {
        if cfg!(target_os = "macos") {
            Self::Macos
        } else if cfg!(target_os = "linux") {
            Self::Linux
        } else if cfg!(target_os = "windows") {
            Self::Windows
        } else if cfg!(target_os = "ios") {
            Self::Ios
        } else if cfg!(target_os = "android") {
            Self::Android
        } else if cfg!(target_family = "wasm") {
            Self::Wasm
        } else {
            Self::Unknown
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        Some(match value {
            "macos" => Self::Macos,
            "linux" => Self::Linux,
            "windows" => Self::Windows,
            "wasm" => Self::Wasm,
            "ios" => Self::Ios,
            "android" => Self::Android,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum TargetArch {
    X86_64,
    Aarch64,
    #[default]
    Unknown,
}

impl TargetArch {
    pub fn host() -> Self {
        if cfg!(target_arch = "x86_64") {
            Self::X86_64
        } else if cfg!(target_arch = "aarch64") {
            Self::Aarch64
        } else {
            Self::Unknown
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        Some(match value {
            "x86_64" => Self::X86_64,
            "aarch64" => Self::Aarch64,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationContext {
    pub profile: Profile,
    pub os: TargetOs,
    pub arch: TargetArch,
    pub features: HashSet<String>,
}

impl Default for CompilationContext {
    fn default() -> Self {
        Self {
            profile: Profile::Debug,
            os: TargetOs::host(),
            arch: TargetArch::host(),
            features: HashSet::new(),
        }
    }
}

impl CompilationContext {
    pub fn eval_predicate(&self, predicate: &str, value: &str) -> Result<bool, PredicateError> {
        match predicate {
            "profile" => match value {
                "debug" => Ok(self.profile == Profile::Debug),
                "release" => Ok(self.profile == Profile::Release),
                _ => Err(PredicateError::UnknownValue),
            },
            "os" => TargetOs::parse(value)
                .map(|os| self.os == os)
                .ok_or(PredicateError::UnknownValue),
            "arch" => TargetArch::parse(value)
                .map(|arch| self.arch == arch)
                .ok_or(PredicateError::UnknownValue),
            "feature" => Ok(self.features.contains(value)),
            _ => Err(PredicateError::UnknownPredicate),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lint_config_uses_registry_defaults() {
        let config = LintConfig::default();

        assert_eq!(config.level(LintId::InternalAccess), LintLevel::Warn);
        assert_eq!(config.level(LintId::Deprecated), LintLevel::Warn);
    }

    #[test]
    fn lint_config_overrides_levels() {
        let mut config = LintConfig::default();

        config.set(LintId::InternalAccess, LintLevel::Error);

        assert_eq!(config.level(LintId::InternalAccess), LintLevel::Error);
        assert_eq!(config.level(LintId::Deprecated), LintLevel::Warn);
        assert_eq!(
            config.level_info(LintId::InternalAccess),
            LintLevelInfo {
                level: LintLevel::Error,
                origin: LintLevelOrigin::Configured,
            }
        );
        assert_eq!(
            config.level_info(LintId::Deprecated),
            LintLevelInfo {
                level: LintLevel::Warn,
                origin: LintLevelOrigin::Default,
            }
        );
    }

    #[test]
    fn lint_config_does_not_store_default_overrides() {
        let mut config = LintConfig::default();

        config.set(LintId::InternalAccess, LintLevel::Error);
        config.set(LintId::InternalAccess, LintLevel::Warn);

        assert_eq!(config, LintConfig::default());
    }

    #[test]
    fn apply_override_accepts_names_groups_and_repetition() {
        let mut config = LintConfig::default();

        config.apply_override("api=error").unwrap();
        config.apply_override("deprecated=allow").unwrap();

        assert_eq!(config.level(LintId::InternalAccess), LintLevel::Error);
        assert_eq!(config.level(LintId::Deprecated), LintLevel::Allow);
        assert_eq!(
            config.level(LintId::PublicInferredDynContract),
            LintLevel::Error
        );
    }

    #[test]
    fn apply_override_accepts_all_group() {
        let mut config = LintConfig::default();

        config.apply_override("all=allow").unwrap();

        assert_eq!(config.level(LintId::InternalAccess), LintLevel::Allow);
        assert_eq!(config.level(LintId::Deprecated), LintLevel::Allow);
        assert_eq!(
            config.level(LintId::PublicInferredDynContract),
            LintLevel::Allow
        );
    }

    #[test]
    fn apply_override_rejects_invalid_input() {
        let cases = [
            ("deprecated", LintParseError::MissingEquals),
            ("=warn", LintParseError::EmptyName),
            ("deprecated=", LintParseError::EmptyLevel),
            (
                "deprecated=deny",
                LintParseError::UnknownLevel {
                    level: "deny".to_string(),
                },
            ),
            (
                "unused_variable=warn",
                LintParseError::UnknownName {
                    name: "unused_variable".to_string(),
                    available: available_override_names(),
                },
            ),
        ];

        for (text, expected) in cases {
            assert_eq!(LintConfig::default().apply_override(text), Err(expected));
        }
    }
}
