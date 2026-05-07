use std::collections::HashSet;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum LintLevel {
    Allow,
    #[default]
    Warn,
    Error,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct LintConfig {
    pub internal_access: LintLevel,
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
