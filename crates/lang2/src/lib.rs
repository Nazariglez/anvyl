mod check;
mod dependency;
mod error;
mod source;

pub use anvyx_frontend::{
    config::{CompilationContext, LintConfig, Profile, TargetArch, TargetOs},
    diagnostic::render::{render_plain_diagnostic, render_plain_report, render_rich_report},
    lint::{
        LintId, LintInfo, LintLevel, LintParseError, available_group_names, available_lint_names,
        available_override_names, expand_group, find_lint, implemented_lints,
    },
    pipeline::{
        Diagnostic, DiagnosticCode, DiagnosticLabel, DiagnosticReport, DiagnosticSeverity,
        DiagnosticTag, FrontendConfig, LabelStyle,
    },
    resolve::PackageId,
    source::{SourceFile, SourceId, SourceKind, SourceTable},
    span::SourceSpan,
};
pub use check::{CheckFileInput, CheckPackageInput, check_file, check_package};
pub use dependency::{DependencyAliasError, validate_dependency_alias};
pub use error::{CheckError, CheckOk, CheckResult};
pub use source::{
    ModuleSource, PackageSource, SourceBundle, SourceOverride, SourceText, SystemPackageSource,
};
