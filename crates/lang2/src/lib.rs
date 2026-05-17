mod check;
mod dependency;
mod error;
mod source;

pub use anvyx_frontend::{
    config::{CompilationContext, LintConfig, LintLevelInfo, Profile, TargetArch, TargetOs},
    diagnostic::render::{
        RenderConfig, render_rich_report, render_rich_report_with_config,
        render_rich_report_with_overrides,
    },
    lint::{
        LintId, LintInfo, LintLevel, LintParseError, available_group_names, available_lint_names,
        available_override_names, expand_group, find_lint, implemented_lints,
    },
    pipeline::{
        Diagnostic, DiagnosticCode, DiagnosticCodeKind, DiagnosticLabel, DiagnosticProjection,
        DiagnosticReport, DiagnosticSeverity, DiagnosticTag, FrontendConfig, LabelStyle,
        LintLevelOrigin,
    },
    resolve::PackageId,
    source::{LineCol, SourceFile, SourceId, SourceKind, SourceTable},
    span::SourceSpan,
};
pub use check::{CheckFileInput, CheckPackageInput, check_file, check_package};
pub use dependency::{DependencyAliasError, validate_dependency_alias};
pub use error::{CheckError, CheckOutput, CheckPhase, CheckResult, CheckStatus};
pub use source::{
    ModuleSource, PackageSource, SourceBundle, SourceOverride, SourceText, SystemPackageSource,
};
