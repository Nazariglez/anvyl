mod check;
mod dependency;
mod error;
mod source;

pub use anvyx_frontend::{
    config::{CompilationContext, LintConfig, LintLevel, Profile, TargetArch, TargetOs},
    pipeline::{Diagnostic, DiagnosticSeverity, FrontendConfig},
    resolve::PackageId,
};
pub use check::{CheckFileInput, CheckPackageInput, check_file, check_package};
pub use dependency::{DependencyAliasError, validate_dependency_alias};
pub use error::{CheckError, CheckOk, CheckResult};
pub use source::{ModuleSource, PackageSource, SourceBundle, SourceText, SystemPackageSource};
