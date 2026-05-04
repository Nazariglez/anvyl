mod check;
mod dependency;
mod error;
mod source;

pub use anvyx_frontend::resolve::PackageId;
pub use check::{CheckFileInput, CheckPackageInput, check_file, check_package};
pub use dependency::{DependencyAliasError, validate_dependency_alias};
pub use error::{CheckError, CheckOk, CheckResult};
pub use source::{ModuleSource, PackageSource, SourceBundle, SourceText, SystemPackageSource};
