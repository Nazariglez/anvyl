use anvyx_externs::{ExternDescriptorError, ExternFunctionDescriptor, ExternMemberSelector};

use crate::{resolve::ModuleId, span::SourceSpan};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct RawExterns {
    pub modules: Vec<RawExternModule>,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct RawExternSite {
    pub span: Option<SourceSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternModule {
    pub scope: ModuleId,
    pub types: Vec<RawExternType>,
    pub functions: Vec<RawExternFunction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternType {
    pub decl: anvyx_externs::ExternTypeDescriptor,
    pub exported: bool,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternFunction {
    pub decl: ExternFunctionDescriptor,
    pub exported: bool,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExternInputError {
    InvalidRawDescriptor {
        decl: RawExternDecl,
        error: ExternDescriptorError,
    },
    DuplicateRawIdentity {
        key: RawExternIdentityKey,
        first: RawExternDecl,
        duplicate: RawExternDecl,
    },
    UnsupportedSource {
        span: SourceSpan,
        kind: UnsupportedSourceKind,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum RawExternIdentityKey {
    Function(String),
    Type(String),
    Member {
        owner: String,
        selector: ExternMemberSelector,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum UnsupportedSourceKind {
    Type(String),
    InferReturn,
    ReturnPlace,
    CallbackReturn,
    Operator(String),
    Param {
        name: String,
        reason: UnsupportedSourceParamReason,
    },
    CallbackParam {
        reason: UnsupportedSourceParamReason,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnsupportedSourceParamReason {
    Mutable,
    CastAccept,
    Default,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternDecl {
    pub scope: ModuleId,
    pub site: RawExternSite,
}
