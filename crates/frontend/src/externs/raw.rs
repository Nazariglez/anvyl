use anvyx_externs::{
    ExternDescriptorError, ExternFieldDescriptor, ExternFunctionDescriptor, ExternInitDescriptor,
    ExternMemberSelector, ExternMethodDescriptor, ExternOperatorDescriptor, ExternRep,
    ExternStaticDescriptor, ModulePath, ProviderDescriptor, ProviderId,
};

use crate::{resolve::ModuleId, span::SourceSpan};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExternInputs {
    pub packages: Vec<PackageExternInputs>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageExternInputs {
    pub package: crate::resolve::PackageId,
    pub providers: Vec<ProviderDescriptor>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct RawExterns {
    pub groups: Vec<RawExternGroup>,
}

impl RawExterns {
    pub(crate) fn append(&mut self, other: Self) {
        self.groups.extend(other.groups);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternGroup {
    pub provenance: ExternProvenance,
    pub modules: Vec<RawExternModule>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExternProvenance {
    Provider {
        package: crate::resolve::PackageId,
        provider: ProviderId,
    },
    Source {
        module: RawExternScope,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum RawExternScope {
    Module(ModuleId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RawExternFunctionKey {
    pub module: RawExternScope,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RawExternTypeKey {
    pub module: RawExternScope,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct RawExternMemberKey {
    pub owner: RawExternTypeKey,
    pub selector: ExternMemberSelector,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct RawExternSite {
    pub span: Option<SourceSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternModule {
    pub scope: RawExternScope,
    pub types: Vec<RawExternType>,
    pub functions: Vec<RawExternFunction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternType {
    pub name: String,
    pub doc: Option<String>,
    pub exported: bool,
    pub rep: ExternRep,
    pub layout: Option<anvyx_externs::ExternLayout>,
    pub materialization: Option<anvyx_externs::ExternMaterialization>,
    pub owns_heap_edges: Option<bool>,
    pub fields: Vec<RawExternField>,
    pub variants: Vec<anvyx_externs::ExternEnumVariantDescriptor>,
    pub init: Option<RawExternInit>,
    pub methods: Vec<RawExternMethod>,
    pub statics: Vec<RawExternStatic>,
    pub operators: Vec<RawExternOperator>,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternFunction {
    pub decl: ExternFunctionDescriptor,
    pub exported: bool,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternField {
    pub decl: ExternFieldDescriptor,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternInit {
    pub decl: ExternInitDescriptor,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternMethod {
    pub decl: ExternMethodDescriptor,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternStatic {
    pub decl: ExternStaticDescriptor,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RawExternOperator {
    pub decl: ExternOperatorDescriptor,
    pub site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExternInputError {
    InvalidProviderDescriptor {
        package: crate::resolve::PackageId,
        provider: ProviderId,
        error: ExternDescriptorError,
    },
    DuplicateProviderModule {
        package: crate::resolve::PackageId,
        module: ModulePath,
        first: ProviderId,
        duplicate: ProviderId,
    },
    InvalidRawDescriptor {
        decl: RawExternDecl,
        scope: RawExternScope,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RawExternIdentityKey {
    Function(RawExternFunctionKey),
    Type(RawExternTypeKey),
    Member(RawExternMemberKey),
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
    pub provenance: ExternProvenance,
    pub site: RawExternSite,
}
