use std::{fmt, rc::Rc};

use serde::{Deserialize, Serialize};
use unicode_ident::{is_xid_continue, is_xid_start};

use crate::{
    CallbackEscape, CallbackThread, ExternBindingKey, ExternBindingOp, ExternBindingTarget,
    ExternMaterialization, ExternMemberSelector, ExternModuleDescriptor, ExternParam, ExternRep,
    ExternSignature, ExternTypeDescriptor, ExternTypeExpr, ExternTypeKey, ModulePath, ParamFlow,
    ProviderDescriptor, ReceiverMode, callback_escape_matches,
};

pub const INLINE_MATERIALIZER_SYMBOL: &str = "__anvyx_materialize";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustSupportError(Box<ProviderSupportDetail>);

#[derive(Debug, Clone, PartialEq, Eq)]
struct ProviderSupportDetail {
    provider: String,
    kind: SupportErrorKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SupportErrorKind {
    DuplicateType {
        key: ExternTypeKey,
    },
    DuplicateBinding {
        key: ExternBindingKey,
    },
    Type {
        key: ExternTypeKey,
        reason: TypeSupportError,
    },
    Binding {
        key: ExternBindingKey,
        reason: BindingSupportError,
    },
    TrackedReturn {
        key: ExternBindingKey,
        ty: ExternTypeKey,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeSupportError {
    Unknown,
    InconsistentHeapEdges,
    Path(PathReason),
    MaterializationMode,
    MaterializerType,
    MaterializerFunction,
    MissingMaterializer,
    ExtraMaterializer,
    SharedMaterialization,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BindingSupportError {
    Unknown,
    Path(PathReason),
    Abi(RustAbiError),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PathReason {
    role: PathRole,
    segment: String,
    error: PathError,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PathRole {
    Type,
    MaterializerType,
    MaterializerFunction,
    Binding,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PathError {
    Empty,
    Rooted,
    Reserved,
    Keyword,
    InvalidIdentifier,
    Generic,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RustAbiError {
    ParamCount { expected: usize, found: usize },
    Param { index: usize },
    MutPlaceResource,
    SharedResourceParam,
    Return,
    StructuralResourceReturn,
    CallbackReturn,
    WrapperContext,
    CallbackReceiver,
    CallbackReceiverResource,
    InitField,
    CallbackWrapperScopedReceiver,
    CallbackWrapperReceiverMissing,
    CallbackWrapperReceiverMutPlace,
    CallbackWrapperBorrowedParam,
}

impl RustSupportError {
    fn new(provider: &str, kind: SupportErrorKind) -> Self {
        Self(Box::new(ProviderSupportDetail {
            provider: provider.to_string(),
            kind,
        }))
    }

    fn ty(provider: &str, key: &ExternTypeKey, reason: TypeSupportError) -> Self {
        Self::new(
            provider,
            SupportErrorKind::Type {
                key: key.clone(),
                reason,
            },
        )
    }

    fn binding(provider: &str, key: &ExternBindingKey, reason: BindingSupportError) -> Self {
        Self::new(
            provider,
            SupportErrorKind::Binding {
                key: key.clone(),
                reason,
            },
        )
    }

    pub(crate) fn duplicate_type(provider: &str, key: &ExternTypeKey) -> Self {
        Self::new(
            provider,
            SupportErrorKind::DuplicateType { key: key.clone() },
        )
    }

    pub(crate) fn duplicate_binding(provider: &str, key: &ExternBindingKey) -> Self {
        Self::new(
            provider,
            SupportErrorKind::DuplicateBinding { key: key.clone() },
        )
    }
}

impl fmt::Display for RustSupportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let provider = &self.0.provider;
        match &self.0.kind {
            SupportErrorKind::DuplicateType { key } => write!(
                f,
                "native provider `{provider}` has duplicate type support for {}",
                type_name(key)
            ),
            SupportErrorKind::DuplicateBinding { key } => write!(
                f,
                "native provider `{provider}` has duplicate binding support for {}",
                binding_name(key)
            ),
            SupportErrorKind::Type { key, reason } => type_error(f, provider, key, reason),
            SupportErrorKind::Binding { key, reason } => binding_error(f, provider, key, reason),
            SupportErrorKind::TrackedReturn { key, ty } => write!(
                f,
                "native {} returns tracked resource `{}::{}` by owned value; return AnvRef instead",
                binding_name(key),
                ty.module.segments.join("::"),
                ty.name
            ),
        }
    }
}

fn type_error(
    f: &mut fmt::Formatter<'_>,
    provider: &str,
    key: &ExternTypeKey,
    reason: &TypeSupportError,
) -> fmt::Result {
    let message = match reason {
        TypeSupportError::Unknown => "has support for unknown type",
        TypeSupportError::InconsistentHeapEdges => "has inconsistent heap-edge metadata for type",
        TypeSupportError::MaterializationMode => "has mismatched materialization mode for type",
        TypeSupportError::MaterializerType => "has mismatched native path for type",
        TypeSupportError::MaterializerFunction => "has mismatched materializer symbol for type",
        TypeSupportError::MissingMaterializer => "is missing materializer support for type",
        TypeSupportError::ExtraMaterializer => "has extra materializer support for type",
        TypeSupportError::SharedMaterialization => "has invalid shared materialization for type",
        TypeSupportError::Path(path) => return path_error(f, provider, &type_name(key), path),
    };
    write!(
        f,
        "native provider `{provider}` {message} {}",
        type_name(key)
    )
}

fn binding_error(
    f: &mut fmt::Formatter<'_>,
    provider: &str,
    key: &ExternBindingKey,
    reason: &BindingSupportError,
) -> fmt::Result {
    match reason {
        BindingSupportError::Unknown => write!(
            f,
            "native provider `{provider}` has support for unknown {}",
            binding_name(key)
        ),
        BindingSupportError::Abi(reason) => write!(
            f,
            "native provider `{provider}` has mismatched ABI for {}: {reason}",
            binding_name(key)
        ),
        BindingSupportError::Path(path) => path_error(f, provider, &binding_name(key), path),
    }
}

fn path_error(
    f: &mut fmt::Formatter<'_>,
    provider: &str,
    subject: &str,
    path: &PathReason,
) -> fmt::Result {
    let role = match path.role {
        PathRole::Type => "type",
        PathRole::MaterializerType => "materializer type",
        PathRole::MaterializerFunction => "materializer function",
        PathRole::Binding => "binding",
    };
    write!(
        f,
        "native provider `{provider}` has invalid {role} Rust path for {subject}: segment `{}` is {}",
        path.segment, path.error
    )
}

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Empty => "empty",
            Self::Rooted => "rooted or contains a path separator",
            Self::Reserved => "a reserved path keyword",
            Self::Keyword => "an unraw Rust keyword",
            Self::InvalidIdentifier => "not a Rust identifier",
            Self::Generic => "an unsupported generic suffix",
        })
    }
}

impl fmt::Display for RustAbiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ParamCount { expected, found } => {
                write!(f, "parameter count mismatch: expected {expected}, found {found}")
            }
            Self::Param { index } => write!(f, "parameter {index} ABI mismatch"),
            Self::MutPlaceResource => f.write_str("mutable-place ABI is unsupported for shared resources"),
            Self::SharedResourceParam => f.write_str("shared resource parameters must use top-level, Option, or Result AnvRef"),
            Self::Return => f.write_str("return ABI mismatch"),
            Self::StructuralResourceReturn => f.write_str("structural owned resource return ABI is unsupported"),
            Self::CallbackReturn => f.write_str("native callback return ABI is unsupported"),
            Self::WrapperContext => f.write_str("wrapper ctx mode mismatch"),
            Self::CallbackReceiver => f.write_str("callback wrapper ABI method receiver must be borrowed"),
            Self::CallbackReceiverResource => f.write_str("callback wrapper ABI method receiver must be a shared resource"),
            Self::InitField => f.write_str("init field ABI mismatch"),
            Self::CallbackWrapperScopedReceiver => f.write_str("scoped callback wrapper ABI cannot be combined with method receivers"),
            Self::CallbackWrapperReceiverMissing => f.write_str("callback wrapper ABI method receiver is missing"),
            Self::CallbackWrapperReceiverMutPlace => f.write_str("callback wrapper ABI cannot use mutable-place method receivers"),
            Self::CallbackWrapperBorrowedParam => f.write_str("callback wrapper ABI cannot be combined with borrowed or mutable-place provider parameters"),
        }
    }
}

fn type_name(key: &ExternTypeKey) -> String {
    format!("`{}::{}`", key.module.segments.join("::"), key.name)
}

fn binding_name(key: &ExternBindingKey) -> String {
    match &key.target {
        ExternBindingTarget::Function(function) => format!(
            "function `{}::{}`",
            function.module.segments.join("::"),
            function.name
        ),
        ExternBindingTarget::Member(member) => match &member.selector {
            ExternMemberSelector::Field(name) => {
                format!("field `{name}` on type {}", type_name(&member.owner))
            }
            ExternMemberSelector::Method(name) => {
                format!("method `{name}` on type {}", type_name(&member.owner))
            }
            ExternMemberSelector::Static(name) => {
                format!("static `{name}` on type {}", type_name(&member.owner))
            }
            ExternMemberSelector::Init => {
                format!("initializer on type {}", type_name(&member.owner))
            }
            ExternMemberSelector::Operator(op) => {
                format!("operator `{op}` on type {}", type_name(&member.owner))
            }
        },
    }
}

impl std::error::Error for RustSupportError {}

pub(crate) fn member_binding_has_receiver(
    selector: &ExternMemberSelector,
    operation: ExternBindingOp,
) -> bool {
    matches!(operation, ExternBindingOp::Call)
        && matches!(
            selector,
            ExternMemberSelector::Method(_) | ExternMemberSelector::Operator(_)
        )
        || matches!(selector, ExternMemberSelector::Field(_))
            && matches!(operation, ExternBindingOp::Get | ExternBindingOp::Set)
}

pub fn native_materializer_module(rust_type_path: &str) -> String {
    let name = rust_type_path
        .rsplit("::")
        .next()
        .expect("Rust type path must contain a type name")
        .split('<')
        .next()
        .expect("Rust type path name is non-empty");
    let suffix = name.trim_start_matches('_').to_lowercase();
    format!(
        "__anvyx_native_export_{}",
        if suffix.is_empty() { "_" } else { &suffix }
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustTypeBinding {
    pub key: ExternTypeKey,
    pub path: RustPath,
    pub owns_heap_edges: bool,
    pub materializer: Option<RustMaterializerBinding>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustMaterializerBinding {
    pub mode: ExternMaterialization,
    pub rust_type: RustPath,
    pub path: RustPath,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustExternBinding {
    pub key: ExternBindingKey,
    pub path: RustPath,
    pub abi: RustExternAbi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustPath {
    pub crate_name: Rc<str>,
    pub segments: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustExternAbi {
    pub params: Vec<RustParamAdapter>,
    pub ret: RustReturnAdapter,
    pub ctx: RustCallContext,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustCallContext {
    HiddenRuntime,
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustParamAdapter {
    Value,
    OwnedNamed,
    Borrow,
    MutBorrow,
    MutPlace,
    ScopedLambda,
    EscapingLambda,
    AnvCallback,
    InitField(Box<RustParamAdapter>),
    Option(Box<RustParamAdapter>),
    Result(Box<RustParamAdapter>, Box<RustParamAdapter>),
    Slice(Box<RustParamAdapter>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustReturnAdapter {
    Void,
    Value,
    OwnedNamed,
    Option(Box<RustReturnAdapter>),
    Result(Box<RustReturnAdapter>, Box<RustReturnAdapter>),
}

impl RustParamAdapter {
    fn callback_escape(&self) -> Option<CallbackEscape> {
        match self {
            Self::ScopedLambda => Some(CallbackEscape::NonEscaping),
            Self::EscapingLambda | Self::AnvCallback => Some(CallbackEscape::Escaping),
            _ => None,
        }
    }

    fn contains_init_field(&self) -> bool {
        match self {
            Self::InitField(_) => true,
            Self::Option(inner) | Self::Slice(inner) => inner.contains_init_field(),
            Self::Result(ok, err) => ok.contains_init_field() || err.contains_init_field(),
            Self::Value
            | Self::OwnedNamed
            | Self::Borrow
            | Self::MutBorrow
            | Self::MutPlace
            | Self::ScopedLambda
            | Self::EscapingLambda
            | Self::AnvCallback => false,
        }
    }

    fn requires_hidden_runtime(&self) -> bool {
        match self {
            Self::MutPlace => true,
            Self::InitField(inner) | Self::Option(inner) | Self::Slice(inner) => {
                inner.requires_hidden_runtime()
            }
            Self::Result(ok, err) => ok.requires_hidden_runtime() || err.requires_hidden_runtime(),
            Self::Value
            | Self::OwnedNamed
            | Self::Borrow
            | Self::MutBorrow
            | Self::ScopedLambda
            | Self::EscapingLambda
            | Self::AnvCallback => false,
        }
    }

    pub fn is_shared_or_mut_borrow(&self) -> bool {
        matches!(self, Self::Borrow | Self::MutBorrow)
    }

    fn is_borrowed(&self) -> bool {
        self.is_shared_or_mut_borrow() || matches!(self, Self::MutPlace)
    }
}

fn param_adapter_matches(param: &ExternParam, adapter: &RustParamAdapter) -> bool {
    match (&param.ty, param.flow, adapter) {
        (ExternTypeExpr::Option(inner), ParamFlow::Value, RustParamAdapter::Option(adapter))
        | (ExternTypeExpr::Slice(inner), ParamFlow::Value, RustParamAdapter::Slice(adapter)) => {
            param_adapter_matches(&value_param((**inner).clone()), adapter)
        }
        (
            ExternTypeExpr::Result(ok, err),
            ParamFlow::Value,
            RustParamAdapter::Result(ok_adapter, err_adapter),
        ) => {
            param_adapter_matches(&value_param((**ok).clone()), ok_adapter)
                && param_adapter_matches(&value_param((**err).clone()), err_adapter)
        }
        (ExternTypeExpr::Callback(callback), ParamFlow::Value, adapter)
            if adapter.callback_escape().is_some() =>
        {
            callback.policy.thread == CallbackThread::SameThread
                && adapter.callback_escape() == Some(callback.policy.escape)
                && callback_escape_matches(param.escape, callback)
                && callback.callback_wrapper_signature_supported()
        }
        (ty, ParamFlow::Value, RustParamAdapter::InitField(adapter)) => {
            param_adapter_matches(&value_param(ty.clone()), adapter)
        }
        (ty, ParamFlow::Value, RustParamAdapter::Value) => bare_rust_value(ty),
        (ExternTypeExpr::Named { args, .. }, ParamFlow::Value, RustParamAdapter::OwnedNamed) => {
            args.is_empty()
        }
        (_, ParamFlow::Borrow, RustParamAdapter::Borrow)
        | (_, ParamFlow::MutBorrow, RustParamAdapter::MutBorrow | RustParamAdapter::MutPlace) => {
            true
        }
        _ => false,
    }
}

fn return_adapter_matches(ty: &ExternTypeExpr, adapter: &RustReturnAdapter) -> bool {
    match (ty, adapter) {
        (ExternTypeExpr::Void, RustReturnAdapter::Void) => true,
        (ExternTypeExpr::Option(inner), RustReturnAdapter::Option(adapter)) => {
            return_adapter_matches(inner, adapter)
        }
        (ExternTypeExpr::Result(ok, err), RustReturnAdapter::Result(ok_adapter, err_adapter)) => {
            return_adapter_matches(ok, ok_adapter) && return_adapter_matches(err, err_adapter)
        }
        (ty, RustReturnAdapter::Value) => bare_rust_value(ty),
        (ExternTypeExpr::Named { args, .. }, RustReturnAdapter::OwnedNamed) => args.is_empty(),
        _ => false,
    }
}

fn bare_rust_value(ty: &ExternTypeExpr) -> bool {
    !matches!(
        ty,
        ExternTypeExpr::Void
            | ExternTypeExpr::Option(_)
            | ExternTypeExpr::Result(_, _)
            | ExternTypeExpr::Slice(_)
            | ExternTypeExpr::Callback(_)
    )
}

fn value_param(ty: ExternTypeExpr) -> ExternParam {
    ExternParam {
        name: None,
        ty,
        flow: ParamFlow::Value,
        escape: CallbackEscape::NonEscaping,
    }
}

struct NativeSignature {
    signature: ExternSignature,
    presence_init: Vec<String>,
}

impl NativeSignature {
    fn plain(signature: ExternSignature) -> Self {
        Self {
            signature,
            presence_init: vec![],
        }
    }

    fn receiver(
        mut signature: ExternSignature,
        owner: ExternTypeExpr,
        receiver: ReceiverMode,
    ) -> Self {
        signature.params.insert(0, receiver_param(owner, receiver));
        Self::plain(signature)
    }

    fn init(init: &crate::ExternInitDescriptor) -> Self {
        Self {
            signature: ExternSignature {
                params: init.params.clone(),
                ret: init.ret.clone(),
            },
            presence_init: init.presence_init.clone(),
        }
    }
}

pub(crate) fn validate_rust_module_parts(
    descriptor: &ProviderDescriptor,
    module_path: &ModulePath,
    types: &[RustTypeBinding],
    bindings: &[RustExternBinding],
    package: &[&ProviderDescriptor],
    all_types: &[&RustTypeBinding],
) -> Result<(), RustSupportError> {
    let provider = &descriptor.provider.name;
    let module = descriptor
        .modules
        .iter()
        .find(|module| module.path == *module_path)
        .expect("catalog pairs support with its descriptor module");
    for ty in types {
        let Some(decl) = module.types.iter().find(|decl| decl.name == ty.key.name) else {
            return Err(unknown_type_support(descriptor, ty));
        };
        if ty.key.module != module.path {
            return Err(unknown_type_support(descriptor, ty));
        }
        validate_native_type(descriptor, decl, ty)?;
    }
    for binding in bindings {
        binding
            .path
            .validate(PathRole::Binding, false)
            .map_err(|error| {
                RustSupportError::binding(provider, &binding.key, BindingSupportError::Path(error))
            })?;
        let signature = native_binding_signature(module, &binding.key).ok_or_else(|| {
            RustSupportError::binding(provider, &binding.key, BindingSupportError::Unknown)
        })?;
        validate_native_abi(descriptor, package, &binding.key, &signature, &binding.abi)?;
        validate_tracked_owned_return(
            descriptor,
            &binding.key,
            &signature.signature.ret,
            &binding.abi.ret,
            all_types,
        )?;
    }
    Ok(())
}

impl RustPath {
    fn validate(&self, role: PathRole, allow_lifetime_generic: bool) -> Result<(), PathReason> {
        let failure = self
            .segments
            .iter()
            .enumerate()
            .find_map(|(index, segment)| {
                rust_path_segment_error(
                    segment,
                    allow_lifetime_generic && index + 1 == self.segments.len(),
                )
                .map(|error| (segment.clone(), error))
            })
            .or_else(|| {
                self.segments
                    .is_empty()
                    .then(|| (String::new(), PathError::Empty))
            });
        failure.map_or(Ok(()), |(segment, error)| {
            Err(PathReason {
                role,
                segment,
                error,
            })
        })
    }
}

fn rust_path_segment_error(segment: &str, allow_lifetime_generic: bool) -> Option<PathError> {
    let (ident, generic) = match split_lifetime_generic(segment) {
        Ok(parts) => parts,
        Err(error) => return Some(error),
    };
    (generic && !allow_lifetime_generic)
        .then_some(PathError::Generic)
        .or_else(|| rust_identifier_error(ident))
}

fn split_lifetime_generic(segment: &str) -> Result<(&str, bool), PathError> {
    if segment.is_empty() {
        return Err(PathError::Empty);
    }
    if segment.contains("::") || segment.contains(['/', '\\']) {
        return Err(PathError::Rooted);
    }
    match segment.split_once('<') {
        Some((ident, "'cx>")) => Ok((ident, true)),
        Some(_) => Err(PathError::Generic),
        None => Ok((segment, false)),
    }
}

pub(crate) fn valid_rust_identifier(segment: &str) -> bool {
    rust_identifier_error(segment).is_none()
}

fn rust_identifier_error(segment: &str) -> Option<PathError> {
    if matches!(segment, "." | "..") {
        return Some(PathError::Rooted);
    }
    let raw = segment.strip_prefix("r#");
    let name = raw.unwrap_or(segment);
    if matches!(name, "_" | "Self" | "self" | "super" | "crate") {
        return Some(if name == "_" {
            PathError::InvalidIdentifier
        } else {
            PathError::Reserved
        });
    }
    let mut chars = name.chars();
    let valid = chars
        .next()
        .is_some_and(|first| (first == '_' || is_xid_start(first)) && chars.all(is_xid_continue));
    if !valid {
        return Some(PathError::InvalidIdentifier);
    }
    (raw.is_none() && rust_keyword(name)).then_some(PathError::Keyword)
}

const RUST_KEYWORDS: &str = concat!(
    "abstract as async await become box break const continue crate do dyn else enum extern false final gen fn ",
    "for if impl in let loop macro match mod move mut override priv pub ref return self Self static struct ",
    "super trait true try type typeof union unsafe unsized use virtual where while yield",
);

fn rust_keyword(name: &str) -> bool {
    RUST_KEYWORDS
        .split_ascii_whitespace()
        .any(|keyword| keyword == name)
}

fn validate_native_type(
    provider: &ProviderDescriptor,
    decl: &ExternTypeDescriptor,
    support: &RustTypeBinding,
) -> Result<(), RustSupportError> {
    let name = &provider.provider.name;
    let err = |reason| RustSupportError::ty(name, &support.key, reason);
    if decl.owns_heap_edges != Some(support.owns_heap_edges) {
        return Err(err(TypeSupportError::InconsistentHeapEdges));
    }
    support
        .path
        .validate(PathRole::Type, true)
        .map_err(|error| err(TypeSupportError::Path(error)))?;
    match (decl.rep, decl.materialization, &support.materializer) {
        (ExternRep::Inline, Some(mode), Some(materializer)) => {
            for (role, path, generic) in [
                (PathRole::MaterializerType, &materializer.rust_type, true),
                (PathRole::MaterializerFunction, &materializer.path, false),
            ] {
                path.validate(role, generic)
                    .map_err(|error| err(TypeSupportError::Path(error)))?;
            }
            if materializer.mode != mode {
                return Err(err(TypeSupportError::MaterializationMode));
            }
            if materializer.rust_type != support.path {
                return Err(err(TypeSupportError::MaterializerType));
            }
            let native_type = materializer
                .rust_type
                .segments
                .last()
                .expect("validated native type path");
            let mut expected = support.path.segments[..support.path.segments.len() - 1].to_vec();
            expected.extend([
                native_materializer_module(native_type),
                INLINE_MATERIALIZER_SYMBOL.to_string(),
            ]);
            if materializer.path.crate_name != support.path.crate_name
                || materializer.path.segments != expected
            {
                return Err(err(TypeSupportError::MaterializerFunction));
            }
        }
        (ExternRep::Inline, Some(_), None) => {
            return Err(err(TypeSupportError::MissingMaterializer));
        }
        (ExternRep::Inline | ExternRep::Shared, None, None) => {}
        (ExternRep::Inline | ExternRep::Shared, None, Some(_)) => {
            return Err(err(TypeSupportError::ExtraMaterializer));
        }
        (ExternRep::Shared, Some(_), _) => {
            return Err(err(TypeSupportError::SharedMaterialization));
        }
    }
    Ok(())
}

fn unknown_type_support(provider: &ProviderDescriptor, ty: &RustTypeBinding) -> RustSupportError {
    RustSupportError::ty(&provider.provider.name, &ty.key, TypeSupportError::Unknown)
}

fn validate_tracked_owned_return(
    provider: &ProviderDescriptor,
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
    adapter: &RustReturnAdapter,
    types: &[&RustTypeBinding],
) -> Result<(), RustSupportError> {
    let Some(ty) = tracked_owned_return_type(key, ty, adapter, types) else {
        return Ok(());
    };
    Err(RustSupportError::new(
        &provider.provider.name,
        SupportErrorKind::TrackedReturn {
            key: key.clone(),
            ty: ty.clone(),
        },
    ))
}

fn tracked_owned_return_type<'a>(
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
    adapter: &RustReturnAdapter,
    types: &'a [&RustTypeBinding],
) -> Option<&'a ExternTypeKey> {
    match (ty, adapter) {
        (ExternTypeExpr::Named { module, name, args }, RustReturnAdapter::OwnedNamed)
            if args.is_empty() =>
        {
            let module = module.as_ref().unwrap_or_else(|| binding_module(key));
            types
                .iter()
                .find(|binding| {
                    binding.owns_heap_edges
                        && &binding.key.module == module
                        && binding.key.name == *name
                })
                .map(|binding| &binding.key)
        }
        (ExternTypeExpr::Option(inner), RustReturnAdapter::Option(adapter)) => {
            tracked_owned_return_type(key, inner, adapter, types)
        }
        (ExternTypeExpr::Result(ok, err), RustReturnAdapter::Result(ok_adapter, err_adapter)) => {
            tracked_owned_return_type(key, ok, ok_adapter, types)
                .or_else(|| tracked_owned_return_type(key, err, err_adapter, types))
        }
        _ => None,
    }
}

fn native_binding_signature(
    module: &ExternModuleDescriptor,
    key: &ExternBindingKey,
) -> Option<NativeSignature> {
    match (&key.target, key.operation) {
        (ExternBindingTarget::Function(function), ExternBindingOp::Call)
            if function.module == module.path =>
        {
            module
                .functions
                .iter()
                .find(|descriptor| descriptor.name == function.name)
                .map(|function| NativeSignature::plain(function.signature.clone()))
        }
        (ExternBindingTarget::Member(member), op) if member.owner.module == module.path => {
            let ty = module
                .types
                .iter()
                .find(|descriptor| descriptor.name == member.owner.name)?;
            let owner = ExternTypeExpr::Named {
                module: Some(member.owner.module.clone()),
                name: member.owner.name.clone(),
                args: vec![],
            };
            match (&member.selector, op) {
                (ExternMemberSelector::Method(name), ExternBindingOp::Call) => {
                    let method = ty.methods.iter().find(|method| method.name == *name)?;
                    Some(NativeSignature::receiver(
                        method.signature.clone(),
                        owner,
                        method.receiver,
                    ))
                }
                (ExternMemberSelector::Static(name), ExternBindingOp::Call) => ty
                    .statics
                    .iter()
                    .find(|static_method| static_method.name == *name)
                    .map(|static_method| NativeSignature::plain(static_method.signature.clone())),
                (ExternMemberSelector::Init, ExternBindingOp::Call) => {
                    let init = ty.init.as_ref()?;
                    Some(NativeSignature::init(init))
                }
                (ExternMemberSelector::Operator(op), ExternBindingOp::Call) => {
                    let operator = ty.operators.iter().find(|operator| operator.op == *op)?;
                    Some(NativeSignature::receiver(
                        operator.signature.clone(),
                        owner,
                        operator.receiver,
                    ))
                }
                (ExternMemberSelector::Field(name), ExternBindingOp::Get) => {
                    let field = ty
                        .fields
                        .iter()
                        .find(|field| field.name == *name && field.readable)?;
                    Some(NativeSignature::receiver(
                        ExternSignature {
                            params: vec![],
                            ret: field.ty.clone(),
                        },
                        owner,
                        field.get_receiver,
                    ))
                }
                (ExternMemberSelector::Field(name), ExternBindingOp::Set) => {
                    let field = ty
                        .fields
                        .iter()
                        .find(|field| field.name == *name && field.writable)?;
                    Some(NativeSignature::receiver(
                        ExternSignature {
                            params: vec![value_param(field.ty.clone())],
                            ret: ExternTypeExpr::Void,
                        },
                        owner,
                        field.set_receiver,
                    ))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn receiver_param(ty: ExternTypeExpr, receiver: ReceiverMode) -> ExternParam {
    ExternParam {
        name: Some("self".to_string()),
        ty,
        flow: match receiver {
            ReceiverMode::Value => ParamFlow::Value,
            ReceiverMode::Shared => ParamFlow::Borrow,
            ReceiverMode::Mutable => ParamFlow::MutBorrow,
        },
        escape: CallbackEscape::NonEscaping,
    }
}

fn validate_native_abi(
    descriptor: &ProviderDescriptor,
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    signature: &NativeSignature,
    abi: &RustExternAbi,
) -> Result<(), RustSupportError> {
    validate_native_abi_inner(package, key, signature, abi)
        .map_err(|reason| native_abi_error(descriptor, key, reason))
}

fn validate_native_abi_inner(
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    signature: &NativeSignature,
    abi: &RustExternAbi,
) -> Result<(), RustAbiError> {
    if abi.params.len() != signature.signature.params.len() {
        return Err(RustAbiError::ParamCount {
            expected: signature.signature.params.len(),
            found: abi.params.len(),
        });
    }
    let receiver = callback_receiver_index(key);
    let has_callback = abi
        .params
        .iter()
        .any(|adapter| adapter.callback_escape().is_some());
    validate_context(abi, has_callback)?;
    if has_callback {
        validate_callback_receiver(package, key, signature, abi, receiver)?;
    }
    for (index, (param, adapter)) in signature
        .signature
        .params
        .iter()
        .zip(&abi.params)
        .enumerate()
    {
        validate_init_field_adapter(&signature.presence_init, param, adapter)?;
        if !param_adapter_matches(param, adapter) {
            return Err(RustAbiError::Param { index });
        }
        if matches!(adapter, RustParamAdapter::MutPlace)
            && payload_has_resource(package, key, &param.ty)
        {
            return Err(RustAbiError::MutPlaceResource);
        }
        if adapter_shared_resource_value(package, key, &param.ty, adapter) {
            return Err(RustAbiError::SharedResourceParam);
        }
    }
    if !return_adapter_matches(&signature.signature.ret, &abi.ret) {
        return Err(RustAbiError::Return);
    }
    if return_adapter_has_structural_resource(package, key, &signature.signature.ret, &abi.ret) {
        return Err(RustAbiError::StructuralResourceReturn);
    }
    if extern_type_contains_callback(&signature.signature.ret) {
        return Err(RustAbiError::CallbackReturn);
    }
    Ok(())
}

fn callback_receiver_index(key: &ExternBindingKey) -> Option<usize> {
    match &key.target {
        ExternBindingTarget::Member(member)
            if member_binding_has_receiver(&member.selector, key.operation) =>
        {
            Some(0)
        }
        ExternBindingTarget::Function(_) | ExternBindingTarget::Member(_) => None,
    }
}

fn extern_type_contains_callback(ty: &ExternTypeExpr) -> bool {
    match ty {
        ExternTypeExpr::Callback(_) => true,
        ExternTypeExpr::List(inner)
        | ExternTypeExpr::Option(inner)
        | ExternTypeExpr::Array { elem: inner, .. }
        | ExternTypeExpr::Slice(inner) => extern_type_contains_callback(inner),
        ExternTypeExpr::Map(key, value) | ExternTypeExpr::Result(key, value) => {
            extern_type_contains_callback(key) || extern_type_contains_callback(value)
        }
        ExternTypeExpr::Tuple(fields) => fields.iter().any(extern_type_contains_callback),
        ExternTypeExpr::Void
        | ExternTypeExpr::Unit
        | ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Char
        | ExternTypeExpr::Any
        | ExternTypeExpr::Named { .. } => false,
    }
}

fn validate_context(abi: &RustExternAbi, has_callback: bool) -> Result<(), RustAbiError> {
    let scoped_or_escaping = abi.params.iter().any(|adapter| {
        matches!(
            adapter,
            RustParamAdapter::ScopedLambda | RustParamAdapter::EscapingLambda
        )
    });
    if scoped_or_escaping && abi.ctx != RustCallContext::None {
        return Err(RustAbiError::WrapperContext);
    }
    if !has_callback
        && abi.ctx == RustCallContext::None
        && abi
            .params
            .iter()
            .any(RustParamAdapter::requires_hidden_runtime)
    {
        return Err(RustAbiError::WrapperContext);
    }
    Ok(())
}

fn validate_callback_receiver(
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    signature: &NativeSignature,
    abi: &RustExternAbi,
    receiver: Option<usize>,
) -> Result<(), RustAbiError> {
    if receiver.is_some_and(|index| index >= abi.params.len()) {
        return Err(RustAbiError::CallbackWrapperReceiverMissing);
    }
    if receiver.is_some()
        && abi
            .params
            .iter()
            .any(|adapter| matches!(adapter, RustParamAdapter::ScopedLambda))
    {
        return Err(RustAbiError::CallbackWrapperScopedReceiver);
    }
    for (index, adapter) in abi.params.iter().enumerate() {
        if Some(index) == receiver {
            if matches!(adapter, RustParamAdapter::MutPlace) {
                return Err(RustAbiError::CallbackWrapperReceiverMutPlace);
            }
            if !matches!(
                adapter,
                RustParamAdapter::Borrow | RustParamAdapter::MutBorrow
            ) {
                return Err(RustAbiError::CallbackReceiver);
            }
            let param = &signature.signature.params[index];
            if !matches!(&param.ty, ExternTypeExpr::Named { args, .. } if args.is_empty())
                || !named_type_is_shared(
                    package,
                    key,
                    named_module(&param.ty),
                    named_name(&param.ty).unwrap_or_default(),
                )
            {
                return Err(RustAbiError::CallbackReceiverResource);
            }
        } else if adapter.is_borrowed() {
            return Err(RustAbiError::CallbackWrapperBorrowedParam);
        }
    }
    Ok(())
}

fn named_module(ty: &ExternTypeExpr) -> Option<&ModulePath> {
    match ty {
        ExternTypeExpr::Named { module, .. } => module.as_ref(),
        _ => None,
    }
}

fn named_name(ty: &ExternTypeExpr) -> Option<&str> {
    match ty {
        ExternTypeExpr::Named { name, .. } => Some(name),
        _ => None,
    }
}

fn validate_init_field_adapter(
    presence_init: &[String],
    param: &ExternParam,
    adapter: &RustParamAdapter,
) -> Result<(), RustAbiError> {
    let listed = param
        .name
        .as_ref()
        .is_some_and(|name| presence_init.contains(name));
    match (listed, adapter) {
        (true, RustParamAdapter::InitField(inner)) if !inner.contains_init_field() => Ok(()),
        (false, adapter) if !adapter.contains_init_field() => Ok(()),
        _ => Err(RustAbiError::InitField),
    }
}

fn adapter_shared_resource_value(
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
    adapter: &RustParamAdapter,
) -> bool {
    match adapter {
        RustParamAdapter::OwnedNamed => payload_has_resource(package, key, ty),
        RustParamAdapter::Value => value_has_structural_resource(package, key, ty),
        RustParamAdapter::InitField(inner) => {
            adapter_shared_resource_value(package, key, ty, inner)
        }
        RustParamAdapter::Option(inner) | RustParamAdapter::Slice(inner) => match ty {
            ExternTypeExpr::Option(ty) | ExternTypeExpr::Slice(ty) => {
                adapter_shared_resource_value(package, key, ty, inner)
            }
            _ => false,
        },
        RustParamAdapter::Result(ok, err) => match ty {
            ExternTypeExpr::Result(ok_ty, err_ty) => {
                adapter_shared_resource_value(package, key, ok_ty, ok)
                    || adapter_shared_resource_value(package, key, err_ty, err)
            }
            _ => false,
        },
        RustParamAdapter::Borrow
        | RustParamAdapter::MutBorrow
        | RustParamAdapter::MutPlace
        | RustParamAdapter::ScopedLambda
        | RustParamAdapter::EscapingLambda
        | RustParamAdapter::AnvCallback => false,
    }
}

fn return_adapter_has_structural_resource(
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
    adapter: &RustReturnAdapter,
) -> bool {
    match adapter {
        RustReturnAdapter::Value => value_has_structural_resource(package, key, ty),
        RustReturnAdapter::Option(adapter) => match ty {
            ExternTypeExpr::Option(ty) => {
                return_adapter_has_structural_resource(package, key, ty, adapter)
            }
            _ => false,
        },
        RustReturnAdapter::Result(ok, err) => match ty {
            ExternTypeExpr::Result(ok_ty, err_ty) => {
                return_adapter_has_structural_resource(package, key, ok_ty, ok)
                    || return_adapter_has_structural_resource(package, key, err_ty, err)
            }
            _ => false,
        },
        RustReturnAdapter::Void | RustReturnAdapter::OwnedNamed => false,
    }
}

fn value_has_structural_resource(
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
) -> bool {
    match ty {
        ExternTypeExpr::Tuple(fields) => fields
            .iter()
            .any(|field| payload_has_resource(package, key, field)),
        ExternTypeExpr::Array { elem, .. }
        | ExternTypeExpr::List(elem)
        | ExternTypeExpr::Slice(elem) => payload_has_resource(package, key, elem),
        ExternTypeExpr::Map(key_ty, value_ty) => {
            payload_has_resource(package, key, key_ty)
                || payload_has_resource(package, key, value_ty)
        }
        _ => false,
    }
}

fn payload_has_resource(
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
) -> bool {
    match ty {
        ExternTypeExpr::Named { module, name, args } => {
            args.is_empty() && named_type_is_shared(package, key, module.as_ref(), name)
        }
        ExternTypeExpr::Option(inner)
        | ExternTypeExpr::List(inner)
        | ExternTypeExpr::Slice(inner) => payload_has_resource(package, key, inner),
        ExternTypeExpr::Result(ok, err) | ExternTypeExpr::Map(ok, err) => {
            payload_has_resource(package, key, ok) || payload_has_resource(package, key, err)
        }
        ExternTypeExpr::Tuple(fields) => fields
            .iter()
            .any(|field| payload_has_resource(package, key, field)),
        ExternTypeExpr::Array { elem, .. } => payload_has_resource(package, key, elem),
        _ => false,
    }
}

fn named_type_is_shared(
    package: &[&ProviderDescriptor],
    key: &ExternBindingKey,
    module: Option<&ModulePath>,
    name: &str,
) -> bool {
    let module = module.unwrap_or_else(|| binding_module(key));
    package
        .iter()
        .flat_map(|descriptor| &descriptor.modules)
        .find(|candidate| &candidate.path == module)
        .and_then(|module| module.types.iter().find(|ty| ty.name == name))
        .is_some_and(|ty| ty.rep == ExternRep::Shared)
}

fn binding_module(key: &ExternBindingKey) -> &ModulePath {
    match &key.target {
        ExternBindingTarget::Function(function) => &function.module,
        ExternBindingTarget::Member(member) => &member.owner.module,
    }
}

fn native_abi_error(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    reason: RustAbiError,
) -> RustSupportError {
    RustSupportError::binding(
        &descriptor.provider.name,
        key,
        BindingSupportError::Abi(reason),
    )
}
