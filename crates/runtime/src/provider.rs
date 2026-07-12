use std::{collections::HashSet, fmt::Debug, path::PathBuf};

pub use anvyx_externs::{
    AbiPosition, BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread, ExternBindingKey,
    ExternBindingOp, ExternBindingTarget, ExternCallbackParam, ExternCallbackSignature,
    ExternEffects, ExternEnumVariantDescriptor, ExternEnumVariantFieldDescriptor,
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternFunctionKey, ExternInitDescriptor,
    ExternLayout, ExternMaterialization, ExternMemberKey, ExternMemberSelector,
    ExternMethodDescriptor, ExternModuleDescriptor, ExternOperator, ExternOperatorDescriptor,
    ExternParam, ExternRep, ExternSignature, ExternStaticDescriptor, ExternTypeDescriptor,
    ExternTypeExpr, ExternTypeKey, ModulePath, ParamFlow, ProviderDescriptor, ProviderId,
    ReceiverMode, UnaryOp, effective_callback_escape,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionExport {
    pub descriptor: ExternFunctionDescriptor,
    pub rust: RustLocalBinding,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExport {
    pub rust_type_path: &'static str,
    pub owns_heap_edges: bool,
    pub descriptor: ExternTypeDescriptor,
    pub bindings: Vec<RustMemberBinding>,
}

pub struct TypeMemberExport {
    pub rust_type_path: &'static str,
    pub export: fn() -> TypeExport,
}

inventory::collect!(TypeMemberExport);

pub fn merge_type_members(mut base: TypeExport) -> TypeExport {
    for item in inventory::iter::<TypeMemberExport> {
        if item.rust_type_path == base.rust_type_path {
            let mut members = (item.export)();
            retarget_members(&mut members, &base.descriptor.name);
            merge_member_fragment(&mut base, members);
        }
    }
    validate_type_members(&base.descriptor);
    base
}

fn merge_member_fragment(base: &mut TypeExport, mut members: TypeExport) {
    base.descriptor
        .fields
        .append(&mut members.descriptor.fields);
    if members.descriptor.init.is_some() {
        assert!(base.descriptor.init.is_none(), "duplicate extern init");
        base.descriptor.init = members.descriptor.init;
    }
    base.descriptor
        .methods
        .append(&mut members.descriptor.methods);
    base.descriptor
        .statics
        .append(&mut members.descriptor.statics);
    base.descriptor
        .operators
        .append(&mut members.descriptor.operators);
    base.bindings.append(&mut members.bindings);
}

fn retarget_members(members: &mut TypeExport, target_name: &str) {
    let source_name = members.descriptor.name.clone();
    for field in &mut members.descriptor.fields {
        retarget_type(&mut field.ty, &source_name, target_name);
    }
    for variant in &mut members.descriptor.variants {
        for field in &mut variant.fields {
            retarget_type(&mut field.ty, &source_name, target_name);
        }
    }
    if let Some(init) = &mut members.descriptor.init {
        for param in &mut init.params {
            retarget_type(&mut param.ty, &source_name, target_name);
        }
        retarget_type(&mut init.ret, &source_name, target_name);
    }
    for method in &mut members.descriptor.methods {
        retarget_signature(&mut method.signature, &source_name, target_name);
    }
    for static_method in &mut members.descriptor.statics {
        retarget_signature(&mut static_method.signature, &source_name, target_name);
    }
    for operator in &mut members.descriptor.operators {
        retarget_signature(&mut operator.signature, &source_name, target_name);
    }
    for binding in &mut members.bindings {
        retarget_abi(&mut binding.abi, &source_name, target_name);
    }
}

fn retarget_signature(signature: &mut ExternSignature, source_name: &str, target_name: &str) {
    for param in &mut signature.params {
        retarget_type(&mut param.ty, source_name, target_name);
    }
    retarget_type(&mut signature.ret, source_name, target_name);
}

fn retarget_abi(abi: &mut RustExternAbi, source_name: &str, target_name: &str) {
    for param in &mut abi.params {
        retarget_param_abi(param, source_name, target_name);
    }
    retarget_return_abi(&mut abi.ret, source_name, target_name);
}

fn retarget_param_abi(abi: &mut RustParamAbi, source_name: &str, target_name: &str) {
    match abi {
        RustParamAbi::Value(ty)
        | RustParamAbi::OwnedNamed(ty)
        | RustParamAbi::Borrow(ty)
        | RustParamAbi::MutBorrow(ty)
        | RustParamAbi::MutPlace(ty) => {
            retarget_type(ty, source_name, target_name);
        }
        RustParamAbi::ScopedLambda(callback)
        | RustParamAbi::EscapingLambda(callback)
        | RustParamAbi::AnvCallback(callback) => {
            retarget_callback(callback, source_name, target_name);
        }
        RustParamAbi::InitField(inner)
        | RustParamAbi::Option(inner)
        | RustParamAbi::Slice(inner) => {
            retarget_param_abi(inner, source_name, target_name);
        }
        RustParamAbi::Result(ok, err) => {
            retarget_param_abi(ok, source_name, target_name);
            retarget_param_abi(err, source_name, target_name);
        }
    }
}

fn retarget_callback(callback: &mut ExternCallbackSignature, source_name: &str, target_name: &str) {
    for param in &mut callback.params {
        retarget_type(&mut param.ty, source_name, target_name);
    }
    retarget_type(&mut callback.ret, source_name, target_name);
}

fn retarget_return_abi(abi: &mut RustReturnAbi, source_name: &str, target_name: &str) {
    match abi {
        RustReturnAbi::Void => {}
        RustReturnAbi::Value(ty) | RustReturnAbi::OwnedNamed(ty) => {
            retarget_type(ty, source_name, target_name);
        }
        RustReturnAbi::Option(inner) => retarget_return_abi(inner, source_name, target_name),
        RustReturnAbi::Result(ok, err) => {
            retarget_return_abi(ok, source_name, target_name);
            retarget_return_abi(err, source_name, target_name);
        }
    }
}

fn member_binding_has_receiver(
    selector: &ExternMemberSelector,
    operation: ExternBindingOp,
) -> bool {
    matches!(
        (selector, operation),
        (
            ExternMemberSelector::Method(_) | ExternMemberSelector::Operator(_),
            ExternBindingOp::Call
        ) | (
            ExternMemberSelector::Field(_),
            ExternBindingOp::Get | ExternBindingOp::Set
        )
    )
}

fn qualify_param_abi_owner(abi: &mut RustParamAbi, owner: &ExternTypeKey) {
    match abi {
        RustParamAbi::Value(ty)
        | RustParamAbi::OwnedNamed(ty)
        | RustParamAbi::Borrow(ty)
        | RustParamAbi::MutBorrow(ty)
        | RustParamAbi::MutPlace(ty) => qualify_owner_type(ty, owner),
        RustParamAbi::ScopedLambda(callback)
        | RustParamAbi::EscapingLambda(callback)
        | RustParamAbi::AnvCallback(callback) => {
            qualify_callback_owner(callback, owner);
        }
        RustParamAbi::InitField(inner)
        | RustParamAbi::Option(inner)
        | RustParamAbi::Slice(inner) => {
            qualify_param_abi_owner(inner, owner);
        }
        RustParamAbi::Result(ok, err) => {
            qualify_param_abi_owner(ok, owner);
            qualify_param_abi_owner(err, owner);
        }
    }
}

fn qualify_callback_owner(callback: &mut ExternCallbackSignature, owner: &ExternTypeKey) {
    for param in &mut callback.params {
        qualify_owner_type(&mut param.ty, owner);
    }
    qualify_owner_type(&mut callback.ret, owner);
}

fn qualify_owner_type(ty: &mut ExternTypeExpr, owner: &ExternTypeKey) {
    rewrite_type_names(ty, &mut |module, name| {
        if module.is_none() && name.as_str() == owner.name.as_str() {
            *module = Some(owner.module.clone());
        }
    });
}

fn retarget_type(ty: &mut ExternTypeExpr, source_name: &str, target_name: &str) {
    rewrite_type_names(ty, &mut |module, name| {
        if module.is_none() && name.as_str() == source_name {
            *name = target_name.to_string();
        }
    });
}

fn rewrite_type_names(
    ty: &mut ExternTypeExpr,
    rewrite: &mut impl FnMut(&mut Option<ModulePath>, &mut String),
) {
    match ty {
        ExternTypeExpr::Named { module, name, args } => {
            rewrite(module, name);
            for arg in args {
                rewrite_type_names(arg, rewrite);
            }
        }
        ExternTypeExpr::List(inner)
        | ExternTypeExpr::Option(inner)
        | ExternTypeExpr::Array { elem: inner, .. }
        | ExternTypeExpr::Slice(inner) => {
            rewrite_type_names(inner, rewrite);
        }
        ExternTypeExpr::Map(key, value) | ExternTypeExpr::Result(key, value) => {
            rewrite_type_names(key, rewrite);
            rewrite_type_names(value, rewrite);
        }
        ExternTypeExpr::Tuple(fields) => {
            for field in fields {
                rewrite_type_names(field, rewrite);
            }
        }
        ExternTypeExpr::Callback(callback) => {
            for param in &mut callback.params {
                rewrite_type_names(&mut param.ty, rewrite);
            }
            rewrite_type_names(&mut callback.ret, rewrite);
        }
        ExternTypeExpr::Void
        | ExternTypeExpr::Unit
        | ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Char
        | ExternTypeExpr::Any => {}
    }
}

fn validate_type_members(ty: &ExternTypeDescriptor) {
    assert_unique(
        ty.fields.iter().map(|field| field.name.as_str()),
        "extern field",
    );
    assert_unique(
        ty.methods.iter().map(|method| method.name.as_str()),
        "extern method",
    );
    assert_unique(
        ty.statics
            .iter()
            .map(|static_method| static_method.name.as_str()),
        "extern static",
    );
    assert_unique(
        ty.operators.iter().map(|operator| operator.op),
        "extern operator",
    );
}

fn assert_unique<T>(items: impl IntoIterator<Item = T>, label: &str)
where
    T: Eq + std::hash::Hash + Debug,
{
    let mut seen = HashSet::new();
    for item in items {
        assert!(seen.insert(item), "duplicate {label}");
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustLocalBinding {
    pub symbol: String,
    pub abi: RustExternAbi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustMemberBinding {
    pub selector: ExternMemberSelector,
    pub operation: ExternBindingOp,
    pub module: String,
    pub symbol: String,
    pub abi: RustExternAbi,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ModuleExport {
    pub functions: Vec<FunctionExport>,
    pub types: Vec<TypeExport>,
}

pub trait ModuleExportItem {
    fn push_descriptor(self, module: &mut ModuleExport);
    fn rust_bindings(self, module: ModulePath, crate_name: &str) -> Vec<RustExternBinding>;
    fn rust_type_bindings(self, module: ModulePath, crate_name: &str) -> Vec<RustTypeBinding>;
}

impl ModuleExportItem for FunctionExport {
    fn push_descriptor(self, module: &mut ModuleExport) {
        module.functions.push(self);
    }

    fn rust_bindings(self, module: ModulePath, crate_name: &str) -> Vec<RustExternBinding> {
        vec![RustExternBinding {
            key: ExternBindingKey {
                target: ExternBindingTarget::Function(ExternFunctionKey {
                    module,
                    name: self.descriptor.name,
                }),
                operation: ExternBindingOp::Call,
            },
            path: RustPath {
                crate_name: crate_name.to_string(),
                segments: vec!["__anvyx_native".to_string(), self.rust.symbol],
            },
            abi: self.rust.abi,
        }]
    }

    fn rust_type_bindings(self, _module: ModulePath, _crate_name: &str) -> Vec<RustTypeBinding> {
        vec![]
    }
}

impl ModuleExportItem for TypeExport {
    fn push_descriptor(self, module: &mut ModuleExport) {
        module.types.push(self);
    }

    fn rust_bindings(self, module: ModulePath, crate_name: &str) -> Vec<RustExternBinding> {
        let owner = ExternTypeKey {
            module,
            name: self.descriptor.name,
        };
        self.bindings
            .into_iter()
            .map(|binding| {
                let mut abi = binding.abi;
                if member_binding_has_receiver(&binding.selector, binding.operation)
                    && let Some(receiver) = abi.params.first_mut()
                {
                    qualify_param_abi_owner(receiver, &owner);
                }
                RustExternBinding {
                    key: ExternBindingKey {
                        target: ExternBindingTarget::Member(ExternMemberKey {
                            owner: owner.clone(),
                            selector: binding.selector,
                        }),
                        operation: binding.operation,
                    },
                    path: RustPath {
                        crate_name: crate_name.to_string(),
                        segments: vec![binding.module, binding.symbol],
                    },
                    abi,
                }
            })
            .collect()
    }

    fn rust_type_bindings(self, module: ModulePath, crate_name: &str) -> Vec<RustTypeBinding> {
        let segments = self
            .rust_type_path
            .split("::")
            .skip(1)
            .map(str::to_string)
            .collect();
        vec![RustTypeBinding {
            key: ExternTypeKey {
                module,
                name: self.descriptor.name,
            },
            path: RustPath {
                crate_name: crate_name.to_string(),
                segments,
            },
            owns_heap_edges: self.owns_heap_edges,
        }]
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustProviderSupport {
    pub package: String,
    pub provider: ProviderId,
    pub cargo: RustProviderCargo,
    pub modules: Vec<RustModuleSupport>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustProviderCargo {
    pub manifest_key: String,
    pub package: Option<String>,
    pub path: Option<PathBuf>,
    pub features: Vec<String>,
    pub default_features: bool,
}

impl Default for RustProviderCargo {
    fn default() -> Self {
        Self {
            manifest_key: String::new(),
            package: None,
            path: None,
            features: vec![],
            default_features: true,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustModuleSupport {
    pub module: ModulePath,
    pub types: Vec<RustTypeBinding>,
    pub bindings: Vec<RustExternBinding>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustTypeBinding {
    pub key: ExternTypeKey,
    pub path: RustPath,
    pub owns_heap_edges: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustExternBinding {
    pub key: ExternBindingKey,
    pub path: RustPath,
    pub abi: RustExternAbi,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustPath {
    pub crate_name: String,
    pub segments: Vec<String>,
}

impl RustPath {
    pub fn prefix_native(&mut self, prefix: &[String]) {
        let index = usize::from(
            self.segments
                .first()
                .is_some_and(|segment| segment == "__anvyx_native"),
        );
        self.segments.splice(index..index, prefix.iter().cloned());
    }

    pub fn retarget_native_root(&mut self, root: &[String]) {
        self.segments.splice(0..0, root.iter().cloned());
    }

    pub fn retarget_prefix(&mut self, from: &[String], to: &[String]) {
        if self.segments.starts_with(from) {
            self.segments.splice(0..from.len(), to.iter().cloned());
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustExternAbi {
    pub params: Vec<RustParamAbi>,
    pub ret: RustReturnAbi,
    pub fallible: bool,
    pub support: RustAbiSupport,
    pub ctx: RustWrapperCtx,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustAbiSupport {
    Direct,
    NeedsWrapperConversion,
    Unsupported,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustWrapperCtx {
    HiddenRuntime,
    None,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustParamAbi {
    Value(ExternTypeExpr),
    OwnedNamed(ExternTypeExpr),
    Borrow(ExternTypeExpr),
    MutBorrow(ExternTypeExpr),
    MutPlace(ExternTypeExpr),
    ScopedLambda(ExternCallbackSignature),
    EscapingLambda(ExternCallbackSignature),
    AnvCallback(ExternCallbackSignature),
    InitField(Box<RustParamAbi>),
    Option(Box<RustParamAbi>),
    Result(Box<RustParamAbi>, Box<RustParamAbi>),
    Slice(Box<RustParamAbi>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustReturnAbi {
    Void,
    Value(ExternTypeExpr),
    OwnedNamed(ExternTypeExpr),
    Option(Box<RustReturnAbi>),
    Result(Box<RustReturnAbi>, Box<RustReturnAbi>),
}

impl RustExternAbi {
    pub fn has_callback_wrapper(&self) -> bool {
        self.params
            .iter()
            .any(|param| param.class() == RustAbiClass::CallbackWrapper)
    }

    pub fn supported_callback_wrapper(&self) -> bool {
        self.supported_callback_wrapper_with_receiver(None)
    }

    pub fn supported_callback_wrapper_with_receiver(&self, receiver: Option<usize>) -> bool {
        self.has_callback_wrapper() && self.callback_wrapper_shape_error(receiver).is_none()
    }

    fn callback_wrapper_shape_error(
        &self,
        receiver: Option<usize>,
    ) -> Option<CallbackWrapperAbiError> {
        if self.ctx != RustWrapperCtx::None
            && self.params.iter().any(|param| {
                matches!(
                    param,
                    RustParamAbi::ScopedLambda(_) | RustParamAbi::EscapingLambda(_)
                )
            })
        {
            return Some(CallbackWrapperAbiError::Ctx);
        }
        if receiver.is_some()
            && self
                .params
                .iter()
                .any(|param| matches!(param, RustParamAbi::ScopedLambda(_)))
        {
            return Some(CallbackWrapperAbiError::ScopedReceiver);
        }
        if receiver.is_some_and(|index| index >= self.params.len()) {
            return Some(CallbackWrapperAbiError::ReceiverMissing);
        }
        for (index, param) in self.params.iter().enumerate() {
            if Some(index) == receiver {
                match param {
                    RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_) => {}
                    RustParamAbi::MutPlace(_) => {
                        return Some(CallbackWrapperAbiError::ReceiverMutPlace);
                    }
                    _ => return Some(CallbackWrapperAbiError::ReceiverNotBorrowed),
                }
            } else if param.is_borrowed_provider_param() {
                return Some(CallbackWrapperAbiError::BorrowedParam);
            }
            let class_supported = matches!(
                param.class(),
                RustAbiClass::Direct | RustAbiClass::CallbackWrapper
            );
            if !class_supported || param.contains_direct_collection() {
                return Some(CallbackWrapperAbiError::UnsupportedParam);
            }
        }
        if self.ret.class() != RustAbiClass::Direct || self.ret.contains_direct_collection() {
            return Some(CallbackWrapperAbiError::UnsupportedReturn);
        }
        None
    }

    pub fn backend_supported(&self) -> bool {
        match self.support {
            RustAbiSupport::Direct => {
                self.direct_ctx_supported()
                    && self.params.iter().all(|param| {
                        param.class() == RustAbiClass::Direct && !param.direct_mut_collection_abi()
                    })
                    && self.ret.class() == RustAbiClass::Direct
            }
            RustAbiSupport::NeedsWrapperConversion => self.supported_callback_wrapper(),
            RustAbiSupport::Unsupported => false,
        }
    }

    pub fn direct_ctx_supported(&self) -> bool {
        match self.ctx {
            RustWrapperCtx::HiddenRuntime => true,
            RustWrapperCtx::None => {
                !self
                    .params
                    .iter()
                    .any(RustParamAbi::requires_hidden_runtime_arg)
                    && !self.ret.requires_hidden_runtime_arg()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RustAbiClass {
    Direct,
    CallbackWrapper,
    BackendUnsupported,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CallbackWrapperAbiError {
    Ctx,
    ScopedReceiver,
    ReceiverMissing,
    ReceiverNotBorrowed,
    ReceiverMutPlace,
    BorrowedParam,
    UnsupportedParam,
    UnsupportedReturn,
}

impl CallbackWrapperAbiError {
    fn message(self) -> Option<&'static str> {
        match self {
            Self::ScopedReceiver => {
                Some("scoped callback wrapper ABI cannot be combined with method receivers")
            }
            Self::ReceiverMissing => Some("callback wrapper ABI method receiver is missing"),
            Self::ReceiverMutPlace => {
                Some("callback wrapper ABI cannot use mutable-place method receivers")
            }
            Self::ReceiverNotBorrowed => {
                Some("callback wrapper ABI method receiver must be borrowed")
            }
            Self::BorrowedParam => Some(
                "callback wrapper ABI cannot be combined with borrowed or mutable-place provider parameters",
            ),
            Self::Ctx | Self::UnsupportedParam | Self::UnsupportedReturn => None,
        }
    }
}

impl RustAbiClass {
    fn merge(self, other: Self) -> Self {
        if self == Self::Direct && other == Self::Direct {
            Self::Direct
        } else {
            Self::BackendUnsupported
        }
    }
}

impl RustParamAbi {
    pub fn is_callback_wrapper(&self) -> bool {
        self.class() == RustAbiClass::CallbackWrapper
    }

    pub fn callback_wrapper_signature(&self) -> Option<&ExternCallbackSignature> {
        match self {
            Self::ScopedLambda(callback)
            | Self::EscapingLambda(callback)
            | Self::AnvCallback(callback) => Some(callback),
            _ => None,
        }
    }

    pub fn callback_wrapper_escape(&self) -> Option<CallbackEscape> {
        match self {
            Self::ScopedLambda(_) => Some(CallbackEscape::NonEscaping),
            Self::EscapingLambda(_) | Self::AnvCallback(_) => Some(CallbackEscape::Escaping),
            _ => None,
        }
    }

    pub fn matches_extern_param(&self, param: &ExternParam) -> bool {
        param_abi_matches(param, self)
    }

    pub fn callback_wrapper_matches_param(&self, param: &ExternParam) -> bool {
        let ExternTypeExpr::Callback(callback) = &param.ty else {
            return false;
        };
        let Some(abi_callback) = self.callback_wrapper_signature() else {
            return false;
        };
        let Some(escape) = self.callback_wrapper_escape() else {
            return false;
        };
        callback == abi_callback
            && param.flow == ParamFlow::Value
            && callback.policy.escape == escape
            && callback.policy.thread == CallbackThread::SameThread
            && effective_callback_escape(param.escape, callback).is_ok()
    }

    pub fn direct_collection_abi(&self) -> bool {
        matches!(self.class(), RustAbiClass::BackendUnsupported)
            && self.contains_direct_collection()
    }

    pub fn direct_mut_collection_abi(&self) -> bool {
        matches!(self, Self::MutBorrow(ty) | Self::MutPlace(ty) if type_contains_collection(ty))
    }

    fn contains_init_field(&self) -> bool {
        match self {
            Self::InitField(_) => true,
            Self::Option(inner) | Self::Slice(inner) => inner.contains_init_field(),
            Self::Result(ok, err) => ok.contains_init_field() || err.contains_init_field(),
            Self::Value(_)
            | Self::OwnedNamed(_)
            | Self::Borrow(_)
            | Self::MutBorrow(_)
            | Self::MutPlace(_)
            | Self::ScopedLambda(_)
            | Self::EscapingLambda(_)
            | Self::AnvCallback(_) => false,
        }
    }

    fn class(&self) -> RustAbiClass {
        match self {
            Self::ScopedLambda(_) | Self::EscapingLambda(_) | Self::AnvCallback(_) => {
                RustAbiClass::CallbackWrapper
            }
            Self::InitField(inner) | Self::Option(inner) | Self::Slice(inner) => {
                inner.direct_class()
            }
            Self::Result(ok, err) => ok.direct_class().merge(err.direct_class()),
            Self::Value(ty)
            | Self::OwnedNamed(ty)
            | Self::Borrow(ty)
            | Self::MutBorrow(ty)
            | Self::MutPlace(ty) => {
                if direct_rust_type_supported(ty) {
                    RustAbiClass::Direct
                } else {
                    RustAbiClass::BackendUnsupported
                }
            }
        }
    }

    fn direct_class(&self) -> RustAbiClass {
        match self.class() {
            RustAbiClass::Direct => RustAbiClass::Direct,
            _ => RustAbiClass::BackendUnsupported,
        }
    }

    fn is_borrowed_provider_param(&self) -> bool {
        matches!(
            self,
            Self::Borrow(_) | Self::MutBorrow(_) | Self::MutPlace(_)
        )
    }

    fn contains_direct_collection(&self) -> bool {
        match self {
            Self::Value(ty)
            | Self::OwnedNamed(ty)
            | Self::Borrow(ty)
            | Self::MutBorrow(ty)
            | Self::MutPlace(ty) => type_contains_collection(ty),
            Self::InitField(inner) | Self::Option(inner) | Self::Slice(inner) => {
                inner.contains_direct_collection()
            }
            Self::Result(ok, err) => {
                ok.contains_direct_collection() || err.contains_direct_collection()
            }
            Self::ScopedLambda(_) | Self::EscapingLambda(_) | Self::AnvCallback(_) => false,
        }
    }

    fn contains_callback(&self) -> bool {
        match self {
            Self::Value(ty)
            | Self::OwnedNamed(ty)
            | Self::Borrow(ty)
            | Self::MutBorrow(ty)
            | Self::MutPlace(ty) => type_contains_callback(ty),
            Self::ScopedLambda(_) | Self::EscapingLambda(_) | Self::AnvCallback(_) => true,
            Self::InitField(inner) | Self::Option(inner) | Self::Slice(inner) => {
                inner.contains_callback()
            }
            Self::Result(ok, err) => ok.contains_callback() || err.contains_callback(),
        }
    }

    fn requires_hidden_runtime_arg(&self) -> bool {
        match self {
            Self::MutPlace(_) => true,
            Self::InitField(inner) | Self::Option(inner) | Self::Slice(inner) => {
                inner.requires_hidden_runtime_arg()
            }
            Self::Result(ok, err) => {
                ok.requires_hidden_runtime_arg() || err.requires_hidden_runtime_arg()
            }
            Self::Value(_)
            | Self::OwnedNamed(_)
            | Self::Borrow(_)
            | Self::MutBorrow(_)
            | Self::ScopedLambda(_)
            | Self::EscapingLambda(_)
            | Self::AnvCallback(_) => false,
        }
    }
}

impl RustReturnAbi {
    pub fn matches_extern_type(&self, ty: &ExternTypeExpr) -> bool {
        return_abi_matches(ty, self)
    }

    pub fn direct_collection_abi(&self) -> bool {
        matches!(self.class(), RustAbiClass::BackendUnsupported)
            && self.contains_direct_collection()
    }

    fn class(&self) -> RustAbiClass {
        if self.backend_direct_supported() {
            RustAbiClass::Direct
        } else {
            RustAbiClass::BackendUnsupported
        }
    }

    fn backend_direct_supported(&self) -> bool {
        match self {
            Self::Void => true,
            Self::Value(ty) => direct_rust_type_supported(ty),
            Self::OwnedNamed(ty) => owned_named_payload_supported(ty),
            Self::Option(inner) => inner.backend_direct_supported(),
            Self::Result(ok, err) => {
                ok.backend_direct_supported() && err.backend_direct_supported()
            }
        }
    }

    fn contains_direct_collection(&self) -> bool {
        match self {
            Self::Value(ty) | Self::OwnedNamed(ty) => type_contains_collection(ty),
            Self::Option(inner) => inner.contains_direct_collection(),
            Self::Result(ok, err) => {
                ok.contains_direct_collection() || err.contains_direct_collection()
            }
            Self::Void => false,
        }
    }

    fn requires_hidden_runtime_arg(&self) -> bool {
        match self {
            Self::Option(inner) => inner.requires_hidden_runtime_arg(),
            Self::Result(ok, err) => {
                ok.requires_hidden_runtime_arg() || err.requires_hidden_runtime_arg()
            }
            Self::Void | Self::Value(_) | Self::OwnedNamed(_) => false,
        }
    }
}

fn direct_rust_type_supported(ty: &ExternTypeExpr) -> bool {
    match ty {
        ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Char
        | ExternTypeExpr::Unit => true,
        ExternTypeExpr::Named { args, .. } => args.is_empty(),
        ExternTypeExpr::Option(inner)
        | ExternTypeExpr::List(inner)
        | ExternTypeExpr::Array { elem: inner, .. } => direct_rust_type_supported(inner),
        ExternTypeExpr::Result(ok, err) | ExternTypeExpr::Map(ok, err) => {
            direct_rust_type_supported(ok) && direct_rust_type_supported(err)
        }
        ExternTypeExpr::Tuple(fields) => fields.iter().all(direct_rust_type_supported),
        ExternTypeExpr::Void
        | ExternTypeExpr::Any
        | ExternTypeExpr::Slice(_)
        | ExternTypeExpr::Callback(_) => false,
    }
}

#[derive(Clone)]
struct NativeSignature {
    signature: ExternSignature,
    effects: ExternEffects,
    presence_init: Vec<String>,
}

pub fn validate_rust_provider_support(
    descriptors: &[ProviderDescriptor],
    supports: &[RustProviderSupport],
) -> Result<(), String> {
    validate_unique_native_support(supports)?;
    for support in supports {
        let descriptor = descriptors
            .iter()
            .find(|descriptor| descriptor.provider == support.provider)
            .ok_or_else(|| {
                format!(
                    "native provider `{}` has no descriptor",
                    support.provider.name
                )
            })?;
        let types = support
            .modules
            .iter()
            .flat_map(|module| module.types.iter().cloned())
            .collect::<Vec<_>>();
        for module in &support.modules {
            validate_native_module(descriptor, module, &types)?;
        }
    }
    Ok(())
}

fn validate_unique_native_support(supports: &[RustProviderSupport]) -> Result<(), String> {
    let mut types = HashSet::new();
    let mut bindings = HashSet::new();
    for support in supports {
        for module in &support.modules {
            for ty in &module.types {
                let key = (
                    support.package.clone(),
                    support.provider.clone(),
                    ty.key.clone(),
                );
                if !types.insert(key) {
                    return Err(format!(
                        "native provider `{}` has duplicate type support for `{}::{}`",
                        support.provider.name,
                        ty.key.module.segments.join("::"),
                        ty.key.name,
                    ));
                }
            }
            for binding in &module.bindings {
                let key = (
                    support.package.clone(),
                    support.provider.clone(),
                    binding.key.clone(),
                );
                if !bindings.insert(key) {
                    return Err(format!(
                        "native provider `{}` has duplicate binding support for {:?}",
                        support.provider.name, binding.key,
                    ));
                }
            }
        }
    }
    Ok(())
}

fn validate_native_module(
    descriptor: &ProviderDescriptor,
    support: &RustModuleSupport,
    types: &[RustTypeBinding],
) -> Result<(), String> {
    let module = descriptor
        .modules
        .iter()
        .find(|module| module.path == support.module)
        .ok_or_else(|| {
            format!(
                "native provider `{}` has support for unknown module {:?}",
                descriptor.provider.name, support.module.segments
            )
        })?;
    for ty in &support.types {
        let Some(decl) = module.types.iter().find(|decl| decl.name == ty.key.name) else {
            return Err(format!(
                "native provider `{}` has support for unknown type `{}::{}`",
                descriptor.provider.name,
                ty.key.module.segments.join("::"),
                ty.key.name
            ));
        };
        if ty.key.module != support.module {
            return Err(format!(
                "native provider `{}` has support for unknown type `{}::{}`",
                descriptor.provider.name,
                ty.key.module.segments.join("::"),
                ty.key.name
            ));
        }
        if decl.owns_heap_edges != Some(ty.owns_heap_edges) {
            return Err(format!(
                "native provider `{}` has inconsistent heap-edge metadata for type `{}::{}`",
                descriptor.provider.name,
                ty.key.module.segments.join("::"),
                ty.key.name
            ));
        }
    }
    for binding in &support.bindings {
        let signature = native_binding_signature(module, &binding.key).ok_or_else(|| {
            format!(
                "native provider `{}` has support for unknown binding {:?}",
                descriptor.provider.name, binding.key
            )
        })?;
        validate_native_abi(descriptor, &binding.key, &signature, &binding.abi)?;
        validate_tracked_owned_return(&binding.key, &binding.abi.ret, types)?;
    }
    Ok(())
}

fn validate_tracked_owned_return(
    key: &ExternBindingKey,
    abi: &RustReturnAbi,
    types: &[RustTypeBinding],
) -> Result<(), String> {
    let Some(ty) = tracked_owned_return_type(key, abi, types) else {
        return Ok(());
    };
    Err(format!(
        "native binding {:?} returns tracked resource `{}` by owned value; return AnvRef instead",
        key, ty.name
    ))
}

fn tracked_owned_return_type<'a>(
    key: &ExternBindingKey,
    abi: &RustReturnAbi,
    types: &'a [RustTypeBinding],
) -> Option<&'a ExternTypeKey> {
    match abi {
        RustReturnAbi::OwnedNamed(ty) => tracked_owned_named_type(key, ty, types),
        RustReturnAbi::Option(inner) => tracked_owned_return_type(key, inner, types),
        RustReturnAbi::Result(ok, err) => tracked_owned_return_type(key, ok, types)
            .or_else(|| tracked_owned_return_type(key, err, types)),
        RustReturnAbi::Void | RustReturnAbi::Value(_) => None,
    }
}

fn tracked_owned_named_type<'a>(
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
    types: &'a [RustTypeBinding],
) -> Option<&'a ExternTypeKey> {
    let ExternTypeExpr::Named { module, name, args } = ty else {
        return None;
    };
    if !args.is_empty() {
        return None;
    }
    let module = module.as_ref().unwrap_or_else(|| binding_module(key));
    types
        .iter()
        .find(|binding| {
            binding.owns_heap_edges && &binding.key.module == module && binding.key.name == *name
        })
        .map(|binding| &binding.key)
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
                .map(|function| NativeSignature {
                    signature: function.signature.clone(),
                    effects: function.effects,
                    presence_init: vec![],
                })
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
                    Some(with_receiver(
                        method.signature.clone(),
                        owner,
                        method.receiver,
                        method.effects,
                    ))
                }
                (ExternMemberSelector::Static(name), ExternBindingOp::Call) => ty
                    .statics
                    .iter()
                    .find(|static_method| static_method.name == *name)
                    .map(|static_method| NativeSignature {
                        signature: static_method.signature.clone(),
                        effects: static_method.effects,
                        presence_init: vec![],
                    }),
                (ExternMemberSelector::Init, ExternBindingOp::Call) => {
                    let init = ty.init.as_ref()?;
                    Some(NativeSignature {
                        signature: ExternSignature {
                            params: init.params.clone(),
                            ret: init.ret.clone(),
                        },
                        effects: init.effects,
                        presence_init: init.presence_init.clone(),
                    })
                }
                (ExternMemberSelector::Operator(op), ExternBindingOp::Call) => {
                    let operator = ty.operators.iter().find(|operator| operator.op == *op)?;
                    Some(with_receiver(
                        operator.signature.clone(),
                        owner,
                        operator.receiver,
                        operator.effects,
                    ))
                }
                (ExternMemberSelector::Field(name), ExternBindingOp::Get) => {
                    let field = ty
                        .fields
                        .iter()
                        .find(|field| field.name == *name && field.readable)?;
                    Some(with_receiver(
                        ExternSignature {
                            params: vec![],
                            ret: field.ty.clone(),
                        },
                        owner,
                        field.get_receiver,
                        ExternEffects::default(),
                    ))
                }
                (ExternMemberSelector::Field(name), ExternBindingOp::Set) => {
                    let field = ty
                        .fields
                        .iter()
                        .find(|field| field.name == *name && field.writable)?;
                    Some(with_receiver(
                        ExternSignature {
                            params: vec![value_param(field.ty.clone())],
                            ret: ExternTypeExpr::Void,
                        },
                        owner,
                        field.set_receiver,
                        ExternEffects::default(),
                    ))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

fn with_receiver(
    mut signature: ExternSignature,
    owner: ExternTypeExpr,
    receiver: ReceiverMode,
    effects: ExternEffects,
) -> NativeSignature {
    signature.params.insert(0, receiver_param(owner, receiver));
    NativeSignature {
        signature,
        effects,
        presence_init: vec![],
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

fn value_param(ty: ExternTypeExpr) -> ExternParam {
    ExternParam {
        name: None,
        ty,
        flow: ParamFlow::Value,
        escape: CallbackEscape::NonEscaping,
    }
}

fn validate_native_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    signature: &NativeSignature,
    abi: &RustExternAbi,
) -> Result<(), String> {
    validate_signature_abi(descriptor, key, &signature.signature)?;
    if abi.fallible != signature.effects.fallible {
        return Err(native_abi_error(descriptor, key, "fallible flag mismatch"));
    }
    if abi.params.len() != signature.signature.params.len() {
        return Err(native_abi_error(
            descriptor,
            key,
            "parameter count mismatch",
        ));
    }
    let receiver = callback_receiver_index(key);
    let has_callback_wrapper = abi.has_callback_wrapper();
    validate_wrapper_ctx(descriptor, key, abi, has_callback_wrapper)?;
    validate_callback_wrapper_boundary(descriptor, key, abi, receiver)?;
    for (index, (param, param_abi)) in signature
        .signature
        .params
        .iter()
        .zip(&abi.params)
        .enumerate()
    {
        validate_init_field_abi(descriptor, key, &signature.presence_init, param, param_abi)?;
        let callback_checked = validate_param_callback_abi(descriptor, key, param, param_abi, abi)?;
        if !callback_checked && !param_abi_matches(param, param_abi) {
            return Err(native_abi_error(
                descriptor,
                key,
                &format!("parameter {index} ABI mismatch"),
            ));
        }
        if matches!(param_abi, RustParamAbi::MutPlace(ty) if payload_has_resource(descriptor, key, ty))
        {
            return Err(native_abi_error(
                descriptor,
                key,
                "mutable-place ABI is unsupported for shared resources",
            ));
        }
    }
    if abi
        .params
        .iter()
        .any(|param| param_abi_has_shared_resource_value(descriptor, key, param))
    {
        return Err(native_abi_error(
            descriptor,
            key,
            "shared resource parameters must use top-level, Option, or Result AnvRef",
        ));
    }
    if !return_abi_matches(&signature.signature.ret, &abi.ret) {
        return Err(native_abi_error(descriptor, key, "return ABI mismatch"));
    }
    if return_abi_has_structural_resource(descriptor, key, &abi.ret) {
        return Err(native_abi_error(
            descriptor,
            key,
            "structural owned resource return ABI is unsupported",
        ));
    }
    if type_contains_callback(&signature.signature.ret) {
        return Err(native_abi_error(
            descriptor,
            key,
            "native callback return ABI is unsupported",
        ));
    }
    if abi
        .params
        .iter()
        .any(RustParamAbi::direct_mut_collection_abi)
    {
        return Err(native_abi_error(
            descriptor,
            key,
            "direct mutable collection ABI is unsupported",
        ));
    }
    let direct_collection = abi.params.iter().any(RustParamAbi::direct_collection_abi)
        || abi.ret.direct_collection_abi();
    if direct_collection {
        return Err(native_abi_error(
            descriptor,
            key,
            "unsupported native ABI metadata",
        ));
    }
    validate_wrapper_conversion_abi(descriptor, key, abi, receiver)?;
    if abi.support == RustAbiSupport::Direct && !abi.backend_supported() {
        return Err(native_abi_error(
            descriptor,
            key,
            "unsupported native ABI metadata",
        ));
    }
    Ok(())
}

fn validate_signature_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    signature: &ExternSignature,
) -> Result<(), String> {
    for (index, param) in signature.params.iter().enumerate() {
        let position = match param.flow {
            ParamFlow::Value => AbiPosition::ParamValue,
            ParamFlow::Borrow => AbiPosition::ParamBorrow,
            ParamFlow::MutBorrow => AbiPosition::ParamMutBorrow,
        };
        if let Err(violations) = param.ty.classify_abi(position) {
            let violation = violations[0];
            return Err(native_abi_error(
                descriptor,
                key,
                &format!(
                    "parameter {index} descriptor ABI violation: {:?} at {:?}",
                    violation.reason, violation.position
                ),
            ));
        }
    }
    if let Err(violations) = signature.ret.classify_abi(AbiPosition::Return) {
        let violation = violations[0];
        return Err(native_abi_error(
            descriptor,
            key,
            &format!(
                "return descriptor ABI violation: {:?} at {:?}",
                violation.reason, violation.position
            ),
        ));
    }
    Ok(())
}

fn validate_wrapper_ctx(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustExternAbi,
    has_callback_wrapper: bool,
) -> Result<(), String> {
    let valid = if has_callback_wrapper {
        abi.ctx == RustWrapperCtx::None
            || (abi.ctx == RustWrapperCtx::HiddenRuntime
                && abi.params.iter().all(|param| {
                    !matches!(
                        param,
                        RustParamAbi::ScopedLambda(_) | RustParamAbi::EscapingLambda(_)
                    )
                }))
    } else if abi.support == RustAbiSupport::Direct {
        abi.direct_ctx_supported()
    } else {
        abi.ctx == RustWrapperCtx::HiddenRuntime
    };
    if valid {
        Ok(())
    } else {
        Err(native_abi_error(
            descriptor,
            key,
            "wrapper ctx mode mismatch",
        ))
    }
}

fn validate_wrapper_conversion_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustExternAbi,
    receiver: Option<usize>,
) -> Result<(), String> {
    match abi.support {
        RustAbiSupport::Direct => Ok(()),
        RustAbiSupport::Unsupported => Err(native_abi_error(
            descriptor,
            key,
            "unsupported native ABI metadata",
        )),
        RustAbiSupport::NeedsWrapperConversion
            if abi.supported_callback_wrapper_with_receiver(receiver) =>
        {
            Ok(())
        }
        RustAbiSupport::NeedsWrapperConversion => Err(native_abi_error(
            descriptor,
            key,
            "unsupported wrapper conversion ABI",
        )),
    }
}

fn validate_callback_wrapper_boundary(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustExternAbi,
    receiver: Option<usize>,
) -> Result<(), String> {
    if !abi.has_callback_wrapper() {
        return Ok(());
    }
    if let Some(message) = abi
        .callback_wrapper_shape_error(receiver)
        .and_then(CallbackWrapperAbiError::message)
    {
        return Err(native_abi_error(descriptor, key, message));
    }
    if let Some(index) = receiver {
        validate_callback_wrapper_receiver_resource(descriptor, key, abi, index)?;
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

fn validate_callback_wrapper_receiver_resource(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustExternAbi,
    index: usize,
) -> Result<(), String> {
    let Some(RustParamAbi::Borrow(receiver_ty) | RustParamAbi::MutBorrow(receiver_ty)) =
        abi.params.get(index)
    else {
        return Err(native_abi_error(
            descriptor,
            key,
            "callback wrapper ABI method receiver must be borrowed",
        ));
    };
    let ExternTypeExpr::Named { module, name, args } = receiver_ty else {
        return Err(native_abi_error(
            descriptor,
            key,
            "callback wrapper ABI method receiver must be a shared resource",
        ));
    };
    if args.is_empty() && named_type_is_shared(descriptor, key, module.as_ref(), name) {
        Ok(())
    } else {
        Err(native_abi_error(
            descriptor,
            key,
            "callback wrapper ABI method receiver must be a shared resource",
        ))
    }
}

fn validate_init_field_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    presence_init: &[String],
    param: &ExternParam,
    abi: &RustParamAbi,
) -> Result<(), String> {
    let listed = param
        .name
        .as_ref()
        .is_some_and(|name| presence_init.contains(name));
    match (listed, abi) {
        (true, RustParamAbi::InitField(inner)) if !inner.contains_init_field() => Ok(()),
        (false, abi) if !abi.contains_init_field() => Ok(()),
        _ => Err(native_abi_error(descriptor, key, "init field ABI mismatch")),
    }
}

fn validate_param_callback_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    param: &ExternParam,
    param_abi: &RustParamAbi,
    abi: &RustExternAbi,
) -> Result<bool, String> {
    if let ExternTypeExpr::Callback(callback) = &param.ty
        && effective_callback_escape(param.escape, callback).is_err()
    {
        return Err(native_abi_error(
            descriptor,
            key,
            "callback escape metadata mismatch",
        ));
    }

    match (&param.ty, param_abi.callback_wrapper_signature()) {
        (ExternTypeExpr::Callback(_), Some(_)) => {
            validate_callback_wrapper_abi(descriptor, key, param, param_abi, abi.support)?;
            Ok(true)
        }
        _ if param_contains_callback(param) || param_abi.contains_callback() => Err(
            native_abi_error(descriptor, key, "direct callback ABI is unsupported"),
        ),
        _ => Ok(false),
    }
}

fn validate_callback_wrapper_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    param: &ExternParam,
    param_abi: &RustParamAbi,
    support: RustAbiSupport,
) -> Result<(), String> {
    let ExternTypeExpr::Callback(callback) = &param.ty else {
        return Err(native_abi_error(
            descriptor,
            key,
            "direct callback ABI is unsupported",
        ));
    };
    let escape = param_abi
        .callback_wrapper_escape()
        .expect("callback wrapper ABI must have an expected escape");
    let label = match escape {
        CallbackEscape::NonEscaping => "scoped Lambda",
        CallbackEscape::Escaping => "escaping Lambda",
    };
    if param_abi.callback_wrapper_signature() != Some(callback) {
        return Err(native_abi_error(
            descriptor,
            key,
            &format!("{label} ABI signature mismatch"),
        ));
    }
    if !param_abi.callback_wrapper_matches_param(param) {
        let escape_label = match escape {
            CallbackEscape::NonEscaping => "non-escaping",
            CallbackEscape::Escaping => "escaping",
        };
        return Err(native_abi_error(
            descriptor,
            key,
            &format!("{label} ABI requires {escape_label} same-thread callback policy"),
        ));
    }
    if support != RustAbiSupport::NeedsWrapperConversion {
        return Err(native_abi_error(
            descriptor,
            key,
            &format!("{label} ABI requires wrapper conversion"),
        ));
    }
    if !callback.callback_wrapper_signature_supported() {
        return Err(native_abi_error(
            descriptor,
            key,
            &format!("unsupported {label} ABI signature"),
        ));
    }
    Ok(())
}

fn param_contains_callback(param: &ExternParam) -> bool {
    param.escape == CallbackEscape::Escaping || type_contains_callback(&param.ty)
}

fn type_contains_callback(ty: &ExternTypeExpr) -> bool {
    match ty {
        ExternTypeExpr::Callback(_) => true,
        ExternTypeExpr::List(inner)
        | ExternTypeExpr::Option(inner)
        | ExternTypeExpr::Array { elem: inner, .. }
        | ExternTypeExpr::Slice(inner) => type_contains_callback(inner),
        ExternTypeExpr::Map(key, value) | ExternTypeExpr::Result(key, value) => {
            type_contains_callback(key) || type_contains_callback(value)
        }
        ExternTypeExpr::Tuple(fields) => fields.iter().any(type_contains_callback),
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

fn type_contains_collection(ty: &ExternTypeExpr) -> bool {
    match ty {
        ExternTypeExpr::List(_) | ExternTypeExpr::Map(_, _) => true,
        ExternTypeExpr::Option(inner)
        | ExternTypeExpr::Array { elem: inner, .. }
        | ExternTypeExpr::Slice(inner) => type_contains_collection(inner),
        ExternTypeExpr::Result(ok, err) => {
            type_contains_collection(ok) || type_contains_collection(err)
        }
        ExternTypeExpr::Tuple(fields) => fields.iter().any(type_contains_collection),
        ExternTypeExpr::Callback(callback) => {
            callback
                .params
                .iter()
                .any(|param| type_contains_collection(&param.ty))
                || type_contains_collection(&callback.ret)
        }
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

fn param_abi_matches(param: &ExternParam, abi: &RustParamAbi) -> bool {
    match (&param.ty, param.flow, abi) {
        (ExternTypeExpr::Option(inner), ParamFlow::Value, RustParamAbi::Option(abi))
        | (ExternTypeExpr::Slice(inner), ParamFlow::Value, RustParamAbi::Slice(abi)) => {
            param_abi_matches(&value_param((**inner).clone()), abi)
        }
        (
            ExternTypeExpr::Result(ok, err),
            ParamFlow::Value,
            RustParamAbi::Result(abi_ok, abi_err),
        ) => {
            param_abi_matches(&value_param((**ok).clone()), abi_ok)
                && param_abi_matches(&value_param((**err).clone()), abi_err)
        }
        (ExternTypeExpr::Callback(_), _, abi) if abi.is_callback_wrapper() => {
            abi.callback_wrapper_matches_param(param)
        }
        (ty, ParamFlow::Value, RustParamAbi::InitField(abi)) => {
            param_abi_matches(&value_param(ty.clone()), abi)
        }
        (ty, ParamFlow::Value, RustParamAbi::Value(abi_ty)) => bare_rust_value_matches(ty, abi_ty),
        (ty, ParamFlow::Value, RustParamAbi::OwnedNamed(abi_ty)) => {
            owned_named_payload_supported(abi_ty) && bare_rust_value_matches(ty, abi_ty)
        }
        (ty, ParamFlow::Borrow, RustParamAbi::Borrow(abi_ty))
        | (
            ty,
            ParamFlow::MutBorrow,
            RustParamAbi::MutBorrow(abi_ty) | RustParamAbi::MutPlace(abi_ty),
        ) => ty == abi_ty,
        _ => false,
    }
}

fn bare_rust_value_matches(expected: &ExternTypeExpr, found: &ExternTypeExpr) -> bool {
    !matches!(
        expected,
        ExternTypeExpr::Void
            | ExternTypeExpr::Option(_)
            | ExternTypeExpr::Result(_, _)
            | ExternTypeExpr::Slice(_)
    ) && expected == found
}

fn owned_named_payload_supported(ty: &ExternTypeExpr) -> bool {
    matches!(ty, ExternTypeExpr::Named { args, .. } if args.is_empty())
}

fn return_abi_matches(ret: &ExternTypeExpr, abi: &RustReturnAbi) -> bool {
    match (ret, abi) {
        (ExternTypeExpr::Void, RustReturnAbi::Void) => true,
        (ExternTypeExpr::Option(inner), RustReturnAbi::Option(abi)) => {
            return_abi_matches(inner, abi)
        }
        (ExternTypeExpr::Result(ok, err), RustReturnAbi::Result(abi_ok, abi_err)) => {
            return_abi_matches(ok, abi_ok) && return_abi_matches(err, abi_err)
        }
        (ty, RustReturnAbi::Value(abi_ty)) => bare_rust_value_matches(ty, abi_ty),
        (ty, RustReturnAbi::OwnedNamed(abi_ty)) => {
            owned_named_payload_supported(abi_ty) && bare_rust_value_matches(ty, abi_ty)
        }
        _ => false,
    }
}

fn param_abi_has_shared_resource_value(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustParamAbi,
) -> bool {
    match abi {
        RustParamAbi::OwnedNamed(ty) => payload_has_resource(descriptor, key, ty),
        RustParamAbi::Value(ty) => value_has_structural_resource(descriptor, key, ty),
        RustParamAbi::InitField(inner)
        | RustParamAbi::Option(inner)
        | RustParamAbi::Slice(inner) => param_abi_has_shared_resource_value(descriptor, key, inner),
        RustParamAbi::Result(ok, err) => {
            param_abi_has_shared_resource_value(descriptor, key, ok)
                || param_abi_has_shared_resource_value(descriptor, key, err)
        }
        RustParamAbi::Borrow(_)
        | RustParamAbi::MutBorrow(_)
        | RustParamAbi::MutPlace(_)
        | RustParamAbi::ScopedLambda(_)
        | RustParamAbi::EscapingLambda(_)
        | RustParamAbi::AnvCallback(_) => false,
    }
}

fn return_abi_has_structural_resource(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustReturnAbi,
) -> bool {
    match abi {
        RustReturnAbi::Value(ty) => value_has_structural_resource(descriptor, key, ty),
        RustReturnAbi::Option(inner) => return_abi_has_structural_resource(descriptor, key, inner),
        RustReturnAbi::Result(ok, err) => {
            return_abi_has_structural_resource(descriptor, key, ok)
                || return_abi_has_structural_resource(descriptor, key, err)
        }
        RustReturnAbi::Void | RustReturnAbi::OwnedNamed(_) => false,
    }
}

fn value_has_structural_resource(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
) -> bool {
    match ty {
        ExternTypeExpr::Tuple(fields) => fields
            .iter()
            .any(|field| payload_has_resource(descriptor, key, field)),
        ExternTypeExpr::Array { elem, .. }
        | ExternTypeExpr::List(elem)
        | ExternTypeExpr::Slice(elem) => payload_has_resource(descriptor, key, elem),
        ExternTypeExpr::Map(key_ty, value_ty) => {
            payload_has_resource(descriptor, key, key_ty)
                || payload_has_resource(descriptor, key, value_ty)
        }
        _ => false,
    }
}

fn payload_has_resource(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    ty: &ExternTypeExpr,
) -> bool {
    match ty {
        ExternTypeExpr::Named { module, name, args } => {
            args.is_empty() && named_type_is_shared(descriptor, key, module.as_ref(), name)
        }
        ExternTypeExpr::Option(inner)
        | ExternTypeExpr::List(inner)
        | ExternTypeExpr::Slice(inner) => payload_has_resource(descriptor, key, inner),
        ExternTypeExpr::Result(ok, err) | ExternTypeExpr::Map(ok, err) => {
            payload_has_resource(descriptor, key, ok) || payload_has_resource(descriptor, key, err)
        }
        ExternTypeExpr::Tuple(fields) => fields
            .iter()
            .any(|field| payload_has_resource(descriptor, key, field)),
        ExternTypeExpr::Array { elem, .. } => payload_has_resource(descriptor, key, elem),
        _ => false,
    }
}

fn named_type_is_shared(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    module: Option<&ModulePath>,
    name: &str,
) -> bool {
    let module = module.unwrap_or_else(|| binding_module(key));
    descriptor
        .modules
        .iter()
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
    reason: &str,
) -> String {
    format!(
        "native provider `{}` has mismatched ABI for binding {:?}: {reason}",
        descriptor.provider.name, key
    )
}

pub trait AnvyxInlineExport: Copy {}

/// # Safety
/// Manual impls must set `OWNS_ANVYX_HEAP_EDGES` to true when the type owns any
/// Anvyx heap edge, including hidden `Handle`, `ErasedHandle`, `AnvRef`,
/// `AnvList`, `AnvMap`, `AnvCallback`, or retained callback fields.
pub unsafe trait AnvyxRefExport {
    const OWNS_ANVYX_HEAP_EDGES: bool = false;
}

pub trait AnvyxEnumExport: Clone + PartialEq + Eq + std::hash::Hash {}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn rejects_unknown_binding() {
        let error = validate_rust_provider_support(&[descriptor()], &[support(binding("pong"))])
            .unwrap_err();

        assert!(error.contains("unknown binding"));
    }

    #[test]
    fn rejects_wrong_operation() {
        let mut binding = binding("ping");
        binding.key.operation = ExternBindingOp::Set;

        let error =
            validate_rust_provider_support(&[descriptor()], &[support(binding)]).unwrap_err();

        assert!(error.contains("unknown binding"));
    }

    #[test]
    fn rejects_non_named_owned_return_payload() {
        let list = ExternTypeExpr::List(Box::new(ExternTypeExpr::Int));
        assert_abi_error(
            descriptor_with_params("owned_list", vec![], list.clone()),
            binding_with_abi(
                "owned_list",
                RustParamAbi::Value(ExternTypeExpr::Void),
                RustReturnAbi::OwnedNamed(list),
            ),
            "return ABI mismatch",
        );
    }

    #[test]
    fn rejects_tracked_owned_resource_return() {
        let resource = shared_resource();
        let mut resource_type = shared_resource_type(vec![]);
        resource_type.owns_heap_edges = Some(true);
        let descriptor = ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![resource_type],
                functions: vec![ExternFunctionDescriptor {
                    name: "make".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![],
                        ret: resource.clone(),
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        };
        let mut support = support(binding_with_abi(
            "make",
            RustParamAbi::Value(ExternTypeExpr::Void),
            RustReturnAbi::OwnedNamed(resource),
        ));
        support.modules[0].types.push(RustTypeBinding {
            key: ExternTypeKey {
                module: module(),
                name: "Thing".to_string(),
            },
            path: RustPath {
                crate_name: "test".to_string(),
                segments: vec!["Thing".to_string()],
            },
            owns_heap_edges: true,
        });

        let error = validate_rust_provider_support(&[descriptor], &[support]).unwrap_err();

        assert!(error.contains("returns tracked resource"), "{error}");
    }

    #[test]
    fn rejects_cross_module_tracked_owned_resource_return() {
        let resource_module = ModulePath {
            segments: vec!["resource".to_string()],
        };
        let api_module = ModulePath {
            segments: vec!["api".to_string()],
        };
        let resource = ExternTypeExpr::Named {
            module: Some(resource_module.clone()),
            name: "Thing".to_string(),
            args: vec![],
        };
        let mut resource_type = shared_resource_type(vec![]);
        resource_type.owns_heap_edges = Some(true);
        let descriptor = ProviderDescriptor {
            provider: provider(),
            modules: vec![
                ExternModuleDescriptor {
                    path: resource_module.clone(),
                    types: vec![resource_type],
                    functions: vec![],
                },
                ExternModuleDescriptor {
                    path: api_module.clone(),
                    types: vec![],
                    functions: vec![ExternFunctionDescriptor {
                        name: "make".to_string(),
                        doc: None,
                        signature: ExternSignature {
                            params: vec![],
                            ret: resource.clone(),
                        },
                        effects: ExternEffects::default(),
                    }],
                },
            ],
        };
        let binding = RustExternBinding {
            key: ExternBindingKey {
                target: ExternBindingTarget::Function(ExternFunctionKey {
                    module: api_module.clone(),
                    name: "make".to_string(),
                }),
                operation: ExternBindingOp::Call,
            },
            path: RustPath {
                crate_name: "test".to_string(),
                segments: vec!["make".to_string()],
            },
            abi: RustExternAbi {
                params: vec![],
                ret: RustReturnAbi::OwnedNamed(resource),
                fallible: false,
                support: RustAbiSupport::Direct,
                ctx: RustWrapperCtx::None,
            },
        };
        let mut support = support(binding);
        support.modules = vec![
            RustModuleSupport {
                module: resource_module.clone(),
                types: vec![RustTypeBinding {
                    key: ExternTypeKey {
                        module: resource_module,
                        name: "Thing".to_string(),
                    },
                    path: RustPath {
                        crate_name: "test".to_string(),
                        segments: vec!["Thing".to_string()],
                    },
                    owns_heap_edges: true,
                }],
                bindings: vec![],
            },
            RustModuleSupport {
                module: api_module,
                types: vec![],
                bindings: support.modules[0].bindings.clone(),
            },
        ];

        let error = validate_rust_provider_support(&[descriptor], &[support]).unwrap_err();

        assert!(error.contains("returns tracked resource"), "{error}");
    }

    #[test]
    fn rejects_structural_resource_value_return() {
        let resource = ExternTypeExpr::Named {
            module: None,
            name: "Thing".to_string(),
            args: vec![],
        };
        let ret = ExternTypeExpr::Tuple(vec![resource.clone(), ExternTypeExpr::Int]);
        let descriptor = ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![ExternTypeDescriptor {
                    name: "Thing".to_string(),
                    doc: None,
                    rep: ExternRep::Shared,
                    layout: None,
                    materialization: None,
                    owns_heap_edges: None,
                    fields: vec![],
                    variants: vec![],
                    init: None,
                    methods: vec![],
                    statics: vec![],
                    operators: vec![],
                }],
                functions: vec![ExternFunctionDescriptor {
                    name: "pair".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![],
                        ret: ret.clone(),
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        };

        assert_abi_error(
            descriptor,
            binding_with_abi(
                "pair",
                RustParamAbi::Value(ExternTypeExpr::Void),
                RustReturnAbi::Value(ret),
            ),
            "structural owned resource return ABI is unsupported",
        );
    }

    #[test]
    fn rejects_wrapped_structural_resource_value_return() {
        let resource = ExternTypeExpr::Named {
            module: None,
            name: "Thing".to_string(),
            args: vec![],
        };
        let payload = ExternTypeExpr::Array {
            elem: Box::new(resource),
            len: 2,
        };
        let ret = ExternTypeExpr::Option(Box::new(payload.clone()));
        let descriptor = ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![ExternTypeDescriptor {
                    name: "Thing".to_string(),
                    doc: None,
                    rep: ExternRep::Shared,
                    layout: None,
                    materialization: None,
                    owns_heap_edges: None,
                    fields: vec![],
                    variants: vec![],
                    init: None,
                    methods: vec![],
                    statics: vec![],
                    operators: vec![],
                }],
                functions: vec![ExternFunctionDescriptor {
                    name: "maybe".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![],
                        ret,
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        };

        assert_abi_error(
            descriptor,
            binding_with_abi(
                "maybe",
                RustParamAbi::Value(ExternTypeExpr::Void),
                RustReturnAbi::Option(Box::new(RustReturnAbi::Value(payload))),
            ),
            "structural owned resource return ABI is unsupported",
        );
    }
    #[test]
    fn rejects_owned_shared_resource_param() {
        let resource = shared_resource();
        assert_abi_error(
            shared_resource_descriptor(
                "use_owned",
                vec![value_param(resource.clone())],
                ExternTypeExpr::Void,
            ),
            void_binding("use_owned", RustParamAbi::OwnedNamed(resource)),
            "shared resource parameters must use top-level, Option, or Result AnvRef",
        );
    }

    #[test]
    fn rejects_shared_resource_mutable_place() {
        let resource = shared_resource();
        assert_abi_error(
            shared_resource_descriptor(
                "edit",
                vec![ExternParam {
                    name: Some("value".to_string()),
                    ty: resource.clone(),
                    flow: ParamFlow::MutBorrow,
                    escape: CallbackEscape::NonEscaping,
                }],
                ExternTypeExpr::Void,
            ),
            void_binding("edit", RustParamAbi::MutPlace(resource)),
            "mutable-place ABI is unsupported for shared resources",
        );
    }

    #[test]
    fn rejects_structural_resource_value_param() {
        let resource = shared_resource();
        let tuple = ExternTypeExpr::Tuple(vec![resource]);
        assert_abi_error(
            shared_resource_descriptor(
                "use_tuple",
                vec![value_param(tuple.clone())],
                ExternTypeExpr::Void,
            ),
            void_binding("use_tuple", RustParamAbi::Value(tuple)),
            "shared resource parameters must use top-level, Option, or Result AnvRef",
        );
    }

    #[test]
    fn rejects_owned_shared_resource_operator_rhs() {
        let resource = ExternTypeExpr::Named {
            module: Some(module()),
            name: "Thing".to_string(),
            args: vec![],
        };
        let op = ExternOperator::Binary {
            op: BinaryOp::Add,
            self_on_right: false,
        };
        let descriptor = shared_resource_operator_descriptor(op, resource.clone());
        let mut binding = binding("add");
        binding.key.target = ExternBindingTarget::Member(ExternMemberKey {
            owner: ExternTypeKey {
                module: module(),
                name: "Thing".to_string(),
            },
            selector: ExternMemberSelector::Operator(op),
        });
        binding.abi.params = vec![
            RustParamAbi::Borrow(resource.clone()),
            RustParamAbi::OwnedNamed(resource.clone()),
        ];
        binding.abi.ret = RustReturnAbi::OwnedNamed(resource);

        assert_abi_error(
            descriptor,
            binding,
            "shared resource parameters must use top-level, Option, or Result AnvRef",
        );
    }
    #[test]
    fn rejects_presence_init_without_init_field_abi() {
        assert_abi_error(
            presence_init_descriptor(),
            presence_init_binding(RustParamAbi::Value(ExternTypeExpr::Int)),
            "init field ABI mismatch",
        );
    }

    #[test]
    fn rejects_nested_init_field_abi() {
        let nested_outer = RustParamAbi::Option(Box::new(RustParamAbi::InitField(Box::new(
            RustParamAbi::Value(ExternTypeExpr::Int),
        ))));
        assert_abi_error(
            presence_init_descriptor(),
            presence_init_binding(nested_outer),
            "init field ABI mismatch",
        );

        let nested_inner = RustParamAbi::InitField(Box::new(RustParamAbi::Option(Box::new(
            RustParamAbi::InitField(Box::new(RustParamAbi::Value(ExternTypeExpr::Int))),
        ))));
        assert_abi_error(
            presence_init_descriptor(),
            presence_init_binding(nested_inner),
            "init field ABI mismatch",
        );
    }
    #[test]
    fn rejects_init_field_abi_outside_presence_init() {
        assert_abi_error(
            descriptor(),
            binding_with_abi(
                "ping",
                RustParamAbi::InitField(Box::new(RustParamAbi::Value(ExternTypeExpr::Int))),
                RustReturnAbi::Value(ExternTypeExpr::Bool),
            ),
            "init field ABI mismatch",
        );
    }

    #[test]
    fn rejects_visible_param_abi_mismatch() {
        let mut binding = binding("ping");
        binding.abi.params[0] = RustParamAbi::Borrow(ExternTypeExpr::Int);

        let error =
            validate_rust_provider_support(&[descriptor()], &[support(binding)]).unwrap_err();

        assert!(error.contains("parameter 0 ABI mismatch"));
    }

    #[test]
    fn accepts_direct_mutable_scalar_abi() {
        assert_abi_ok(
            mutable_descriptor("bump", ExternTypeExpr::Int),
            void_binding("bump", RustParamAbi::MutBorrow(ExternTypeExpr::Int)),
        );
    }
    #[test]
    fn rejects_place_aware_abi_mismatches() {
        let cases = vec![
            (
                descriptor(),
                binding_with_abi(
                    "ping",
                    RustParamAbi::MutPlace(ExternTypeExpr::Int),
                    RustReturnAbi::Value(ExternTypeExpr::Bool),
                ),
            ),
            (
                param_descriptor(
                    "peek",
                    ExternTypeExpr::Int,
                    ParamFlow::Borrow,
                    ExternTypeExpr::Void,
                ),
                void_binding("peek", RustParamAbi::MutPlace(ExternTypeExpr::Int)),
            ),
            (
                mutable_descriptor("bump", ExternTypeExpr::Int),
                void_binding("bump", RustParamAbi::MutPlace(ExternTypeExpr::Float)),
            ),
        ];
        for (descriptor, binding) in cases {
            assert_abi_error(descriptor, binding, "parameter 0 ABI mismatch");
        }
    }

    #[test]
    fn validates_mutable_collection_abis() {
        let cases = [
            (
                "filter",
                ExternTypeExpr::List(Box::new(ExternTypeExpr::Int)),
            ),
            (
                "filter_map",
                ExternTypeExpr::Map(
                    Box::new(ExternTypeExpr::String),
                    Box::new(ExternTypeExpr::Int),
                ),
            ),
        ];
        for (name, ty) in cases {
            for abi in [
                RustParamAbi::MutBorrow(ty.clone()),
                RustParamAbi::MutPlace(ty.clone()),
            ] {
                assert_abi_error(
                    mutable_descriptor(name, ty.clone()),
                    void_binding(name, abi),
                    "direct mutable collection ABI is unsupported",
                );
            }
        }
    }
    #[test]
    fn accepts_recursive_direct_collection_carriers() {
        fn int_list() -> ExternTypeExpr {
            ExternTypeExpr::List(Box::new(ExternTypeExpr::Int))
        }

        assert_abi_ok(
            param_descriptor(
                "visible_result_list",
                ExternTypeExpr::List(Box::new(ExternTypeExpr::Result(
                    Box::new(ExternTypeExpr::Int),
                    Box::new(ExternTypeExpr::String),
                ))),
                ParamFlow::Value,
                ExternTypeExpr::Void,
            ),
            void_binding(
                "visible_result_list",
                RustParamAbi::Value(ExternTypeExpr::List(Box::new(ExternTypeExpr::Result(
                    Box::new(ExternTypeExpr::Int),
                    Box::new(ExternTypeExpr::String),
                )))),
            ),
        );

        assert_abi_ok(
            param_descriptor(
                "maybe_make",
                ExternTypeExpr::Void,
                ParamFlow::Value,
                ExternTypeExpr::Option(Box::new(int_list())),
            ),
            binding_with_abi(
                "maybe_make",
                RustParamAbi::Value(ExternTypeExpr::Void),
                RustReturnAbi::Option(Box::new(RustReturnAbi::Value(int_list()))),
            ),
        );
    }

    #[test]
    fn rejects_wrapper_conversion_without_callback_wrapper() {
        fn wrapper(mut binding: RustExternBinding) -> RustExternBinding {
            binding.abi.support = RustAbiSupport::NeedsWrapperConversion;
            binding
        }

        assert_abi_error(
            descriptor(),
            wrapper(binding("ping")),
            "unsupported wrapper conversion ABI",
        );
    }

    #[test]
    fn rejects_descriptor_abi_position_violations() {
        assert_abi_error(
            param_descriptor(
                "make_slice",
                ExternTypeExpr::Void,
                ParamFlow::Value,
                ExternTypeExpr::Slice(Box::new(ExternTypeExpr::Int)),
            ),
            binding_with_abi(
                "make_slice",
                RustParamAbi::Value(ExternTypeExpr::Void),
                RustReturnAbi::Value(ExternTypeExpr::Slice(Box::new(ExternTypeExpr::Int))),
            ),
            "return descriptor ABI violation: SliceOutsideParam",
        );

        let nested = ExternCallbackSignature {
            params: vec![],
            ret: Box::new(ExternTypeExpr::Void),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
                thread: CallbackThread::SameThread,
            },
        };
        let callback = ExternCallbackSignature {
            params: vec![ExternCallbackParam {
                ty: ExternTypeExpr::Callback(nested.clone()),
                escape: CallbackEscape::NonEscaping,
            }],
            ret: Box::new(ExternTypeExpr::Void),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
                thread: CallbackThread::SameThread,
            },
        };
        assert_abi_error(
            callback_descriptor(
                "with_callback",
                callback.clone(),
                CallbackEscape::NonEscaping,
            ),
            scoped_lambda_binding("with_callback", callback),
            "CallbackNested",
        );
    }

    #[test]
    fn rejects_unsupported_abi_metadata() {
        let result = ExternTypeExpr::Result(
            Box::new(ExternTypeExpr::Int),
            Box::new(ExternTypeExpr::String),
        );
        let result_param = RustParamAbi::Result(
            Box::new(RustParamAbi::Value(ExternTypeExpr::Int)),
            Box::new(RustParamAbi::Value(ExternTypeExpr::String)),
        );
        let result_ret = RustReturnAbi::Result(
            Box::new(RustReturnAbi::Value(ExternTypeExpr::Int)),
            Box::new(RustReturnAbi::Value(ExternTypeExpr::String)),
        );
        let mut result_binding = binding_with_abi("visible_result", result_param, result_ret);
        result_binding.abi.support = RustAbiSupport::Unsupported;
        assert_abi_error(
            param_descriptor(
                "visible_result",
                result,
                ParamFlow::Value,
                ExternTypeExpr::Result(
                    Box::new(ExternTypeExpr::Int),
                    Box::new(ExternTypeExpr::String),
                ),
            ),
            result_binding,
            "unsupported native ABI metadata",
        );
    }
    #[test]
    fn rejects_wrapped_values_as_bare_rust_values() {
        assert_abi_error(
            param_descriptor(
                "maybe",
                ExternTypeExpr::Option(Box::new(ExternTypeExpr::Int)),
                ParamFlow::Value,
                ExternTypeExpr::Void,
            ),
            void_binding(
                "maybe",
                RustParamAbi::Value(ExternTypeExpr::Option(Box::new(ExternTypeExpr::Int))),
            ),
            "parameter 0 ABI mismatch",
        );
    }
    #[test]
    fn callback_wrapper_param_matching_is_role_owned() {
        let scoped = callback_signature(vec![], ExternTypeExpr::Void);
        let escaping = callback_signature_with_escape(
            vec![ExternCallbackParam {
                ty: ExternTypeExpr::Int,
                escape: CallbackEscape::NonEscaping,
            }],
            ExternTypeExpr::Bool,
            CallbackEscape::Escaping,
        );
        let cases = [
            (
                RustParamAbi::ScopedLambda(scoped.clone()),
                ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Callback(scoped.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
                true,
            ),
            (
                RustParamAbi::ScopedLambda(escaping.clone()),
                ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Callback(escaping.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::Escaping,
                },
                false,
            ),
            (
                RustParamAbi::EscapingLambda(escaping.clone()),
                ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Callback(escaping.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::Escaping,
                },
                true,
            ),
            (
                RustParamAbi::AnvCallback(escaping.clone()),
                ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Callback(escaping.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::Escaping,
                },
                true,
            ),
            (
                RustParamAbi::EscapingLambda(scoped.clone()),
                ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Callback(scoped),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
                false,
            ),
        ];

        for (abi, param, expected) in cases {
            assert_eq!(abi.callback_wrapper_matches_param(&param), expected);
        }
    }

    #[test]
    fn accepts_static_method_escaping_lambda_abi() {
        let callback =
            callback_signature_with_escape(vec![], ExternTypeExpr::Void, CallbackEscape::Escaping);
        let owner = ExternTypeKey {
            module: module(),
            name: "Host".to_string(),
        };
        let descriptor = ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![ExternTypeDescriptor {
                    name: "Host".to_string(),
                    doc: None,
                    rep: ExternRep::Shared,
                    layout: None,
                    materialization: None,
                    owns_heap_edges: None,
                    fields: vec![],
                    variants: vec![],
                    init: None,
                    methods: vec![],
                    statics: vec![ExternStaticDescriptor {
                        name: "with_callback".to_string(),
                        doc: None,
                        signature: ExternSignature {
                            params: vec![ExternParam {
                                name: Some("cb".to_string()),
                                ty: ExternTypeExpr::Callback(callback.clone()),
                                flow: ParamFlow::Value,
                                escape: CallbackEscape::Escaping,
                            }],
                            ret: ExternTypeExpr::Void,
                        },
                        effects: ExternEffects::default(),
                    }],
                    operators: vec![],
                }],
                functions: vec![],
            }],
        };
        let mut binding = escaping_lambda_binding("with_callback", callback);
        binding.key = ExternBindingKey {
            target: ExternBindingTarget::Member(ExternMemberKey {
                owner,
                selector: ExternMemberSelector::Static("with_callback".to_string()),
            }),
            operation: ExternBindingOp::Call,
        };

        assert_abi_ok(descriptor, binding);
    }

    #[test]
    fn rejects_escaping_scoped_lambda_abi() {
        let callback =
            callback_signature_with_escape(vec![], ExternTypeExpr::Void, CallbackEscape::Escaping);
        let descriptor =
            callback_descriptor("with_callback", callback.clone(), CallbackEscape::Escaping);
        let binding = scoped_lambda_binding("with_callback", callback);

        assert_abi_error(
            descriptor,
            binding,
            "scoped Lambda ABI requires non-escaping same-thread callback policy",
        );
    }

    #[test]
    fn rejects_non_escaping_escaping_lambda_abi() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = callback_descriptor(
            "with_callback",
            callback.clone(),
            CallbackEscape::NonEscaping,
        );
        let binding = escaping_lambda_binding("with_callback", callback);

        assert_abi_error(
            descriptor,
            binding,
            "escaping Lambda ABI requires escaping same-thread callback policy",
        );
    }

    #[test]
    fn rejects_callback_escape_metadata_mismatch() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor =
            callback_descriptor("with_callback", callback.clone(), CallbackEscape::Escaping);
        let binding = scoped_lambda_binding("with_callback", callback);

        assert_abi_error(descriptor, binding, "callback escape metadata mismatch");
    }

    #[test]
    fn rejects_scoped_lambda_hidden_runtime_ctx() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = callback_descriptor(
            "with_callback",
            callback.clone(),
            CallbackEscape::NonEscaping,
        );
        let mut binding = scoped_lambda_binding("with_callback", callback);
        binding.abi.ctx = RustWrapperCtx::HiddenRuntime;

        assert_abi_error(descriptor, binding, "wrapper ctx mode mismatch");
    }

    #[test]
    fn rejects_escaping_lambda_hidden_runtime_ctx() {
        let callback =
            callback_signature_with_escape(vec![], ExternTypeExpr::Void, CallbackEscape::Escaping);
        let descriptor =
            callback_descriptor("with_callback", callback.clone(), CallbackEscape::Escaping);
        let mut binding = escaping_lambda_binding("with_callback", callback);
        binding.abi.ctx = RustWrapperCtx::HiddenRuntime;

        assert_abi_error(descriptor, binding, "wrapper ctx mode mismatch");
    }

    #[test]
    fn accepts_direct_binding_without_hidden_ctx() {
        let descriptor = descriptor();
        let mut binding = binding("ping");
        binding.abi.ctx = RustWrapperCtx::None;

        assert_abi_ok(descriptor, binding);
    }

    #[test]
    fn rejects_mut_place_without_hidden_ctx() {
        let descriptor = mutable_descriptor("bump", ExternTypeExpr::Int);
        let mut binding = void_binding("bump", RustParamAbi::MutPlace(ExternTypeExpr::Int));
        binding.abi.ctx = RustWrapperCtx::None;

        assert_abi_error(descriptor, binding, "wrapper ctx mode mismatch");
    }

    #[test]
    fn rejects_direct_mutable_collection_with_scoped_lambda() {
        let list = ExternTypeExpr::List(Box::new(ExternTypeExpr::Int));
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = descriptor_with_params(
            "mixed",
            vec![
                ExternParam {
                    name: Some("items".to_string()),
                    ty: list.clone(),
                    flow: ParamFlow::MutBorrow,
                    escape: CallbackEscape::NonEscaping,
                },
                ExternParam {
                    name: Some("cb".to_string()),
                    ty: ExternTypeExpr::Callback(callback.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
            ],
            ExternTypeExpr::Void,
        );
        let mut binding = scoped_lambda_binding("mixed", callback);
        binding.abi.params.insert(0, RustParamAbi::MutBorrow(list));

        assert_abi_error(
            descriptor,
            binding,
            "callback wrapper ABI cannot be combined with borrowed or mutable-place provider parameters",
        );
    }

    #[test]
    fn rejects_borrowed_param_with_scoped_lambda() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = descriptor_with_params(
            "mixed",
            vec![
                ExternParam {
                    name: Some("text".to_string()),
                    ty: ExternTypeExpr::String,
                    flow: ParamFlow::Borrow,
                    escape: CallbackEscape::NonEscaping,
                },
                ExternParam {
                    name: Some("cb".to_string()),
                    ty: ExternTypeExpr::Callback(callback.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
            ],
            ExternTypeExpr::Void,
        );
        let mut binding = scoped_lambda_binding("mixed", callback);
        binding
            .abi
            .params
            .insert(0, RustParamAbi::Borrow(ExternTypeExpr::String));

        assert_abi_error(
            descriptor,
            binding,
            "callback wrapper ABI cannot be combined with borrowed or mutable-place provider parameters",
        );
    }

    #[test]
    fn rejects_mut_place_param_with_scoped_lambda() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = descriptor_with_params(
            "mixed",
            vec![
                ExternParam {
                    name: Some("n".to_string()),
                    ty: ExternTypeExpr::Int,
                    flow: ParamFlow::MutBorrow,
                    escape: CallbackEscape::NonEscaping,
                },
                ExternParam {
                    name: Some("cb".to_string()),
                    ty: ExternTypeExpr::Callback(callback.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
            ],
            ExternTypeExpr::Void,
        );
        let mut binding = scoped_lambda_binding("mixed", callback);
        binding
            .abi
            .params
            .insert(0, RustParamAbi::MutPlace(ExternTypeExpr::Int));

        assert_abi_error(
            descriptor,
            binding,
            "callback wrapper ABI cannot be combined with borrowed or mutable-place provider parameters",
        );
    }
    #[test]
    fn rejects_method_receiver_with_scoped_lambda() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = callback_method_descriptor(callback.clone(), CallbackEscape::NonEscaping);
        let binding = callback_method_binding(scoped_lambda_binding("mixed", callback));

        assert_abi_error(
            descriptor,
            binding,
            "scoped callback wrapper ABI cannot be combined with method receivers",
        );
    }

    #[test]
    fn rejects_method_callback_on_inline_receiver() {
        let callback =
            callback_signature_with_escape(vec![], ExternTypeExpr::Void, CallbackEscape::Escaping);
        let descriptor = callback_method_descriptor_with_rep(
            callback.clone(),
            CallbackEscape::Escaping,
            ExternRep::Inline,
        );
        let binding = callback_method_binding(escaping_lambda_binding("mixed", callback));

        assert_abi_error(
            descriptor,
            binding,
            "callback wrapper ABI method receiver must be a shared resource",
        );
    }

    #[test]
    fn rejects_method_callback_with_value_receiver_abi() {
        let callback =
            callback_signature_with_escape(vec![], ExternTypeExpr::Void, CallbackEscape::Escaping);
        let descriptor = callback_method_descriptor(callback.clone(), CallbackEscape::Escaping);
        let mut binding = callback_method_binding(escaping_lambda_binding("mixed", callback));
        binding.abi.params[0] = RustParamAbi::Value(ExternTypeExpr::Named {
            module: Some(module()),
            name: "Thing".to_string(),
            args: vec![],
        });

        assert_abi_error(
            descriptor,
            binding,
            "callback wrapper ABI method receiver must be borrowed",
        );
    }

    #[test]
    fn rejects_method_callback_with_mut_place_receiver_abi() {
        let callback =
            callback_signature_with_escape(vec![], ExternTypeExpr::Void, CallbackEscape::Escaping);
        let descriptor = callback_method_descriptor(callback.clone(), CallbackEscape::Escaping);
        let mut binding = callback_method_binding(escaping_lambda_binding("mixed", callback));
        binding.abi.params[0] = RustParamAbi::MutPlace(ExternTypeExpr::Named {
            module: Some(module()),
            name: "Thing".to_string(),
            args: vec![],
        });

        assert_abi_error(
            descriptor,
            binding,
            "callback wrapper ABI cannot use mutable-place method receivers",
        );
    }

    #[test]
    fn rejects_scoped_lambda_signature_mismatch() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor =
            callback_descriptor("with_callback", callback, CallbackEscape::NonEscaping);
        let binding = scoped_lambda_binding(
            "with_callback",
            callback_signature(
                vec![ExternCallbackParam {
                    ty: ExternTypeExpr::Int,
                    escape: CallbackEscape::NonEscaping,
                }],
                ExternTypeExpr::Void,
            ),
        );

        assert_abi_error(descriptor, binding, "scoped Lambda ABI signature mismatch");
    }

    #[test]
    fn rejects_scoped_lambda_callback_return() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = descriptor_with_param(
            "make_callback",
            ExternParam {
                name: Some("n".to_string()),
                ty: ExternTypeExpr::Int,
                flow: ParamFlow::Value,
                escape: CallbackEscape::NonEscaping,
            },
            ExternTypeExpr::Callback(callback.clone()),
        );
        let mut binding = binding("make_callback");
        binding.abi.ret = RustReturnAbi::Value(ExternTypeExpr::Callback(callback));

        assert_abi_error(
            descriptor,
            binding,
            "return descriptor ABI violation: CallbackOutsideParam",
        );
    }

    #[test]
    fn rejects_unsupported_scoped_lambda_signatures() {
        let above_max_arity = (0..=crate::CALLBACK_WRAPPER_MAX_ARITY)
            .map(|_| ExternCallbackParam {
                ty: ExternTypeExpr::Int,
                escape: CallbackEscape::NonEscaping,
            })
            .collect();
        for params in [
            vec![ExternCallbackParam {
                ty: ExternTypeExpr::String,
                escape: CallbackEscape::NonEscaping,
            }],
            vec![ExternCallbackParam {
                ty: ExternTypeExpr::List(Box::new(ExternTypeExpr::Int)),
                escape: CallbackEscape::NonEscaping,
            }],
            above_max_arity,
        ] {
            let callback = callback_signature(params, ExternTypeExpr::Void);
            let descriptor = callback_descriptor(
                "with_callback",
                callback.clone(),
                CallbackEscape::NonEscaping,
            );
            let binding = scoped_lambda_binding("with_callback", callback);

            assert_abi_error(
                descriptor,
                binding,
                "unsupported scoped Lambda ABI signature",
            );
        }
    }

    #[test]
    fn rejects_direct_callback_abi() {
        let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
            params: vec![],
            ret: Box::new(ExternTypeExpr::Void),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
                thread: CallbackThread::SameThread,
            },
        });
        let descriptor = descriptor_with_param(
            "with_callback",
            ExternParam {
                name: Some("cb".to_string()),
                ty: callback.clone(),
                flow: ParamFlow::Value,
                escape: CallbackEscape::NonEscaping,
            },
            ExternTypeExpr::Void,
        );
        let mut binding = binding("with_callback");
        binding.abi.params = vec![RustParamAbi::Value(callback)];
        binding.abi.ret = RustReturnAbi::Void;

        let error = validate_rust_provider_support(&[descriptor], &[support(binding)]).unwrap_err();

        assert!(error.contains("direct callback ABI is unsupported"));
    }

    #[test]
    fn rejects_nested_direct_callback_abi() {
        let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
            params: vec![ExternCallbackParam {
                ty: ExternTypeExpr::Int,
                escape: CallbackEscape::NonEscaping,
            }],
            ret: Box::new(ExternTypeExpr::Void),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
                thread: CallbackThread::SameThread,
            },
        });
        let nested = ExternTypeExpr::List(Box::new(callback.clone()));
        let descriptor = descriptor_with_param(
            "with_callbacks",
            ExternParam {
                name: Some("callbacks".to_string()),
                ty: nested.clone(),
                flow: ParamFlow::Value,
                escape: CallbackEscape::NonEscaping,
            },
            ExternTypeExpr::Void,
        );
        let mut binding = binding("with_callbacks");
        binding.abi.params = vec![RustParamAbi::Value(nested)];
        binding.abi.ret = RustReturnAbi::Void;

        let error = validate_rust_provider_support(&[descriptor], &[support(binding)]).unwrap_err();

        assert!(error.contains("CallbackNested"));
    }

    fn callback_method_descriptor(
        callback: ExternCallbackSignature,
        escape: CallbackEscape,
    ) -> ProviderDescriptor {
        callback_method_descriptor_with_rep(callback, escape, ExternRep::Shared)
    }

    fn callback_method_descriptor_with_rep(
        callback: ExternCallbackSignature,
        escape: CallbackEscape,
        rep: ExternRep,
    ) -> ProviderDescriptor {
        let mut ty = shared_resource_type(vec![]);
        ty.rep = rep;
        ty.methods.push(ExternMethodDescriptor {
            name: "mixed".to_string(),
            doc: None,
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: Some("cb".to_string()),
                    ty: ExternTypeExpr::Callback(callback),
                    flow: ParamFlow::Value,
                    escape,
                }],
                ret: ExternTypeExpr::Void,
            },
            effects: ExternEffects::default(),
        });
        ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![ty],
                functions: vec![],
            }],
        }
    }

    fn callback_method_binding(mut binding: RustExternBinding) -> RustExternBinding {
        binding.key = ExternBindingKey {
            target: ExternBindingTarget::Member(ExternMemberKey {
                owner: ExternTypeKey {
                    module: module(),
                    name: "Thing".to_string(),
                },
                selector: ExternMemberSelector::Method("mixed".to_string()),
            }),
            operation: ExternBindingOp::Call,
        };
        binding.abi.params.insert(
            0,
            RustParamAbi::Borrow(ExternTypeExpr::Named {
                module: Some(module()),
                name: "Thing".to_string(),
                args: vec![],
            }),
        );
        binding
    }

    fn callback_signature(
        params: Vec<ExternCallbackParam>,
        ret: ExternTypeExpr,
    ) -> ExternCallbackSignature {
        callback_signature_with_escape(params, ret, CallbackEscape::NonEscaping)
    }

    fn callback_signature_with_escape(
        params: Vec<ExternCallbackParam>,
        ret: ExternTypeExpr,
        escape: CallbackEscape,
    ) -> ExternCallbackSignature {
        ExternCallbackSignature {
            params,
            ret: Box::new(ret),
            policy: CallbackPolicy {
                escape,
                thread: CallbackThread::SameThread,
            },
        }
    }

    fn callback_descriptor(
        name: &str,
        callback: ExternCallbackSignature,
        escape: CallbackEscape,
    ) -> ProviderDescriptor {
        descriptor_with_param(
            name,
            ExternParam {
                name: Some("cb".to_string()),
                ty: ExternTypeExpr::Callback(callback),
                flow: ParamFlow::Value,
                escape,
            },
            ExternTypeExpr::Void,
        )
    }

    fn scoped_lambda_binding(name: &str, callback: ExternCallbackSignature) -> RustExternBinding {
        callback_binding(name, RustParamAbi::ScopedLambda(callback))
    }

    fn escaping_lambda_binding(name: &str, callback: ExternCallbackSignature) -> RustExternBinding {
        callback_binding(name, RustParamAbi::EscapingLambda(callback))
    }

    fn callback_binding(name: &str, param: RustParamAbi) -> RustExternBinding {
        let mut binding = void_binding(name, param);
        binding.abi.support = RustAbiSupport::NeedsWrapperConversion;
        binding.abi.ctx = RustWrapperCtx::None;
        binding
    }

    fn provider() -> ProviderId {
        ProviderId {
            name: "test".to_string(),
        }
    }

    fn module() -> ModulePath {
        ModulePath {
            segments: vec!["test".to_string()],
        }
    }

    fn descriptor() -> ProviderDescriptor {
        descriptor_with_param(
            "ping",
            ExternParam {
                name: Some("n".to_string()),
                ty: ExternTypeExpr::Int,
                flow: ParamFlow::Value,
                escape: CallbackEscape::NonEscaping,
            },
            ExternTypeExpr::Bool,
        )
    }

    fn mutable_descriptor(name: &str, ty: ExternTypeExpr) -> ProviderDescriptor {
        param_descriptor(name, ty, ParamFlow::MutBorrow, ExternTypeExpr::Void)
    }

    fn param_descriptor(
        name: &str,
        ty: ExternTypeExpr,
        flow: ParamFlow,
        ret: ExternTypeExpr,
    ) -> ProviderDescriptor {
        if ty == ExternTypeExpr::Void && flow == ParamFlow::Value {
            return descriptor_with_params(name, vec![], ret);
        }
        descriptor_with_param(
            name,
            ExternParam {
                name: Some("n".to_string()),
                ty,
                flow,
                escape: CallbackEscape::NonEscaping,
            },
            ret,
        )
    }

    fn assert_abi_ok(descriptor: ProviderDescriptor, binding: RustExternBinding) {
        validate_rust_provider_support(&[descriptor], &[support(binding)]).unwrap();
    }

    fn assert_abi_error(descriptor: ProviderDescriptor, binding: RustExternBinding, message: &str) {
        let error = validate_rust_provider_support(&[descriptor], &[support(binding)]).unwrap_err();
        assert!(error.contains(message), "expected {message:?} in {error:?}");
    }

    fn void_binding(name: &str, abi: RustParamAbi) -> RustExternBinding {
        binding_with_abi(name, abi, RustReturnAbi::Void)
    }

    fn binding_with_abi(name: &str, abi: RustParamAbi, ret: RustReturnAbi) -> RustExternBinding {
        let mut binding = binding(name);
        binding.abi.params = if abi == RustParamAbi::Value(ExternTypeExpr::Void) {
            vec![]
        } else {
            vec![abi]
        };
        binding.abi.ret = ret;
        binding
    }

    fn descriptor_with_param(
        name: &str,
        param: ExternParam,
        ret: ExternTypeExpr,
    ) -> ProviderDescriptor {
        descriptor_with_params(name, vec![param], ret)
    }

    fn descriptor_with_params(
        name: &str,
        params: Vec<ExternParam>,
        ret: ExternTypeExpr,
    ) -> ProviderDescriptor {
        ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: name.to_string(),
                    doc: None,
                    signature: ExternSignature { params, ret },
                    effects: ExternEffects::default(),
                }],
            }],
        }
    }

    fn presence_init_descriptor() -> ProviderDescriptor {
        ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![ExternTypeDescriptor {
                    name: "Thing".to_string(),
                    doc: None,
                    rep: ExternRep::Shared,
                    layout: None,
                    materialization: None,
                    owns_heap_edges: None,
                    fields: vec![ExternFieldDescriptor {
                        name: "n".to_string(),
                        ty: ExternTypeExpr::Int,
                        computed: false,
                        readable: true,
                        writable: true,
                        get_receiver: ReceiverMode::Shared,
                        set_receiver: ReceiverMode::Mutable,
                        doc: None,
                    }],
                    variants: vec![],
                    init: Some(ExternInitDescriptor {
                        params: vec![ExternParam {
                            name: Some("n".to_string()),
                            ty: ExternTypeExpr::Int,
                            flow: ParamFlow::Value,
                            escape: CallbackEscape::NonEscaping,
                        }],
                        field_init: vec![],
                        presence_init: vec!["n".to_string()],
                        ret: ExternTypeExpr::Named {
                            module: Some(module()),
                            name: "Thing".to_string(),
                            args: vec![],
                        },
                        effects: ExternEffects::default(),
                    }),
                    methods: vec![],
                    statics: vec![],
                    operators: vec![],
                }],
                functions: vec![],
            }],
        }
    }

    fn shared_resource() -> ExternTypeExpr {
        ExternTypeExpr::Named {
            module: None,
            name: "Thing".to_string(),
            args: vec![],
        }
    }

    fn shared_resource_descriptor(
        name: &str,
        params: Vec<ExternParam>,
        ret: ExternTypeExpr,
    ) -> ProviderDescriptor {
        ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![shared_resource_type(vec![])],
                functions: vec![ExternFunctionDescriptor {
                    name: name.to_string(),
                    doc: None,
                    signature: ExternSignature { params, ret },
                    effects: ExternEffects::default(),
                }],
            }],
        }
    }

    fn shared_resource_operator_descriptor(
        op: ExternOperator,
        ret: ExternTypeExpr,
    ) -> ProviderDescriptor {
        ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![shared_resource_type(vec![ExternOperatorDescriptor {
                    op,
                    receiver: ReceiverMode::Shared,
                    signature: ExternSignature {
                        params: vec![value_param(ret.clone())],
                        ret,
                    },
                    effects: ExternEffects::default(),
                }])],
                functions: vec![],
            }],
        }
    }

    fn shared_resource_type(operators: Vec<ExternOperatorDescriptor>) -> ExternTypeDescriptor {
        ExternTypeDescriptor {
            name: "Thing".to_string(),
            doc: None,
            rep: ExternRep::Shared,
            layout: None,
            materialization: None,
            owns_heap_edges: Some(false),
            fields: vec![],
            variants: vec![],
            init: None,
            methods: vec![],
            statics: vec![],
            operators,
        }
    }

    fn presence_init_binding(abi: RustParamAbi) -> RustExternBinding {
        let mut binding = binding("Thing.init");
        binding.key.target = ExternBindingTarget::Member(ExternMemberKey {
            owner: ExternTypeKey {
                module: module(),
                name: "Thing".to_string(),
            },
            selector: ExternMemberSelector::Init,
        });
        binding.abi.params = vec![abi];
        binding.abi.ret = RustReturnAbi::OwnedNamed(ExternTypeExpr::Named {
            module: Some(module()),
            name: "Thing".to_string(),
            args: vec![],
        });
        binding
    }

    fn support(binding: RustExternBinding) -> RustProviderSupport {
        RustProviderSupport {
            package: "test".to_string(),
            provider: provider(),
            cargo: RustProviderCargo {
                manifest_key: "test".to_string(),
                package: None,
                path: None,
                features: vec![],
                default_features: true,
            },
            modules: vec![RustModuleSupport {
                module: module(),
                types: vec![],
                bindings: vec![binding],
            }],
        }
    }

    fn binding(name: &str) -> RustExternBinding {
        RustExternBinding {
            key: ExternBindingKey {
                target: ExternBindingTarget::Function(ExternFunctionKey {
                    module: module(),
                    name: name.to_string(),
                }),
                operation: ExternBindingOp::Call,
            },
            path: RustPath {
                crate_name: "test".to_string(),
                segments: vec![name.to_string()],
            },
            abi: RustExternAbi {
                params: vec![RustParamAbi::Value(ExternTypeExpr::Int)],
                ret: RustReturnAbi::Value(ExternTypeExpr::Bool),
                fallible: false,
                support: RustAbiSupport::Direct,
                ctx: RustWrapperCtx::HiddenRuntime,
            },
        }
    }
}
