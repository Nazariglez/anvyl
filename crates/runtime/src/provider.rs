use std::{collections::HashSet, fmt::Debug, path::PathBuf};

pub use anvyx_externs::{
    BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread, ExternBindingKey, ExternBindingOp,
    ExternBindingTarget, ExternCallbackParam, ExternCallbackSignature, ExternEffects,
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternFunctionKey, ExternInitDescriptor,
    ExternMemberKey, ExternMemberSelector, ExternMethodDescriptor, ExternModuleDescriptor,
    ExternOperator, ExternOperatorDescriptor, ExternParam, ExternRep, ExternSignature,
    ExternStaticDescriptor, ExternTypeDescriptor, ExternTypeExpr, ExternTypeKey, ModulePath,
    ParamFlow, ProviderDescriptor, ProviderId, ReceiverMode, UnaryOp,
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
        | RustParamAbi::Borrow(ty)
        | RustParamAbi::MutBorrow(ty)
        | RustParamAbi::MutPlace(ty) => {
            retarget_type(ty, source_name, target_name);
        }
        RustParamAbi::ScopedLambda(callback) => {
            retarget_callback(callback, source_name, target_name);
        }
        RustParamAbi::Option(inner) | RustParamAbi::List(inner) => {
            retarget_param_abi(inner, source_name, target_name);
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
        RustReturnAbi::Value(ty) => retarget_type(ty, source_name, target_name),
        RustReturnAbi::Option(inner) | RustReturnAbi::List(inner) => {
            retarget_return_abi(inner, source_name, target_name);
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
        | RustParamAbi::Borrow(ty)
        | RustParamAbi::MutBorrow(ty)
        | RustParamAbi::MutPlace(ty) => qualify_owner_type(ty, owner),
        RustParamAbi::ScopedLambda(callback) => qualify_callback_owner(callback, owner),
        RustParamAbi::Option(inner) | RustParamAbi::List(inner) => {
            qualify_param_abi_owner(inner, owner);
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
        ExternTypeExpr::List(inner) | ExternTypeExpr::Option(inner) => {
            rewrite_type_names(inner, rewrite);
        }
        ExternTypeExpr::Map(key, value) => {
            rewrite_type_names(key, rewrite);
            rewrite_type_names(value, rewrite);
        }
        ExternTypeExpr::Callback(callback) => {
            for param in &mut callback.params {
                rewrite_type_names(&mut param.ty, rewrite);
            }
            rewrite_type_names(&mut callback.ret, rewrite);
        }
        ExternTypeExpr::Void
        | ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
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
    Borrow(ExternTypeExpr),
    MutBorrow(ExternTypeExpr),
    MutPlace(ExternTypeExpr),
    ScopedLambda(ExternCallbackSignature),
    Option(Box<RustParamAbi>),
    List(Box<RustParamAbi>),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustReturnAbi {
    Void,
    Value(ExternTypeExpr),
    Option(Box<RustReturnAbi>),
    List(Box<RustReturnAbi>),
}

impl RustExternAbi {
    pub fn has_scoped_lambda(&self) -> bool {
        self.params.iter().any(RustParamAbi::is_scoped_lambda)
    }

    pub fn has_collection_wrapper(&self) -> bool {
        self.params
            .iter()
            .any(RustParamAbi::contains_collection_wrapper)
            || self.ret.contains_collection_wrapper()
    }

    pub fn supported_collection_wrapper(&self) -> bool {
        self.ctx == RustWrapperCtx::HiddenRuntime
            && !self.has_scoped_lambda()
            && self.has_collection_wrapper()
            && self.params.iter().all(|param| match param {
                RustParamAbi::List(_) => param.supported_collection_wrapper(),
                RustParamAbi::Value(_)
                | RustParamAbi::Borrow(_)
                | RustParamAbi::MutBorrow(_)
                | RustParamAbi::MutPlace(_) => !param.direct_collection_abi(),
                RustParamAbi::Option(_) | RustParamAbi::ScopedLambda(_) => false,
            })
            && match &self.ret {
                RustReturnAbi::List(_) => self.ret.supported_collection_wrapper(),
                RustReturnAbi::Void | RustReturnAbi::Value(_) | RustReturnAbi::Option(_) => {
                    !self.ret.contains_collection_wrapper() && !self.ret.direct_collection_abi()
                }
            }
    }
}

impl RustParamAbi {
    pub fn is_scoped_lambda(&self) -> bool {
        matches!(self, Self::ScopedLambda(_))
    }

    pub fn contains_collection_wrapper(&self) -> bool {
        match self {
            Self::List(_) => true,
            Self::Option(inner) => inner.contains_collection_wrapper(),
            Self::Value(_)
            | Self::Borrow(_)
            | Self::MutBorrow(_)
            | Self::MutPlace(_)
            | Self::ScopedLambda(_) => false,
        }
    }

    pub fn supported_collection_wrapper(&self) -> bool {
        matches!(self, Self::List(inner) if matches!(inner.as_ref(), Self::Value(ty) if scalar_collection_wrapper_leaf(ty)))
    }

    pub fn direct_collection_abi(&self) -> bool {
        match self {
            Self::Value(ty) | Self::Borrow(ty) | Self::MutBorrow(ty) | Self::MutPlace(ty) => {
                type_contains_collection(ty)
            }
            Self::Option(inner) => inner.direct_collection_abi(),
            Self::ScopedLambda(_) | Self::List(_) => false,
        }
    }

    pub fn direct_mut_collection_abi(&self) -> bool {
        matches!(self, Self::MutBorrow(ty) | Self::MutPlace(ty) if type_contains_collection(ty))
    }

    fn contains_callback(&self) -> bool {
        match self {
            Self::Value(ty) | Self::Borrow(ty) | Self::MutBorrow(ty) | Self::MutPlace(ty) => {
                type_contains_callback(ty)
            }
            Self::ScopedLambda(_) => true,
            Self::Option(inner) | Self::List(inner) => inner.contains_callback(),
        }
    }
}

impl RustReturnAbi {
    pub fn contains_collection_wrapper(&self) -> bool {
        match self {
            Self::List(_) => true,
            Self::Option(inner) => inner.contains_collection_wrapper(),
            Self::Void | Self::Value(_) => false,
        }
    }

    pub fn supported_collection_wrapper(&self) -> bool {
        matches!(self, Self::List(inner) if matches!(inner.as_ref(), Self::Value(ty) if scalar_collection_wrapper_leaf(ty)))
    }

    pub fn direct_collection_abi(&self) -> bool {
        match self {
            Self::Value(ty) => type_contains_collection(ty),
            Self::Option(inner) => inner.direct_collection_abi(),
            Self::Void | Self::List(_) => false,
        }
    }
}

fn scalar_collection_wrapper_leaf(ty: &ExternTypeExpr) -> bool {
    matches!(
        ty,
        ExternTypeExpr::Bool | ExternTypeExpr::Int | ExternTypeExpr::Float | ExternTypeExpr::String
    )
}

#[derive(Clone)]
struct NativeSignature {
    signature: ExternSignature,
    effects: ExternEffects,
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
        for module in &support.modules {
            validate_native_module(descriptor, module)?;
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
        if ty.key.module != support.module
            || !module.types.iter().any(|decl| decl.name == ty.key.name)
        {
            return Err(format!(
                "native provider `{}` has support for unknown type `{}::{}`",
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
    }
    Ok(())
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
                    }),
                (ExternMemberSelector::Init, ExternBindingOp::Call) => {
                    let init = ty.init.as_ref()?;
                    Some(NativeSignature {
                        signature: ExternSignature {
                            params: init.params.clone(),
                            ret: ExternTypeExpr::Void,
                        },
                        effects: ExternEffects::default(),
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
    NativeSignature { signature, effects }
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
    let has_scoped_lambda = abi.has_scoped_lambda();
    validate_wrapper_ctx(descriptor, key, abi, has_scoped_lambda)?;
    validate_scoped_lambda_isolated(descriptor, key, abi, has_scoped_lambda)?;
    for (index, (param, param_abi)) in signature
        .signature
        .params
        .iter()
        .zip(&abi.params)
        .enumerate()
    {
        if !param_abi_matches(param, param_abi) {
            return Err(native_abi_error(
                descriptor,
                key,
                &format!("parameter {index} ABI mismatch"),
            ));
        }
        validate_param_callback_abi(descriptor, key, param, param_abi, abi)?;
    }
    if !return_abi_matches(&signature.signature.ret, &abi.ret) {
        return Err(native_abi_error(descriptor, key, "return ABI mismatch"));
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
        || abi.ret.direct_collection_abi()
        || (abi.support == RustAbiSupport::Direct && abi.has_collection_wrapper());
    if direct_collection {
        return Err(native_abi_error(
            descriptor,
            key,
            "direct collection ABI is unsupported",
        ));
    }
    validate_wrapper_conversion_abi(descriptor, key, abi, has_scoped_lambda)?;
    Ok(())
}

fn validate_wrapper_ctx(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustExternAbi,
    has_scoped_lambda: bool,
) -> Result<(), String> {
    let expected = if has_scoped_lambda {
        RustWrapperCtx::None
    } else {
        RustWrapperCtx::HiddenRuntime
    };
    if abi.ctx == expected {
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
    has_scoped_lambda: bool,
) -> Result<(), String> {
    match abi.support {
        RustAbiSupport::Direct | RustAbiSupport::Unsupported => Ok(()),
        RustAbiSupport::NeedsWrapperConversion
            if has_scoped_lambda || abi.supported_collection_wrapper() =>
        {
            Ok(())
        }
        RustAbiSupport::NeedsWrapperConversion => Err(native_abi_error(
            descriptor,
            key,
            "unsupported collection wrapper ABI",
        )),
    }
}

fn validate_scoped_lambda_isolated(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    abi: &RustExternAbi,
    has_scoped_lambda: bool,
) -> Result<(), String> {
    if !has_scoped_lambda {
        return Ok(());
    }
    if matches!(
        &key.target,
        ExternBindingTarget::Member(member)
            if !matches!(member.selector, ExternMemberSelector::Static(_))
    ) {
        return Err(native_abi_error(
            descriptor,
            key,
            "scoped Lambda ABI cannot be combined with method receivers",
        ));
    }
    if abi.params.iter().any(|param| {
        matches!(
            param,
            RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_) | RustParamAbi::MutPlace(_)
        )
    }) {
        return Err(native_abi_error(
            descriptor,
            key,
            "scoped Lambda ABI cannot be combined with borrowed provider parameters",
        ));
    }
    if abi.has_collection_wrapper() {
        return Err(native_abi_error(
            descriptor,
            key,
            "scoped Lambda ABI cannot be combined with collection wrapper conversion",
        ));
    }
    Ok(())
}

fn validate_param_callback_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    param: &ExternParam,
    param_abi: &RustParamAbi,
    abi: &RustExternAbi,
) -> Result<(), String> {
    match (&param.ty, param_abi) {
        (ExternTypeExpr::Callback(callback), RustParamAbi::ScopedLambda(_)) => {
            validate_scoped_lambda_abi(descriptor, key, param, callback, abi.support)
        }
        _ if param_contains_callback(param) || param_abi.contains_callback() => Err(
            native_abi_error(descriptor, key, "direct callback ABI is unsupported"),
        ),
        _ => Ok(()),
    }
}

fn validate_scoped_lambda_abi(
    descriptor: &ProviderDescriptor,
    key: &ExternBindingKey,
    param: &ExternParam,
    callback: &ExternCallbackSignature,
    support: RustAbiSupport,
) -> Result<(), String> {
    if param.flow != ParamFlow::Value
        || param.escape != CallbackEscape::NonEscaping
        || !callback.scoped_lambda_policy_supported()
    {
        return Err(native_abi_error(
            descriptor,
            key,
            "scoped Lambda ABI requires non-escaping same-thread callback policy",
        ));
    }
    if support != RustAbiSupport::NeedsWrapperConversion {
        return Err(native_abi_error(
            descriptor,
            key,
            "scoped Lambda ABI requires wrapper conversion",
        ));
    }
    if !callback.scoped_lambda_signature_supported() {
        return Err(native_abi_error(
            descriptor,
            key,
            "unsupported scoped Lambda ABI signature",
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
        ExternTypeExpr::List(inner) | ExternTypeExpr::Option(inner) => {
            type_contains_callback(inner)
        }
        ExternTypeExpr::Map(key, value) => {
            type_contains_callback(key) || type_contains_callback(value)
        }
        ExternTypeExpr::Void
        | ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Any
        | ExternTypeExpr::Named { .. } => false,
    }
}

fn type_contains_collection(ty: &ExternTypeExpr) -> bool {
    match ty {
        ExternTypeExpr::List(_) | ExternTypeExpr::Map(_, _) => true,
        ExternTypeExpr::Option(inner) => type_contains_collection(inner),
        ExternTypeExpr::Callback(callback) => {
            callback
                .params
                .iter()
                .any(|param| type_contains_collection(&param.ty))
                || type_contains_collection(&callback.ret)
        }
        ExternTypeExpr::Void
        | ExternTypeExpr::Bool
        | ExternTypeExpr::Int
        | ExternTypeExpr::Float
        | ExternTypeExpr::String
        | ExternTypeExpr::Any
        | ExternTypeExpr::Named { .. } => false,
    }
}

fn param_abi_matches(param: &ExternParam, abi: &RustParamAbi) -> bool {
    match (&param.ty, param.flow, abi) {
        (ExternTypeExpr::Option(inner), ParamFlow::Value, RustParamAbi::Option(abi))
        | (ExternTypeExpr::List(inner), ParamFlow::Value, RustParamAbi::List(abi)) => {
            param_abi_matches(&value_param((**inner).clone()), abi)
        }
        (ExternTypeExpr::Callback(callback), ParamFlow::Value, RustParamAbi::ScopedLambda(abi)) => {
            callback == abi
        }
        (ty, ParamFlow::Value, RustParamAbi::Value(abi_ty))
        | (ty, ParamFlow::Borrow, RustParamAbi::Borrow(abi_ty))
        | (
            ty,
            ParamFlow::MutBorrow,
            RustParamAbi::MutBorrow(abi_ty) | RustParamAbi::MutPlace(abi_ty),
        ) => ty == abi_ty,
        _ => false,
    }
}

fn return_abi_matches(ret: &ExternTypeExpr, abi: &RustReturnAbi) -> bool {
    match (ret, abi) {
        (ExternTypeExpr::Void, RustReturnAbi::Void) => true,
        (ExternTypeExpr::Option(inner), RustReturnAbi::Option(abi)) => {
            return_abi_matches(inner, abi)
        }
        (ExternTypeExpr::List(inner), RustReturnAbi::List(abi)) => return_abi_matches(inner, abi),
        (ty, RustReturnAbi::Value(abi_ty)) => ty == abi_ty,
        _ => false,
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

pub trait AnvyxInlineExport {}

pub trait AnvyxRefExport {}

pub trait AnvyxEnumExport {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cargo_defaults_keep_default_features_enabled() {
        let cargo = RustProviderCargo::default();

        assert!(cargo.default_features);
        assert!(cargo.features.is_empty());
        assert!(cargo.package.is_none());
        assert!(cargo.path.is_none());
    }

    #[test]
    fn validates_rust_provider_support() {
        validate_rust_provider_support(&[descriptor()], &[support(binding("ping"))]).unwrap();
    }

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
    fn accepts_place_aware_mutable_scalar_abi() {
        assert_abi_ok(
            mutable_descriptor("bump", ExternTypeExpr::Int),
            void_binding("bump", RustParamAbi::MutPlace(ExternTypeExpr::Int)),
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
    fn rejects_direct_collection_value_and_return_abis() {
        fn int_list() -> ExternTypeExpr {
            ExternTypeExpr::List(Box::new(ExternTypeExpr::Int))
        }

        fn list_param_abi() -> RustParamAbi {
            RustParamAbi::List(Box::new(RustParamAbi::Value(ExternTypeExpr::Int)))
        }

        fn list_return_abi() -> RustReturnAbi {
            RustReturnAbi::List(Box::new(RustReturnAbi::Value(ExternTypeExpr::Int)))
        }

        fn wrapper(mut binding: RustExternBinding) -> RustExternBinding {
            binding.abi.support = RustAbiSupport::NeedsWrapperConversion;
            binding
        }

        for (descriptor, binding) in [
            (
                param_descriptor("take", int_list(), ParamFlow::Value, ExternTypeExpr::Void),
                void_binding("take", RustParamAbi::Value(int_list())),
            ),
            (
                param_descriptor(
                    "borrow",
                    int_list(),
                    ParamFlow::Borrow,
                    ExternTypeExpr::Void,
                ),
                void_binding("borrow", RustParamAbi::Borrow(int_list())),
            ),
            (
                param_descriptor("make", ExternTypeExpr::Void, ParamFlow::Value, int_list()),
                binding_with_abi(
                    "make",
                    RustParamAbi::Value(ExternTypeExpr::Void),
                    RustReturnAbi::Value(int_list()),
                ),
            ),
            (
                param_descriptor(
                    "take_list",
                    int_list(),
                    ParamFlow::Value,
                    ExternTypeExpr::Void,
                ),
                void_binding("take_list", list_param_abi()),
            ),
            (
                param_descriptor(
                    "make_list",
                    ExternTypeExpr::Void,
                    ParamFlow::Value,
                    int_list(),
                ),
                binding_with_abi(
                    "make_list",
                    RustParamAbi::Value(ExternTypeExpr::Void),
                    list_return_abi(),
                ),
            ),
            (
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
            ),
        ] {
            assert_abi_error(descriptor, binding, "direct collection ABI is unsupported");
        }

        assert_abi_ok(
            param_descriptor(
                "take_list_wrapper",
                int_list(),
                ParamFlow::Value,
                ExternTypeExpr::Void,
            ),
            wrapper(void_binding("take_list_wrapper", list_param_abi())),
        );
        assert_abi_ok(
            param_descriptor(
                "make_list_wrapper",
                ExternTypeExpr::Void,
                ParamFlow::Value,
                int_list(),
            ),
            wrapper(binding_with_abi(
                "make_list_wrapper",
                RustParamAbi::Value(ExternTypeExpr::Void),
                list_return_abi(),
            )),
        );
    }

    #[test]
    fn rejects_unsupported_collection_wrapper_abis() {
        fn wrapper(mut binding: RustExternBinding) -> RustExternBinding {
            binding.abi.support = RustAbiSupport::NeedsWrapperConversion;
            binding
        }

        let nested_list = ExternTypeExpr::List(Box::new(ExternTypeExpr::List(Box::new(
            ExternTypeExpr::Int,
        ))));
        let nested_list_abi = RustParamAbi::List(Box::new(RustParamAbi::List(Box::new(
            RustParamAbi::Value(ExternTypeExpr::Int),
        ))));
        assert_abi_error(
            param_descriptor(
                "nested",
                nested_list,
                ParamFlow::Value,
                ExternTypeExpr::Void,
            ),
            wrapper(void_binding("nested", nested_list_abi)),
            "unsupported collection wrapper ABI",
        );

        let option_list = ExternTypeExpr::Option(Box::new(ExternTypeExpr::List(Box::new(
            ExternTypeExpr::Int,
        ))));
        let option_list_abi = RustReturnAbi::Option(Box::new(RustReturnAbi::List(Box::new(
            RustReturnAbi::Value(ExternTypeExpr::Int),
        ))));
        assert_abi_error(
            param_descriptor(
                "maybe_make",
                ExternTypeExpr::Void,
                ParamFlow::Value,
                option_list,
            ),
            wrapper(binding_with_abi(
                "maybe_make",
                RustParamAbi::Value(ExternTypeExpr::Void),
                option_list_abi,
            )),
            "unsupported collection wrapper ABI",
        );

        let named = ExternTypeExpr::Named {
            module: Some(module()),
            name: "Vec2".to_string(),
            args: vec![],
        };
        assert_abi_error(
            param_descriptor(
                "named",
                ExternTypeExpr::List(Box::new(named.clone())),
                ParamFlow::Value,
                ExternTypeExpr::Void,
            ),
            wrapper(void_binding(
                "named",
                RustParamAbi::List(Box::new(RustParamAbi::Value(named))),
            )),
            "unsupported collection wrapper ABI",
        );

        assert_abi_error(
            descriptor(),
            wrapper(binding("ping")),
            "unsupported collection wrapper ABI",
        );
    }

    #[test]
    fn accepts_scoped_lambda_abi() {
        let callback = callback_signature(
            vec![ExternCallbackParam {
                ty: ExternTypeExpr::Int,
                escape: CallbackEscape::NonEscaping,
            }],
            ExternTypeExpr::Bool,
        );
        let descriptor = callback_descriptor(
            "with_callback",
            callback.clone(),
            CallbackEscape::NonEscaping,
        );
        let binding = scoped_lambda_binding("with_callback", callback);

        assert_abi_ok(descriptor, binding);
    }

    #[test]
    fn rejects_escaping_scoped_lambda_abi() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
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
    fn rejects_no_hidden_ctx_without_scoped_lambda() {
        let descriptor = descriptor();
        let mut binding = binding("ping");
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
            "scoped Lambda ABI cannot be combined with borrowed provider parameters",
        );
    }

    #[test]
    fn rejects_collection_wrapper_with_scoped_lambda() {
        let list = ExternTypeExpr::List(Box::new(ExternTypeExpr::Int));
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
        let descriptor = descriptor_with_params(
            "mixed",
            vec![
                ExternParam {
                    name: Some("items".to_string()),
                    ty: list.clone(),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
                ExternParam {
                    name: Some("cb".to_string()),
                    ty: ExternTypeExpr::Callback(callback.clone()),
                    flow: ParamFlow::Value,
                    escape: CallbackEscape::NonEscaping,
                },
            ],
            list.clone(),
        );
        let mut binding = scoped_lambda_binding("mixed", callback);
        binding.abi.params.insert(
            0,
            RustParamAbi::List(Box::new(RustParamAbi::Value(ExternTypeExpr::Int))),
        );
        binding.abi.ret = RustReturnAbi::List(Box::new(RustReturnAbi::Value(ExternTypeExpr::Int)));

        assert_abi_error(
            descriptor,
            binding,
            "scoped Lambda ABI cannot be combined with collection wrapper conversion",
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
            "scoped Lambda ABI cannot be combined with borrowed provider parameters",
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
            "scoped Lambda ABI cannot be combined with borrowed provider parameters",
        );
    }

    #[test]
    fn rejects_method_receiver_with_scoped_lambda() {
        let callback = callback_signature(vec![], ExternTypeExpr::Void);
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
                    fields: vec![],
                    init: None,
                    methods: vec![ExternMethodDescriptor {
                        name: "mixed".to_string(),
                        doc: None,
                        receiver: ReceiverMode::Shared,
                        signature: ExternSignature {
                            params: vec![ExternParam {
                                name: Some("cb".to_string()),
                                ty: ExternTypeExpr::Callback(callback.clone()),
                                flow: ParamFlow::Value,
                                escape: CallbackEscape::NonEscaping,
                            }],
                            ret: ExternTypeExpr::Void,
                        },
                        effects: ExternEffects::default(),
                    }],
                    statics: vec![],
                    operators: vec![],
                }],
                functions: vec![],
            }],
        };
        let mut binding = scoped_lambda_binding("mixed", callback);
        binding.key = ExternBindingKey {
            target: ExternBindingTarget::Member(ExternMemberKey {
                owner,
                selector: ExternMemberSelector::Method("mixed".to_string()),
            }),
            operation: ExternBindingOp::Call,
        };
        binding.abi.params.insert(
            0,
            RustParamAbi::Borrow(ExternTypeExpr::Named {
                module: Some(module()),
                name: "Host".to_string(),
                args: vec![],
            }),
        );

        assert_abi_error(
            descriptor,
            binding,
            "scoped Lambda ABI cannot be combined with method receivers",
        );
    }

    #[test]
    fn rejects_string_scoped_lambda_signature() {
        let callback = callback_signature(
            vec![ExternCallbackParam {
                ty: ExternTypeExpr::String,
                escape: CallbackEscape::NonEscaping,
            }],
            ExternTypeExpr::Void,
        );
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

        assert_abi_error(descriptor, binding, "parameter 0 ABI mismatch");
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
            "native callback return ABI is unsupported",
        );
    }

    #[test]
    fn rejects_unsupported_scoped_lambda_signature() {
        let callback = callback_signature(
            vec![ExternCallbackParam {
                ty: ExternTypeExpr::List(Box::new(ExternTypeExpr::Int)),
                escape: CallbackEscape::NonEscaping,
            }],
            ExternTypeExpr::Void,
        );
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

    #[test]
    fn rejects_scoped_lambda_above_max_arity() {
        let params = (0..=crate::SCOPED_LAMBDA_MAX_ARITY)
            .map(|_| ExternCallbackParam {
                ty: ExternTypeExpr::Int,
                escape: CallbackEscape::NonEscaping,
            })
            .collect();
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

        assert!(error.contains("direct callback ABI is unsupported"));
    }

    fn callback_signature(
        params: Vec<ExternCallbackParam>,
        ret: ExternTypeExpr,
    ) -> ExternCallbackSignature {
        ExternCallbackSignature {
            params,
            ret: Box::new(ret),
            policy: CallbackPolicy {
                escape: CallbackEscape::NonEscaping,
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
        let mut binding = void_binding(name, RustParamAbi::ScopedLambda(callback));
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
        binding.abi.params = vec![abi];
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
