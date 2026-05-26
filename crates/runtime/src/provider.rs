use std::{collections::HashSet, fmt, path::PathBuf};

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
        RustParamAbi::Value(ty) | RustParamAbi::Borrow(ty) | RustParamAbi::MutBorrow(ty) => {
            retarget_type(ty, source_name, target_name);
        }
        RustParamAbi::Option(inner) | RustParamAbi::List(inner) => {
            retarget_param_abi(inner, source_name, target_name);
        }
    }
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

fn retarget_type(ty: &mut ExternTypeExpr, source_name: &str, target_name: &str) {
    match ty {
        ExternTypeExpr::Named { module, name, args } => {
            if module.is_none() && name == source_name {
                *name = target_name.to_string();
            }
            for arg in args {
                retarget_type(arg, source_name, target_name);
            }
        }
        ExternTypeExpr::List(inner) | ExternTypeExpr::Option(inner) => {
            retarget_type(inner, source_name, target_name);
        }
        ExternTypeExpr::Map(key, value) => {
            retarget_type(key, source_name, target_name);
            retarget_type(value, source_name, target_name);
        }
        ExternTypeExpr::Callback(callback) => {
            for param in &mut callback.params {
                retarget_type(&mut param.ty, source_name, target_name);
            }
            retarget_type(&mut callback.ret, source_name, target_name);
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
    T: Eq + std::hash::Hash + fmt::Debug,
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
    pub receiver: Option<ReceiverMode>,
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
                if let Some(receiver) = binding.receiver {
                    let ty = ExternTypeExpr::Named {
                        module: Some(owner.module.clone()),
                        name: owner.name.clone(),
                        args: vec![],
                    };
                    let abi_receiver = match receiver {
                        ReceiverMode::Shared => RustParamAbi::Borrow(ty),
                        ReceiverMode::Mutable => RustParamAbi::MutBorrow(ty),
                        ReceiverMode::Value => RustParamAbi::Value(ty),
                    };
                    abi.params.insert(0, abi_receiver);
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
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustExternAbi {
    pub params: Vec<RustParamAbi>,
    pub ret: RustReturnAbi,
    pub needs_context: bool,
    pub fallible: bool,
    pub support: RustAbiSupport,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustAbiSupport {
    Direct,
    NeedsWrapperConversion,
    Unsupported,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum RustParamAbi {
    Value(ExternTypeExpr),
    Borrow(ExternTypeExpr),
    MutBorrow(ExternTypeExpr),
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
    if abi.needs_context {
        return Err(native_abi_error(
            descriptor,
            key,
            "unexpected context parameter",
        ));
    }
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
    for (index, (param, abi)) in signature
        .signature
        .params
        .iter()
        .zip(&abi.params)
        .enumerate()
    {
        if !param_abi_matches(param, abi) {
            return Err(native_abi_error(
                descriptor,
                key,
                &format!("parameter {index} ABI mismatch"),
            ));
        }
    }
    if !return_abi_matches(&signature.signature.ret, &abi.ret) {
        return Err(native_abi_error(descriptor, key, "return ABI mismatch"));
    }
    Ok(())
}

fn param_abi_matches(param: &ExternParam, abi: &RustParamAbi) -> bool {
    match (&param.ty, param.flow, abi) {
        (ExternTypeExpr::Option(inner), ParamFlow::Value, RustParamAbi::Option(abi))
        | (ExternTypeExpr::List(inner), ParamFlow::Value, RustParamAbi::List(abi)) => {
            param_abi_matches(&value_param((**inner).clone()), abi)
        }
        (ty, ParamFlow::Value, RustParamAbi::Value(abi_ty))
        | (ty, ParamFlow::Borrow, RustParamAbi::Borrow(abi_ty))
        | (ty, ParamFlow::MutBorrow, RustParamAbi::MutBorrow(abi_ty)) => ty == abi_ty,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeError {
    message: String,
}

impl RuntimeError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for RuntimeError {}

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
    fn runtime_error_exposes_message() {
        let err = RuntimeError::new("boom");

        assert_eq!(err.message(), "boom");
        assert_eq!(err.to_string(), "boom");
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
        ProviderDescriptor {
            provider: provider(),
            modules: vec![ExternModuleDescriptor {
                path: module(),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "ping".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![ExternParam {
                            name: Some("n".to_string()),
                            ty: ExternTypeExpr::Int,
                            flow: ParamFlow::Value,
                            escape: CallbackEscape::NonEscaping,
                        }],
                        ret: ExternTypeExpr::Bool,
                    },
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
                needs_context: false,
                fallible: false,
                support: RustAbiSupport::Direct,
            },
        }
    }
}
