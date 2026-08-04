use std::{any::type_name, collections::HashSet, fmt::Debug, path::PathBuf};

pub use anvyx_externs::{
    AbiPosition, BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread, ExternBindingKey,
    ExternBindingOp, ExternBindingTarget, ExternCallbackParam, ExternCallbackSignature,
    ExternEffects, ExternEnumVariantDescriptor, ExternEnumVariantFieldDescriptor,
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternFunctionKey, ExternInitDescriptor,
    ExternLayout, ExternMaterialization, ExternMemberKey, ExternMemberSelector,
    ExternMethodDescriptor, ExternModuleDescriptor, ExternOperator, ExternOperatorDescriptor,
    ExternParam, ExternRep, ExternSignature, ExternStaticDescriptor, ExternTypeDescriptor,
    ExternTypeExpr, ExternTypeKey, INLINE_MATERIALIZER_SYMBOL, ModulePath, ParamFlow,
    ProviderDescriptor, ProviderId, ReceiverMode, UnaryOp, effective_callback_escape,
    native_materializer_module,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionExport {
    pub descriptor: ExternFunctionDescriptor,
    pub rust: RustLocalBinding,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExport {
    rust_type_path: &'static str,
    owns_heap_edges: bool,
    inline_materialization: Option<InlineMaterializationAttestation>,
    pub descriptor: ExternTypeDescriptor,
    pub bindings: Vec<RustMemberBinding>,
}

impl TypeExport {
    #[doc(hidden)]
    pub fn copy<T: AnvyxInlineExport + 'static>(
        rust_type_path: &'static str,
        descriptor: ExternTypeDescriptor,
        bindings: Vec<RustMemberBinding>,
    ) -> Self {
        Self::with_materialization(
            rust_type_path,
            T::OWNS_ANVYX_HEAP_EDGES,
            descriptor,
            bindings,
            Some(InlineMaterializationAttestation::inline::<T>()),
        )
    }

    #[doc(hidden)]
    pub fn enumeration<T: AnvyxEnumExport + 'static>(
        rust_type_path: &'static str,
        descriptor: ExternTypeDescriptor,
        bindings: Vec<RustMemberBinding>,
    ) -> Self {
        Self::with_materialization(
            rust_type_path,
            T::OWNS_ANVYX_HEAP_EDGES,
            descriptor,
            bindings,
            Some(InlineMaterializationAttestation::enumeration::<T>()),
        )
    }

    #[doc(hidden)]
    pub fn shared<T: AnvyxRefExport>(
        rust_type_path: &'static str,
        descriptor: ExternTypeDescriptor,
        bindings: Vec<RustMemberBinding>,
    ) -> Self {
        Self::with_materialization(
            rust_type_path,
            T::OWNS_ANVYX_HEAP_EDGES,
            descriptor,
            bindings,
            None,
        )
    }

    fn with_materialization(
        rust_type_path: &'static str,
        owns_heap_edges: bool,
        descriptor: ExternTypeDescriptor,
        bindings: Vec<RustMemberBinding>,
        inline_materialization: Option<InlineMaterializationAttestation>,
    ) -> Self {
        let export = Self {
            rust_type_path,
            owns_heap_edges,
            inline_materialization,
            descriptor,
            bindings,
        };
        validate_inline_materialization(&export);
        export
    }

    pub fn rust_type_path(&self) -> &'static str {
        self.rust_type_path
    }

    pub fn inline_materialization(&self) -> Option<InlineMaterializationAttestation> {
        self.inline_materialization
    }
}

/// Typed evidence for an inline provider value's reusable materializer.
///
/// The materializer is a trusted provider boundary: it must be infallible,
/// panic-free, deterministic, non-reentrant, and called only on the Anvyx
/// runtime thread.
///
/// The evidence can only be derived from an export trait's typed materializer;
/// its native module and symbol follow the deterministic derive convention.
///
/// ```compile_fail
/// use anvyx_runtime::{ExternMaterialization, InlineMaterializationAttestation};
///
/// let _forged = InlineMaterializationAttestation {
///     mode: ExternMaterialization::Copy,
///     rust_type_path: "forged::Type",
/// };
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InlineMaterializationAttestation {
    mode: ExternMaterialization,
    rust_type_path: &'static str,
}

impl InlineMaterializationAttestation {
    fn inline<T: AnvyxInlineExport + 'static>() -> Self {
        Self::new::<T>(ExternMaterialization::Copy)
    }

    fn enumeration<T: AnvyxEnumExport + 'static>() -> Self {
        Self::new::<T>(ExternMaterialization::Materialize)
    }

    fn new<T: 'static>(mode: ExternMaterialization) -> Self {
        Self {
            mode,
            rust_type_path: type_name::<T>(),
        }
    }

    pub fn mode(self) -> ExternMaterialization {
        self.mode
    }

    pub fn rust_type_path(self) -> &'static str {
        self.rust_type_path
    }

    pub fn module(self) -> String {
        native_materializer_module(self.rust_type_path)
    }

    pub fn symbol(self) -> &'static str {
        INLINE_MATERIALIZER_SYMBOL
    }
}

pub struct TypeMemberFragment {
    pub name: String,
    pub fields: Vec<ExternFieldDescriptor>,
    pub init: Option<ExternInitDescriptor>,
    pub methods: Vec<ExternMethodDescriptor>,
    pub statics: Vec<ExternStaticDescriptor>,
    pub operators: Vec<ExternOperatorDescriptor>,
    pub bindings: Vec<RustMemberBinding>,
}

impl TypeMemberFragment {
    #[doc(hidden)]
    pub fn new(
        name: String,
        fields: Vec<ExternFieldDescriptor>,
        init: Option<ExternInitDescriptor>,
        methods: Vec<ExternMethodDescriptor>,
        statics: Vec<ExternStaticDescriptor>,
        operators: Vec<ExternOperatorDescriptor>,
        bindings: Vec<RustMemberBinding>,
    ) -> Self {
        Self {
            name,
            fields,
            init,
            methods,
            statics,
            operators,
            bindings,
        }
    }
}

pub struct TypeMemberExport {
    pub rust_type_path: &'static str,
    pub export: fn() -> TypeMemberFragment,
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
    validate_inline_materialization(&base);
    validate_type_members(&base.descriptor);
    base
}

fn validate_inline_materialization(export: &TypeExport) {
    match (
        export.descriptor.rep,
        export.descriptor.materialization,
        export.inline_materialization,
    ) {
        (ExternRep::Inline, Some(mode), Some(attestation)) => {
            assert_eq!(
                attestation.mode(),
                mode,
                "inline materialization mode mismatch"
            );
            assert_eq!(
                attestation.rust_type_path(),
                export.rust_type_path,
                "inline materialization type mismatch"
            );
        }
        (ExternRep::Inline | ExternRep::Shared, None, None) => {}
        (ExternRep::Inline, Some(_), None) => {
            panic!("inline type requires typed materialization evidence")
        }
        (ExternRep::Inline | ExternRep::Shared, None, Some(_)) => {
            panic!("type has unexpected inline materialization evidence")
        }
        (ExternRep::Shared, Some(_), _) => {
            panic!("shared type cannot define inline materialization")
        }
    }
}

fn merge_member_fragment(base: &mut TypeExport, mut members: TypeMemberFragment) {
    base.descriptor.fields.append(&mut members.fields);
    if members.init.is_some() {
        assert!(base.descriptor.init.is_none(), "duplicate extern init");
        base.descriptor.init = members.init;
    }
    base.descriptor.methods.append(&mut members.methods);
    base.descriptor.statics.append(&mut members.statics);
    base.descriptor.operators.append(&mut members.operators);
    base.bindings.append(&mut members.bindings);
}

fn retarget_members(members: &mut TypeMemberFragment, target_name: &str) {
    for field in &mut members.fields {
        retarget_type(&mut field.ty, &members.name, target_name);
    }
    if let Some(init) = &mut members.init {
        for param in &mut init.params {
            retarget_type(&mut param.ty, &members.name, target_name);
        }
        retarget_type(&mut init.ret, &members.name, target_name);
    }
    for method in &mut members.methods {
        retarget_signature(&mut method.signature, &members.name, target_name);
    }
    for static_method in &mut members.statics {
        retarget_signature(&mut static_method.signature, &members.name, target_name);
    }
    for operator in &mut members.operators {
        retarget_signature(&mut operator.signature, &members.name, target_name);
    }
    for binding in &mut members.bindings {
        retarget_abi(&mut binding.abi, &members.name, target_name);
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
        validate_inline_materialization(&self);
        let rust_type_path = self
            .inline_materialization
            .map_or(self.rust_type_path, |attestation| {
                attestation.rust_type_path()
            });
        let segments = rust_type_path
            .split("::")
            .skip(1)
            .map(str::to_string)
            .collect::<Vec<_>>();
        let materializer = self.inline_materialization.map(|attestation| {
            let mut segments = segments[..segments.len() - 1].to_vec();
            segments.push(attestation.module());
            segments.push(attestation.symbol().to_string());
            RustMaterializerBinding {
                mode: attestation.mode(),
                rust_type: RustPath {
                    crate_name: crate_name.to_string(),
                    segments: attestation
                        .rust_type_path()
                        .split("::")
                        .skip(1)
                        .map(str::to_string)
                        .collect(),
                },
                path: RustPath {
                    crate_name: crate_name.to_string(),
                    segments,
                },
            }
        });
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
            materializer,
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
    pub materializer: Option<RustMaterializerBinding>,
}

impl RustTypeBinding {
    #[doc(hidden)]
    pub fn validated_materializer(
        &self,
        mode: ExternMaterialization,
    ) -> Option<&RustMaterializerBinding> {
        let materializer = self.materializer.as_ref()?;
        if materializer.mode != mode
            || materializer.rust_type != self.path
            || self.path.segments.is_empty()
        {
            return None;
        }
        let native_type = self.path.segments.last()?;
        let mut expected = self.path.segments[..self.path.segments.len() - 1].to_vec();
        expected.push(native_materializer_module(native_type));
        expected.push(INLINE_MATERIALIZER_SYMBOL.to_string());
        (materializer.path.crate_name == self.path.crate_name
            && materializer.path.segments == expected)
            .then_some(materializer)
    }

    pub fn retarget_crate(&mut self, crate_name: &str) {
        self.path.crate_name = crate_name.to_string();
        if let Some(materializer) = &mut self.materializer {
            materializer.rust_type.crate_name = crate_name.to_string();
            materializer.path.crate_name = crate_name.to_string();
        }
    }

    pub fn retarget_prefix(&mut self, from: &[String], to: &[String]) {
        self.path.retarget_prefix(from, to);
        if let Some(materializer) = &mut self.materializer {
            materializer.rust_type.retarget_prefix(from, to);
            materializer.path.retarget_prefix(from, to);
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RustMaterializerBinding {
    pub mode: ExternMaterialization,
    pub rust_type: RustPath,
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
            validate_native_module(descriptor, module, &types, &support.cargo.manifest_key)?;
        }
    }
    validate_native_type_completeness(descriptors, supports)?;
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

fn validate_native_type_completeness(
    descriptors: &[ProviderDescriptor],
    supports: &[RustProviderSupport],
) -> Result<(), String> {
    for descriptor in descriptors {
        for module in &descriptor.modules {
            for decl in &module.types {
                let found = supports.iter().any(|support| {
                    support.provider == descriptor.provider
                        && support.modules.iter().any(|supported_module| {
                            supported_module.module == module.path
                                && supported_module.types.iter().any(|ty| {
                                    ty.key.module == module.path && ty.key.name == decl.name
                                })
                        })
                });
                if !found {
                    return Err(format!(
                        "native provider `{}` is missing type support for `{}::{}`",
                        descriptor.provider.name,
                        module.path.segments.join("::"),
                        decl.name
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
    crate_name: &str,
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
            return Err(unknown_type_support(descriptor, ty));
        };
        if ty.key.module != support.module {
            return Err(unknown_type_support(descriptor, ty));
        }
        validate_native_type(descriptor, decl, ty, crate_name)?;
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

fn validate_native_type(
    provider: &ProviderDescriptor,
    decl: &ExternTypeDescriptor,
    support: &RustTypeBinding,
    crate_name: &str,
) -> Result<(), String> {
    let type_name = format!(
        "{}::{}",
        support.key.module.segments.join("::"),
        support.key.name
    );
    if decl.owns_heap_edges != Some(support.owns_heap_edges) {
        return Err(format!(
            "native provider `{}` has inconsistent heap-edge metadata for type `{type_name}`",
            provider.provider.name
        ));
    }
    if support.path.crate_name.is_empty()
        || support.path.crate_name != crate_name
        || support.path.segments.is_empty()
    {
        return Err(format!(
            "native provider `{}` has invalid native path for type `{type_name}`",
            provider.provider.name
        ));
    }

    match (decl.rep, decl.materialization, &support.materializer) {
        (ExternRep::Inline, Some(mode), Some(materializer)) => {
            if materializer.mode != mode {
                return Err(format!(
                    "native provider `{}` has mismatched materialization mode for type `{type_name}`",
                    provider.provider.name
                ));
            }
            if materializer.rust_type != support.path {
                return Err(format!(
                    "native provider `{}` has mismatched native path for type `{type_name}`",
                    provider.provider.name
                ));
            }
            let native_type = materializer
                .rust_type
                .segments
                .last()
                .expect("matched validated non-empty native type path");
            let mut expected = support.path.segments[..support.path.segments.len() - 1].to_vec();
            expected.push(native_materializer_module(native_type));
            expected.push(INLINE_MATERIALIZER_SYMBOL.to_string());
            let valid_path = materializer.path.crate_name == support.path.crate_name
                && materializer.path.segments == expected;
            if !valid_path {
                return Err(format!(
                    "native provider `{}` has mismatched materializer symbol for type `{type_name}`",
                    provider.provider.name
                ));
            }
        }
        (ExternRep::Inline, Some(_), None) => {
            return Err(format!(
                "native provider `{}` is missing materializer support for type `{type_name}`",
                provider.provider.name
            ));
        }
        (ExternRep::Inline | ExternRep::Shared, None, None) => {}
        (ExternRep::Inline | ExternRep::Shared, None, Some(_)) => {
            return Err(format!(
                "native provider `{}` has extra materializer support for type `{type_name}`",
                provider.provider.name
            ));
        }
        (ExternRep::Shared, Some(_), _) => {
            return Err(format!(
                "native provider `{}` has invalid shared materialization for type `{type_name}`",
                provider.provider.name
            ));
        }
    }
    Ok(())
}

fn unknown_type_support(provider: &ProviderDescriptor, ty: &RustTypeBinding) -> String {
    format!(
        "native provider `{}` has support for unknown type `{}::{}`",
        provider.provider.name,
        ty.key.module.segments.join("::"),
        ty.key.name
    )
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

/// # Safety
/// Manual implementations must set `OWNS_ANVYX_HEAP_EDGES` correctly and point
/// `__ANVYX_MATERIALIZER` at the function exported under the deterministic native
/// materializer module and symbol. The function must also satisfy
/// [`InlineMaterializationAttestation`]'s trusted-boundary contract.
///
/// ```compile_fail
/// use anvyx_runtime::AnvyxInlineExport;
///
/// #[derive(Clone, Copy)]
/// struct Manual;
///
/// impl AnvyxInlineExport for Manual {
///     const OWNS_ANVYX_HEAP_EDGES: bool = false;
///     const __ANVYX_MATERIALIZER: fn(&Self) -> Self = |value| *value;
/// }
/// ```
pub unsafe trait AnvyxInlineExport: Copy {
    const OWNS_ANVYX_HEAP_EDGES: bool;

    #[doc(hidden)]
    const __ANVYX_MATERIALIZER: fn(&Self) -> Self;
}

/// # Safety
/// Manual impls must set `OWNS_ANVYX_HEAP_EDGES` to true when the type owns any
/// Anvyx heap edge, including hidden `Handle`, `ErasedHandle`, `AnvRef`,
/// `AnvList`, `AnvMap`, `AnvCallback`, or retained callback fields.
pub unsafe trait AnvyxRefExport {
    const OWNS_ANVYX_HEAP_EDGES: bool = false;
}

/// # Safety
/// Manual implementations must set `OWNS_ANVYX_HEAP_EDGES` correctly and point
/// `__ANVYX_MATERIALIZER` at the function exported under the deterministic native
/// materializer module and symbol. The function must also satisfy
/// [`InlineMaterializationAttestation`]'s trusted-boundary contract.
pub unsafe trait AnvyxEnumExport {
    const OWNS_ANVYX_HEAP_EDGES: bool;

    #[doc(hidden)]
    const __ANVYX_MATERIALIZER: fn(&Self) -> Self;
}
