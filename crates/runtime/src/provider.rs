use std::any::type_name;

pub use anvyx_externs::{
    AbiPosition, BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread, ExternBindingKey,
    ExternBindingOp, ExternBindingTarget, ExternCallbackParam, ExternCallbackSignature,
    ExternEffects, ExternEnumVariantDescriptor, ExternEnumVariantFieldDescriptor,
    ExternFieldDescriptor, ExternFunctionDescriptor, ExternFunctionKey, ExternInitDescriptor,
    ExternLayout, ExternMaterialization, ExternMemberKey, ExternMemberSelector,
    ExternMethodDescriptor, ExternModuleDescriptor, ExternOperator, ExternOperatorDescriptor,
    ExternParam, ExternRep, ExternSignature, ExternStaticDescriptor, ExternTypeDescriptor,
    ExternTypeExpr, ExternTypeKey, ModulePath, ParamFlow, ProviderDescriptor, ProviderId,
    RawModuleBindingTarget, RawProviderExport, RawProviderPackage, RawRustExternBinding,
    RawRustMaterializerBinding, RawRustModuleExport, RawRustProviderExport, RawRustSymbolPath,
    RawRustTypeArgs, RawRustTypeBinding, RawRustTypePath, ReceiverMode, RustCallContext,
    RustExternAbi, UnaryOp,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeExport {
    rust_type_path: NativeTypePath,
    owns_heap_edges: bool,
    inline_materialization: Option<InlineMaterializationAttestation>,
    pub descriptor: ExternTypeDescriptor,
    pub bindings: Vec<RustMemberBinding>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeTypePath {
    Plain(&'static str),
    ContextLifetime(&'static str),
}

impl NativeTypePath {
    fn path(&self) -> &'static str {
        match self {
            Self::Plain(path) | Self::ContextLifetime(path) => path,
        }
    }

    fn has_context_lifetime(&self) -> bool {
        matches!(self, Self::ContextLifetime(_))
    }
}

impl TypeExport {
    #[doc(hidden)]
    pub fn copy<T: AnvyxInlineExport + 'static>(
        rust_type_path: &'static str,
        descriptor: ExternTypeDescriptor,
        bindings: Vec<RustMemberBinding>,
    ) -> Self {
        Self::with_materialization(
            NativeTypePath::Plain(rust_type_path),
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
            NativeTypePath::Plain(rust_type_path),
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
            NativeTypePath::Plain(rust_type_path),
            T::OWNS_ANVYX_HEAP_EDGES,
            descriptor,
            bindings,
            None,
        )
    }

    #[doc(hidden)]
    pub fn shared_context_lifetime<T: AnvyxRefExport>(
        rust_type_path: &'static str,
        descriptor: ExternTypeDescriptor,
        bindings: Vec<RustMemberBinding>,
    ) -> Self {
        Self::with_materialization(
            NativeTypePath::ContextLifetime(rust_type_path),
            T::OWNS_ANVYX_HEAP_EDGES,
            descriptor,
            bindings,
            None,
        )
    }

    fn with_materialization(
        rust_type_path: NativeTypePath,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct InlineMaterializationAttestation {
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

    fn module(self) -> String {
        anvyx_externs::native_materializer_module(self.rust_type_path)
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

pub struct TypeMemberExport {
    pub rust_type_path: &'static str,
    pub export: fn() -> TypeMemberFragment,
}

inventory::collect!(TypeMemberExport);

pub fn merge_type_members(mut base: TypeExport) -> TypeExport {
    for item in inventory::iter::<TypeMemberExport> {
        if item.rust_type_path == base.rust_type_path.path() {
            let mut members = (item.export)();
            retarget_members(&mut members, &base.descriptor.name);
            merge_member_fragment(&mut base, members);
        }
    }
    validate_inline_materialization(&base);
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
                attestation.mode, mode,
                "inline materialization mode mismatch"
            );
            assert_eq!(
                attestation.rust_type_path,
                export.rust_type_path.path(),
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
}

fn retarget_signature(signature: &mut ExternSignature, source_name: &str, target_name: &str) {
    for param in &mut signature.params {
        retarget_type(&mut param.ty, source_name, target_name);
    }
    retarget_type(&mut signature.ret, source_name, target_name);
}

fn retarget_type(ty: &mut ExternTypeExpr, source_name: &str, target_name: &str) {
    ty.rewrite_names(&mut |module, name| {
        if module.is_none() && name.as_str() == source_name {
            *name = target_name.to_string();
        }
    });
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
    functions: Vec<FunctionExport>,
    types: Vec<TypeExport>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FunctionExport {
    descriptor: ExternFunctionDescriptor,
    symbol: String,
    abi: RustExternAbi,
}

impl ModuleExport {
    #[doc(hidden)]
    pub fn function(
        descriptor: ExternFunctionDescriptor,
        symbol: String,
        abi: RustExternAbi,
    ) -> Self {
        Self {
            functions: vec![FunctionExport {
                descriptor,
                symbol,
                abi,
            }],
            types: vec![],
        }
    }

    #[doc(hidden)]
    pub fn ty(export: TypeExport) -> Self {
        Self {
            functions: vec![],
            types: vec![export],
        }
    }

    #[doc(hidden)]
    pub fn extend(&mut self, mut other: Self) {
        self.functions.append(&mut other.functions);
        self.types.append(&mut other.types);
    }

    #[doc(hidden)]
    pub fn finalize(
        self,
        package: &mut RawProviderPackage,
        provider: ProviderId,
        path: ModulePath,
        function_prefix: &[String],
        type_prefix: Option<&[String]>,
    ) {
        let mut module = RawRustModuleExport {
            descriptor: ExternModuleDescriptor {
                path,
                types: vec![],
                functions: vec![],
            },
            types: vec![],
            bindings: vec![],
        };
        for export in self.types {
            export.finalize(&mut module, type_prefix);
        }
        for export in self.functions {
            let name = export.descriptor.name.clone();
            module.descriptor.functions.push(export.descriptor);
            let mut segments = function_prefix.to_vec();
            segments.push(export.symbol);
            module.bindings.push(RawRustExternBinding {
                target: RawModuleBindingTarget::Function(name),
                operation: ExternBindingOp::Call,
                path: RawRustSymbolPath(segments),
                abi: export.abi,
            });
        }
        if let Some(raw) = package.exports.iter_mut().find_map(|export| match export {
            RawProviderExport::Rust(raw) if raw.provider == provider => Some(raw),
            RawProviderExport::Descriptor(_) | RawProviderExport::Rust(_) => None,
        }) {
            raw.modules.push(module);
        } else {
            package
                .exports
                .push(RawProviderExport::Rust(RawRustProviderExport {
                    provider,
                    modules: vec![module],
                }));
        }
    }
}

impl TypeExport {
    fn finalize(self, module: &mut RawRustModuleExport, type_prefix: Option<&[String]>) {
        validate_inline_materialization(&self);
        let path = raw_type_path(
            self.inline_materialization.map_or_else(
                || self.rust_type_path.path(),
                |attestation| attestation.rust_type_path,
            ),
            type_prefix,
            self.rust_type_path.has_context_lifetime(),
        );
        let name = self.descriptor.name.clone();
        let materializer = self.inline_materialization.map(|attestation| {
            let mut segments = path.symbol.0.clone();
            segments.pop();
            segments.push(attestation.module());
            segments.push(anvyx_externs::INLINE_MATERIALIZER_SYMBOL.to_string());
            RawRustMaterializerBinding {
                mode: attestation.mode,
                rust_type: path.clone(),
                path: RawRustSymbolPath(segments),
            }
        });
        module.descriptor.types.push(self.descriptor);
        module.types.push(RawRustTypeBinding {
            name: name.clone(),
            path: path.clone(),
            owns_heap_edges: self.owns_heap_edges,
            materializer,
        });
        for binding in self.bindings {
            let mut segments = path.symbol.0.clone();
            segments.pop();
            segments.push(binding.module);
            segments.push(binding.symbol);
            module.bindings.push(RawRustExternBinding {
                target: RawModuleBindingTarget::Member {
                    owner: name.clone(),
                    selector: binding.selector,
                },
                operation: binding.operation,
                path: RawRustSymbolPath(segments),
                abi: binding.abi,
            });
        }
    }
}

fn raw_type_path(
    rust_type_path: &str,
    type_prefix: Option<&[String]>,
    context_lifetime: bool,
) -> RawRustTypePath {
    let mut segments = rust_type_path
        .split("::")
        .skip(1)
        .map(str::to_string)
        .collect::<Vec<_>>();
    if let Some(prefix) = type_prefix
        && let Some(name) = segments.pop()
    {
        segments = prefix.to_vec();
        segments.push(name);
    }
    RawRustTypePath {
        symbol: RawRustSymbolPath(segments),
        args: if context_lifetime {
            RawRustTypeArgs::ContextLifetime
        } else {
            RawRustTypeArgs::None
        },
    }
}

/// # Safety
/// Manual implementations must set `OWNS_ANVYX_HEAP_EDGES` correctly: it is true
/// exactly when values own Anvyx heap edges. `__ANVYX_MATERIALIZER` must be exported
/// under the deterministic native materializer module and
/// [`anvyx_externs::INLINE_MATERIALIZER_SYMBOL`]. It must be panic-free, infallible,
/// deterministic, non-reentrant into Anvyx, and called only on the Anvyx runtime
/// thread. It must preserve valid ownership of every declared Anvyx heap edge without
/// accessing or mutating the Anvyx heap except through that edge's supported copy/
/// trace operations.
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
/// Manual implementations must set `OWNS_ANVYX_HEAP_EDGES` correctly: it is true
/// exactly when values own Anvyx heap edges. `__ANVYX_MATERIALIZER` must be exported
/// under the deterministic native materializer module and
/// [`anvyx_externs::INLINE_MATERIALIZER_SYMBOL`]. It must be panic-free, infallible,
/// deterministic, non-reentrant into Anvyx, and called only on the Anvyx runtime
/// thread. It must preserve valid ownership of every declared Anvyx heap edge without
/// accessing or mutating the Anvyx heap except through that edge's supported copy/
/// trace operations.
pub unsafe trait AnvyxEnumExport {
    const OWNS_ANVYX_HEAP_EDGES: bool;

    #[doc(hidden)]
    const __ANVYX_MATERIALIZER: fn(&Self) -> Self;
}
