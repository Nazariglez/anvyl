use std::collections::HashMap;

use anvyx_externs::{
    AbiPosition, AbiTypeError, BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread,
    ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternCallbackParam,
    ExternCallbackSignature, ExternEffects, ExternMemberKey, ExternMemberSelector, ExternOperator,
    ExternParam, ExternRep, ExternSignature, ExternTypeExpr, ExternTypeKey, OperatorReturn,
    ParamFlow, ProviderCatalog, ProviderId, ProviderPackageKey, ReceiverMode, UnaryOp,
};

use crate::{
    ast::{ArrayLen, EscapeMode, FuncParam, GenericArg, Ident, ReturnSpec, Type, TypeVisitor},
    externs::{
        extern_module_path, provider_module_scope,
        raw::{RawExternModule, RawExternSite, RawExterns},
        raw_module_scope,
    },
    resolve::ModulePath,
    semantic_id::NominalId,
    span::SourceSpan,
    typecheck::{
        DeclarationIndex, GenericTypeContext, ModuleScope, NominalKey, TypeRefError,
        TypeRefResolver, nominal_type, type_closure_facts,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternTypeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternFunctionId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternFieldId(usize);

impl ExternFieldId {
    pub(crate) fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternMethodId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternStaticId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternOperatorId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternFieldRef {
    owner: ExternTypeId,
    id: ExternFieldId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternMethodRef {
    owner: ExternTypeId,
    id: ExternMethodId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternStaticRef {
    owner: ExternTypeId,
    id: ExternStaticId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternOperatorRef {
    owner: ExternTypeId,
    id: ExternOperatorId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct FunctionKey {
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TypeKey {
    pub(crate) module: ModuleScope,
    pub(crate) name: Ident,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ResolvedExternSignature {
    pub(crate) params: Vec<ResolvedExternParam>,
    pub(crate) ret: ResolvedExternTy,
}

impl ResolvedExternSignature {
    pub(crate) fn to_func_type(&self) -> Type {
        Type::func(
            self.params
                .iter()
                .map(ResolvedExternParam::to_func_param)
                .collect(),
            ReturnSpec::value(self.ret.ty.clone()),
        )
    }
}

fn callback_escape_mode(escape: CallbackEscape) -> EscapeMode {
    match escape {
        CallbackEscape::NonEscaping => EscapeMode::NonEscaping,
        CallbackEscape::Escaping => EscapeMode::Escaping,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ResolvedExternParam {
    pub(crate) name: Option<Ident>,
    pub(crate) ty: ResolvedExternTy,
    pub(crate) flow: ParamFlow,
    pub(crate) escape: EscapeMode,
}

impl ResolvedExternParam {
    fn to_func_param(&self) -> FuncParam {
        FuncParam::new(
            self.ty.ty.clone(),
            matches!(self.flow, ParamFlow::MutBorrow),
            false,
            self.escape,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ResolvedExternTy {
    pub(crate) ty: Type,
    pub(crate) abi: ExternTypeExpr,
}

impl ResolvedExternTy {
    pub(crate) fn contains_any(&self) -> bool {
        type_closure_facts(&self.ty).contains_any
    }
}

impl Default for ResolvedExternTy {
    fn default() -> Self {
        Self {
            ty: Type::Void,
            abi: ExternTypeExpr::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExternOrigin {
    Provider {
        package: ProviderPackageKey,
        provider: ProviderId,
    },
    Source,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ProviderExternTypeBinding {
    pub(crate) package: ProviderPackageKey,
    pub(crate) provider: ProviderId,
    pub(crate) key: ExternTypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternLoweringInfo {
    pub(crate) binding: Option<ProviderExternLoweringInfo>,
    pub(crate) effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ProviderExternLoweringInfo {
    pub(crate) package: ProviderPackageKey,
    pub(crate) provider: ProviderId,
    pub(crate) key: ExternBindingKey,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ExternCatalog {
    types: Vec<ExternType>,
    functions: Vec<ExternFunction>,
    functions_by_key: HashMap<FunctionKey, ExternFunctionId>,
    types_by_key: HashMap<TypeKey, ExternTypeId>,
    types_by_nominal: HashMap<NominalId, ExternTypeId>,
}

enum ExternCatalogVisit<'a> {
    Ty {
        context: ExternCatalogContext,
        ty: &'a ResolvedExternTy,
        position: AbiPosition,
        site: RawExternSite,
    },
    OperatorReturn {
        context: ExternCatalogContext,
        operator: &'a ExternOperatorDecl,
    },
}

impl ExternCatalog {
    pub(crate) fn functions(&self) -> impl Iterator<Item = &ExternFunction> {
        self.functions.iter()
    }

    pub(crate) fn functions_in_scope<'a>(
        &'a self,
        scope: &'a ModuleScope,
    ) -> impl Iterator<Item = &'a ExternFunction> {
        self.functions
            .iter()
            .filter(move |function| &function.key.module == scope)
    }

    pub(crate) fn for_each_resolved_ty(
        &self,
        mut visit: impl FnMut(&ResolvedExternTy, RawExternSite),
    ) {
        let mut visit_item = |item| {
            if let ExternCatalogVisit::Ty { ty, site, .. } = item {
                visit(ty, site);
            }
        };
        self.visit_functions(&mut visit_item);
        self.visit_types(&mut visit_item);
    }

    fn visit_functions<'a>(&'a self, visit: &mut impl FnMut(ExternCatalogVisit<'a>)) {
        for function in &self.functions {
            visit_extern_signature_with_context(
                ExternCatalogContext::function(
                    &function.origin,
                    function.key.module.clone(),
                    function.key.name,
                ),
                &function.signature,
                function.site,
                visit,
            );
        }
    }

    fn visit_types<'a>(&'a self, visit: &mut impl FnMut(ExternCatalogVisit<'a>)) {
        for ty in &self.types {
            for field in &ty.fields {
                visit(ExternCatalogVisit::Ty {
                    context: ty.context.field(field.name),
                    ty: &field.ty,
                    position: AbiPosition::Field,
                    site: field.site,
                });
            }
            for method in &ty.methods {
                visit_extern_signature_with_context(
                    ty.context.method(method.name),
                    &method.signature,
                    method.site,
                    visit,
                );
            }
            for static_method in &ty.statics {
                visit_extern_signature_with_context(
                    ty.context.static_method(static_method.name),
                    &static_method.signature,
                    static_method.site,
                    visit,
                );
            }
            if let Some(init) = &ty.init {
                visit_extern_signature_with_context(
                    ty.context.init(),
                    &init.signature,
                    ty.site,
                    visit,
                );
            }
            for variant in &ty.variants {
                for field in &variant.fields {
                    visit(ExternCatalogVisit::Ty {
                        context: ty.context.clone(),
                        ty: &field.ty,
                        position: AbiPosition::Field,
                        site: ty.site,
                    });
                }
            }
            for operator in &ty.operators {
                let context = ty.context.operator(operator.op);
                visit_extern_signature_with_context(
                    context.clone(),
                    &operator.signature,
                    operator.site,
                    visit,
                );
                visit(ExternCatalogVisit::OperatorReturn { context, operator });
            }
        }
    }

    pub(crate) fn function(&self, id: ExternFunctionId) -> &ExternFunction {
        &self.functions[id.0]
    }

    pub(crate) fn ty(&self, id: ExternTypeId) -> &ExternType {
        &self.types[id.0]
    }

    pub(crate) fn function_by_key(&self, key: &FunctionKey) -> Option<ExternFunctionId> {
        self.functions_by_key.get(key).copied()
    }

    pub(crate) fn type_by_nominal(&self, key: &NominalKey) -> Option<ExternTypeId> {
        self.types_by_nominal.get(&key.id).copied()
    }

    pub(crate) fn field_ref(&self, field_ref: ExternFieldRef) -> (&ExternType, &ExternField) {
        let ty = self.ty(field_ref.owner);
        (ty, &ty.fields[field_ref.id.0])
    }

    pub(crate) fn method_ref(&self, method_ref: ExternMethodRef) -> (&ExternType, &ExternMethod) {
        let ty = self.ty(method_ref.owner);
        (ty, &ty.methods[method_ref.id.0])
    }

    pub(crate) fn static_ref(&self, static_ref: ExternStaticRef) -> (&ExternType, &ExternStatic) {
        let ty = self.ty(static_ref.owner);
        (ty, &ty.statics[static_ref.id.0])
    }

    pub(crate) fn operator_ref(
        &self,
        operator_ref: ExternOperatorRef,
    ) -> (&ExternType, &ExternOperatorDecl) {
        let ty = self.ty(operator_ref.owner);
        (ty, &ty.operators[operator_ref.id.0])
    }

    pub(crate) fn field(
        &self,
        owner: ExternTypeId,
        name: Ident,
    ) -> Option<(ExternFieldRef, &ExternField)> {
        let ty = self.ty(owner);
        let id = ty.fields_by_name.get(&name).copied()?;
        Some((ExternFieldRef { owner, id }, &ty.fields[id.0]))
    }

    pub(crate) fn method(
        &self,
        owner: ExternTypeId,
        name: Ident,
    ) -> Option<(ExternMethodRef, &ExternMethod)> {
        let ty = self.ty(owner);
        let id = ty.methods_by_name.get(&name).copied()?;
        Some((ExternMethodRef { owner, id }, &ty.methods[id.0]))
    }

    pub(crate) fn static_method(
        &self,
        owner: ExternTypeId,
        name: Ident,
    ) -> Option<(ExternStaticRef, &ExternStatic)> {
        let ty = self.ty(owner);
        let id = ty.statics_by_name.get(&name).copied()?;
        Some((ExternStaticRef { owner, id }, &ty.statics[id.0]))
    }

    pub(crate) fn unary_operator(
        &self,
        owner: ExternTypeId,
        op: UnaryOp,
    ) -> Option<(ExternOperatorRef, &ExternOperatorDecl)> {
        self.operator(owner, ExternOperator::Unary(op))
    }

    pub(crate) fn binary_operator(
        &self,
        owner: ExternTypeId,
        op: BinaryOp,
        self_on_right: bool,
    ) -> Option<(ExternOperatorRef, &ExternOperatorDecl)> {
        self.operator(owner, ExternOperator::Binary { op, self_on_right })
    }

    fn operator(
        &self,
        owner: ExternTypeId,
        op: ExternOperator,
    ) -> Option<(ExternOperatorRef, &ExternOperatorDecl)> {
        let ty = self.ty(owner);
        let id = ty.operators_by_op.get(&op).copied()?;
        Some((ExternOperatorRef { owner, id }, &ty.operators[id.0]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternFunction {
    pub(crate) id: ExternFunctionId,
    pub(crate) key: FunctionKey,
    pub(crate) origin: ExternOrigin,
    pub(crate) lowering: ExternLoweringInfo,
    pub(crate) site: RawExternSite,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternType {
    pub(crate) key: TypeKey,
    pub(crate) nominal: NominalKey,
    pub(crate) context: ExternCatalogContext,
    pub(crate) binding: Option<ProviderExternTypeBinding>,
    pub(crate) site: RawExternSite,
    pub(crate) rep: ExternRep,
    pub(crate) layout: Option<anvyx_externs::ExternLayout>,
    pub(crate) materialization: Option<anvyx_externs::ExternMaterialization>,
    pub(crate) owns_heap_edges: Option<bool>,
    pub(crate) fields: Vec<ExternField>,
    pub(crate) variants: Vec<ExternEnumVariant>,
    pub(crate) init: Option<ExternInit>,
    pub(crate) methods: Vec<ExternMethod>,
    pub(crate) statics: Vec<ExternStatic>,
    pub(crate) operators: Vec<ExternOperatorDecl>,
    fields_by_name: HashMap<Ident, ExternFieldId>,
    methods_by_name: HashMap<Ident, ExternMethodId>,
    statics_by_name: HashMap<Ident, ExternStaticId>,
    operators_by_op: HashMap<ExternOperator, ExternOperatorId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternInit {
    pub(crate) fields: ExternInitFields,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) lowering: ExternLoweringInfo,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct ExternInitFields {
    pub(crate) required: Vec<ExternInitField>,
    pub(crate) presence: Vec<ExternInitField>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ExternInitField {
    pub(crate) field: ExternFieldId,
    pub(crate) param: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternEnumVariant {
    pub(crate) name: Ident,
    pub(crate) fields: Vec<ExternEnumVariantField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternEnumVariantField {
    pub(crate) name: Option<Ident>,
    pub(crate) ty: ResolvedExternTy,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternField {
    pub(crate) name: Ident,
    pub(crate) ty: ResolvedExternTy,
    pub(crate) computed: bool,
    pub(crate) readable: bool,
    pub(crate) writable: bool,
    pub(crate) get_receiver: ReceiverMode,
    pub(crate) set_receiver: ReceiverMode,
    pub(crate) get_lowering: ExternLoweringInfo,
    pub(crate) set_lowering: ExternLoweringInfo,
    pub(crate) site: RawExternSite,
}

impl ExternType {
    pub(crate) fn required_init_fields(
        &self,
    ) -> Option<impl Iterator<Item = (ExternInitField, &ExternField)> + '_> {
        let init = self.init.as_ref()?;
        init.backs_literal(&self.nominal).then(|| {
            init.fields.required.iter().copied().map(|init| {
                let index = init.field.0;
                (init, &self.fields[index])
            })
        })
    }

    pub(crate) fn presence_init_fields(
        &self,
    ) -> Option<impl Iterator<Item = (ExternInitField, &ExternField)> + '_> {
        let init = self.init.as_ref()?;
        init.backs_literal(&self.nominal).then(|| {
            init.fields.presence.iter().copied().map(|init| {
                let index = init.field.0;
                (init, &self.fields[index])
            })
        })
    }
}

impl ExternInit {
    fn backs_literal(&self, owner: &NominalKey) -> bool {
        init_backs_literal(&self.signature, self.lowering.effects, owner)
    }
}

fn init_backs_literal(
    signature: &ResolvedExternSignature,
    effects: ExternEffects,
    owner: &NominalKey,
) -> bool {
    signature.ret.ty == nominal_type(owner)
        && !effects.fallible
        && signature
            .params
            .iter()
            .all(|param| param.flow == ParamFlow::Value)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternMethod {
    pub(crate) name: Ident,
    pub(crate) receiver: ReceiverMode,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) lowering: ExternLoweringInfo,
    pub(crate) site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternStatic {
    pub(crate) name: Ident,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) lowering: ExternLoweringInfo,
    pub(crate) site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternOperatorDecl {
    pub(crate) op: ExternOperator,
    pub(crate) receiver: ReceiverMode,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) lowering: ExternLoweringInfo,
    pub(crate) site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternCatalogContext {
    pub(crate) origin: ExternOrigin,
    pub(crate) module: ModuleScope,
    pub(crate) item: ExternContextItem,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExternContextItem {
    Function { name: Ident },
    Type { name: Ident },
    Field { ty: Ident, field: Ident },
    Init { ty: Ident },
    Method { ty: Ident, method: Ident },
    Static { ty: Ident, method: Ident },
    Operator { ty: Ident, op: ExternOperator },
}

impl ExternCatalogContext {
    fn function(origin: &ExternOrigin, module: ModuleScope, name: Ident) -> Self {
        Self {
            origin: origin.clone(),
            module,
            item: ExternContextItem::Function { name },
        }
    }

    fn ty(origin: &ExternOrigin, module: ModuleScope, name: Ident) -> Self {
        Self {
            origin: origin.clone(),
            module,
            item: ExternContextItem::Type { name },
        }
    }

    fn field(&self, field: Ident) -> Self {
        let ty = self.type_name();
        self.with_item(ExternContextItem::Field { ty, field })
    }

    fn init(&self) -> Self {
        self.with_item(ExternContextItem::Init {
            ty: self.type_name(),
        })
    }

    fn method(&self, method: Ident) -> Self {
        self.with_item(ExternContextItem::Method {
            ty: self.type_name(),
            method,
        })
    }

    fn static_method(&self, method: Ident) -> Self {
        self.with_item(ExternContextItem::Static {
            ty: self.type_name(),
            method,
        })
    }

    fn operator(&self, op: ExternOperator) -> Self {
        self.with_item(ExternContextItem::Operator {
            ty: self.type_name(),
            op,
        })
    }

    fn with_item(&self, item: ExternContextItem) -> Self {
        Self {
            origin: self.origin.clone(),
            module: self.module.clone(),
            item,
        }
    }

    fn type_name(&self) -> Ident {
        let ExternContextItem::Type { name } = &self.item else {
            unreachable!("member context requires type context")
        };
        *name
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExternCatalogError {
    UnknownType {
        context: ExternCatalogContext,
        module: Option<ModuleScope>,
        name: Ident,
        site: RawExternSite,
    },
    PrivateType {
        context: ExternCatalogContext,
        module: ModuleScope,
        name: Ident,
        site: RawExternSite,
    },
    GenericArity {
        context: ExternCatalogContext,
        name: Ident,
        expected: usize,
        found: usize,
        site: RawExternSite,
    },
    GenericArgKindMismatch {
        context: ExternCatalogContext,
        name: Ident,
        expected: &'static str,
        site: RawExternSite,
    },
    InvalidType {
        context: ExternCatalogContext,
        ty: Type,
        reason: InvalidExternTypeReason,
        site: RawExternSite,
    },
    InvalidAbiType {
        context: ExternCatalogContext,
        position: AbiPosition,
        reason: AbiTypeError,
        site: RawExternSite,
    },
    UnknownInitField {
        context: ExternCatalogContext,
        field: Ident,
        site: RawExternSite,
    },
    InitFieldTypeMismatch {
        context: ExternCatalogContext,
        field: Ident,
        expected: Type,
        found: Type,
        site: RawExternSite,
    },
    InvalidOperatorReturn {
        context: ExternCatalogContext,
        found: Type,
        expected: OperatorReturn,
        site: RawExternSite,
    },
}

impl ExternCatalogError {
    pub(crate) fn context(&self) -> &ExternCatalogContext {
        match self {
            Self::UnknownType { context, .. }
            | Self::PrivateType { context, .. }
            | Self::GenericArity { context, .. }
            | Self::GenericArgKindMismatch { context, .. }
            | Self::InvalidType { context, .. }
            | Self::InvalidAbiType { context, .. }
            | Self::UnknownInitField { context, .. }
            | Self::InitFieldTypeMismatch { context, .. }
            | Self::InvalidOperatorReturn { context, .. } => context,
        }
    }

    pub(crate) fn span(&self) -> Option<SourceSpan> {
        match self {
            Self::UnknownType { site, .. }
            | Self::PrivateType { site, .. }
            | Self::GenericArity { site, .. }
            | Self::GenericArgKindMismatch { site, .. }
            | Self::InvalidType { site, .. }
            | Self::InvalidAbiType { site, .. }
            | Self::UnknownInitField { site, .. }
            | Self::InitFieldTypeMismatch { site, .. }
            | Self::InvalidOperatorReturn { site, .. } => site.span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InvalidExternTypeReason {
    Unresolved,
    Infer,
    UnresolvedConst,
    MissingCoreOption,
    MissingCoreResult,
    NonKeyableMapKey,
    UnsupportedFlag,
}

fn visit_extern_signature_with_context<'a>(
    context: ExternCatalogContext,
    signature: &'a ResolvedExternSignature,
    site: RawExternSite,
    visit: &mut impl FnMut(ExternCatalogVisit<'a>),
) {
    for param in &signature.params {
        visit(ExternCatalogVisit::Ty {
            context: context.clone(),
            ty: &param.ty,
            position: abi_param_position(param.flow),
            site,
        });
    }
    visit(ExternCatalogVisit::Ty {
        context,
        ty: &signature.ret,
        position: AbiPosition::Return,
        site,
    });
}

fn abi_param_position(flow: ParamFlow) -> AbiPosition {
    match flow {
        ParamFlow::Value => AbiPosition::ParamValue,
        ParamFlow::Borrow => AbiPosition::ParamBorrow,
        ParamFlow::MutBorrow => AbiPosition::ParamMutBorrow,
    }
}

fn function_lowering(
    origin: &ExternOrigin,
    key: Option<&ExternBindingKey>,
    effects: ExternEffects,
) -> ExternLoweringInfo {
    let binding = match (origin, key) {
        (ExternOrigin::Provider { package, provider }, Some(key)) => {
            Some(ProviderExternLoweringInfo {
                package: package.clone(),
                provider: provider.clone(),
                key: key.clone(),
            })
        }
        (ExternOrigin::Source, None) => None,
        _ => unreachable!("extern function origin and binding disagree"),
    };
    ExternLoweringInfo { binding, effects }
}

pub(crate) fn build_catalog(
    raw: RawExterns,
    providers: &ProviderCatalog,
    decls: &mut DeclarationIndex,
) -> Result<ExternCatalog, Vec<ExternCatalogError>> {
    CatalogBuilder::new(decls).build(raw, providers)
}

struct CatalogBuilder<'a> {
    decls: &'a mut DeclarationIndex,
    catalog: ExternCatalog,
    errors: Vec<ExternCatalogError>,
}

#[derive(Clone, Copy)]
struct ResolveCtx<'a> {
    scope: &'a ModuleScope,
    context: &'a ExternCatalogContext,
    owner: Option<&'a NominalKey>,
    site: RawExternSite,
}

impl<'a> ResolveCtx<'a> {
    fn new(
        scope: &'a ModuleScope,
        context: &'a ExternCatalogContext,
        owner: Option<&'a NominalKey>,
        site: RawExternSite,
    ) -> Self {
        Self {
            scope,
            context,
            owner,
            site,
        }
    }
}

impl<'a> CatalogBuilder<'a> {
    fn new(decls: &'a mut DeclarationIndex) -> Self {
        Self {
            decls,
            catalog: ExternCatalog::default(),
            errors: vec![],
        }
    }

    fn build(
        mut self,
        raw: RawExterns,
        providers: &ProviderCatalog,
    ) -> Result<ExternCatalog, Vec<ExternCatalogError>> {
        let modules = raw.modules;
        for module in &modules {
            self.allocate_module(module, &ExternOrigin::Source);
        }
        for (package, provider, module) in providers.modules() {
            self.allocate_provider_module(package, provider, module);
        }
        for module in &modules {
            self.resolve_module(module, &ExternOrigin::Source);
        }
        for (package, provider, module) in providers.modules() {
            let scope = provider_module_scope(package, &module.path);
            let origin = ExternOrigin::Provider {
                package: package.clone(),
                provider: provider.clone(),
            };
            for function in &module.functions {
                self.resolve_function(&scope, &origin, function, RawExternSite::default());
            }
            for ty in &module.types {
                self.resolve_type(&scope, ty, RawExternSite::default(), Some(&module.path));
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        validate_catalog(&self.catalog, self.decls)?;
        Ok(self.catalog)
    }

    fn allocate_module(&mut self, raw: &RawExternModule, origin: &ExternOrigin) {
        let scope = raw_module_scope(&raw.scope);
        for ty in &raw.types {
            self.allocate_type(&scope, origin, &ty.decl, None, ty.site);
        }
        for function in &raw.functions {
            self.allocate_function(&scope, origin, &function.decl, None, function.site);
        }
    }

    fn allocate_provider_module(
        &mut self,
        package: &ProviderPackageKey,
        provider: &ProviderId,
        module: &anvyx_externs::ExternModuleDescriptor,
    ) {
        let scope = provider_module_scope(package, &module.path);
        let origin = ExternOrigin::Provider {
            package: package.clone(),
            provider: provider.clone(),
        };
        for ty in &module.types {
            let key = ExternTypeKey {
                module: module.path.clone(),
                name: ty.name.clone(),
            };
            self.allocate_type(
                &scope,
                &origin,
                ty,
                Some(ProviderExternTypeBinding {
                    package: package.clone(),
                    provider: provider.clone(),
                    key,
                }),
                RawExternSite::default(),
            );
        }
        for function in &module.functions {
            let key = ExternBindingKey {
                target: ExternBindingTarget::Function(anvyx_externs::ExternFunctionKey {
                    module: module.path.clone(),
                    name: function.name.clone(),
                }),
                operation: ExternBindingOp::Call,
            };
            self.allocate_function(
                &scope,
                &origin,
                function,
                Some(&key),
                RawExternSite::default(),
            );
        }
    }

    fn allocate_type(
        &mut self,
        scope: &ModuleScope,
        origin: &ExternOrigin,
        ty: &anvyx_externs::ExternTypeDescriptor,
        binding: Option<ProviderExternTypeBinding>,
        site: RawExternSite,
    ) {
        let name = Ident::new(&ty.name);
        let id = ExternTypeId(self.catalog.types.len());
        let key = TypeKey {
            module: scope.clone(),
            name,
        };
        let nominal = self
            .decls
            .local_nominal_type(scope, name)
            .expect("catalog extern type missing nominal metadata");
        self.catalog.types_by_key.insert(key.clone(), id);
        self.catalog.types_by_nominal.insert(nominal.id.clone(), id);
        self.catalog.types.push(ExternType {
            key,
            nominal,
            context: ExternCatalogContext::ty(origin, scope.clone(), name),
            binding,
            site,
            rep: ty.rep,
            layout: ty.layout,
            materialization: ty.materialization,
            owns_heap_edges: ty.owns_heap_edges,
            fields: vec![],
            variants: vec![],
            init: None,
            methods: vec![],
            statics: vec![],
            operators: vec![],
            fields_by_name: HashMap::new(),
            methods_by_name: HashMap::new(),
            statics_by_name: HashMap::new(),
            operators_by_op: HashMap::new(),
        });
    }

    fn allocate_function(
        &mut self,
        scope: &ModuleScope,
        origin: &ExternOrigin,
        function: &anvyx_externs::ExternFunctionDescriptor,
        binding: Option<&ExternBindingKey>,
        site: RawExternSite,
    ) {
        let name = Ident::new(&function.name);
        let id = ExternFunctionId(self.catalog.functions.len());
        let key = FunctionKey {
            module: scope.clone(),
            name,
        };
        self.catalog.functions_by_key.insert(key.clone(), id);
        self.catalog.functions.push(ExternFunction {
            id,
            key,
            origin: origin.clone(),
            lowering: function_lowering(origin, binding, function.effects),
            site,
            signature: ResolvedExternSignature::default(),
            effects: function.effects,
        });
    }

    fn resolve_module(&mut self, raw: &RawExternModule, origin: &ExternOrigin) {
        let scope = raw_module_scope(&raw.scope);
        for raw_func in &raw.functions {
            self.resolve_function(&scope, origin, &raw_func.decl, raw_func.site);
        }
        for raw_ty in &raw.types {
            self.resolve_type(&scope, &raw_ty.decl, raw_ty.site, None);
        }
    }

    fn resolve_function(
        &mut self,
        scope: &ModuleScope,
        origin: &ExternOrigin,
        raw: &anvyx_externs::ExternFunctionDescriptor,
        site: RawExternSite,
    ) {
        let key = FunctionKey {
            module: scope.clone(),
            name: Ident::new(&raw.name),
        };
        let Some(id) = self.catalog.functions_by_key.get(&key).copied() else {
            return;
        };
        let context = ExternCatalogContext::function(origin, scope.clone(), key.name);
        let signature =
            self.resolve_signature(ResolveCtx::new(scope, &context, None, site), &raw.signature);
        self.catalog.functions[id.0].signature = signature;
    }

    fn resolve_type(
        &mut self,
        scope: &ModuleScope,
        raw: &anvyx_externs::ExternTypeDescriptor,
        site: RawExternSite,
        module: Option<&anvyx_externs::ModulePath>,
    ) {
        let key = TypeKey {
            module: scope.clone(),
            name: Ident::new(&raw.name),
        };
        let Some(type_id) = self.catalog.types_by_key.get(&key).copied() else {
            return;
        };
        let owner = self.catalog.types[type_id.0].nominal.clone();
        let binding = self.catalog.types[type_id.0].binding.clone();
        let lowering = |key: Option<ExternBindingKey>, effects| ExternLoweringInfo {
            binding: binding.as_ref().map(|binding| ProviderExternLoweringInfo {
                package: binding.package.clone(),
                provider: binding.provider.clone(),
                key: key.expect("provider extern member has lowering key"),
            }),
            effects,
        };
        let member_key = |selector, operation| {
            module.map(|module| ExternBindingKey {
                target: ExternBindingTarget::Member(ExternMemberKey {
                    owner: ExternTypeKey {
                        module: module.clone(),
                        name: raw.name.clone(),
                    },
                    selector,
                }),
                operation,
            })
        };

        for raw_field in &raw.fields {
            let id = ExternFieldId(self.catalog.types[type_id.0].fields.len());
            let name = Ident::new(&raw_field.name);
            let context = self.catalog.types[type_id.0].context.field(name);
            let ty = self.resolve_ty(
                ResolveCtx::new(scope, &context, Some(&owner), site),
                &raw_field.ty,
            );
            let ty_decl = &mut self.catalog.types[type_id.0];
            ty_decl.fields_by_name.insert(name, id);
            ty_decl.fields.push(ExternField {
                name,
                ty,
                computed: raw_field.computed,
                readable: raw_field.readable,
                writable: raw_field.writable,
                get_receiver: raw_field.get_receiver,
                set_receiver: raw_field.set_receiver,
                get_lowering: lowering(
                    member_key(
                        ExternMemberSelector::Field(raw_field.name.clone()),
                        ExternBindingOp::Get,
                    ),
                    ExternEffects::default(),
                ),
                set_lowering: lowering(
                    member_key(
                        ExternMemberSelector::Field(raw_field.name.clone()),
                        ExternBindingOp::Set,
                    ),
                    ExternEffects::default(),
                ),
                site,
            });
        }

        let variant_context = self.catalog.types[type_id.0].context.clone();
        let variants = raw
            .variants
            .iter()
            .map(|variant| ExternEnumVariant {
                name: Ident::new(&variant.name),
                fields: variant
                    .fields
                    .iter()
                    .map(|field| ExternEnumVariantField {
                        name: field.name.as_deref().map(Ident::new),
                        ty: self.resolve_ty(
                            ResolveCtx::new(scope, &variant_context, Some(&owner), site),
                            &field.ty,
                        ),
                    })
                    .collect(),
            })
            .collect();
        self.catalog.types[type_id.0].variants = variants;

        if let Some(raw_init) = &raw.init {
            let context = self.catalog.types[type_id.0].context.init();
            let signature = self.resolve_init_signature(
                ResolveCtx::new(scope, &context, Some(&owner), site),
                raw_init,
                &owner,
            );
            let backs_literal = init_backs_literal(&signature, raw_init.effects, &owner);
            let fields = if backs_literal {
                self.resolve_init_fields(type_id, raw_init, site, &signature, &context)
            } else {
                ExternInitFields::default()
            };
            self.catalog.types[type_id.0].init = Some(ExternInit {
                fields,
                signature,
                lowering: lowering(
                    member_key(ExternMemberSelector::Init, ExternBindingOp::Call),
                    raw_init.effects,
                ),
            });
        }

        for raw_method in &raw.methods {
            let id = ExternMethodId(self.catalog.types[type_id.0].methods.len());
            let name = Ident::new(&raw_method.name);
            let context = self.catalog.types[type_id.0].context.method(name);
            let signature = self.resolve_signature(
                ResolveCtx::new(scope, &context, Some(&owner), site),
                &raw_method.signature,
            );
            let ty_decl = &mut self.catalog.types[type_id.0];
            ty_decl.methods_by_name.insert(name, id);
            ty_decl.methods.push(ExternMethod {
                name,
                receiver: raw_method.receiver,
                signature,
                lowering: lowering(
                    member_key(
                        ExternMemberSelector::Method(raw_method.name.clone()),
                        ExternBindingOp::Call,
                    ),
                    raw_method.effects,
                ),
                site,
            });
        }

        for raw_static in &raw.statics {
            let id = ExternStaticId(self.catalog.types[type_id.0].statics.len());
            let name = Ident::new(&raw_static.name);
            let context = self.catalog.types[type_id.0].context.static_method(name);
            let signature = self.resolve_signature(
                ResolveCtx::new(scope, &context, Some(&owner), site),
                &raw_static.signature,
            );
            let ty = &mut self.catalog.types[type_id.0];
            ty.statics_by_name.insert(name, id);
            ty.statics.push(ExternStatic {
                name,
                signature,
                lowering: lowering(
                    member_key(
                        ExternMemberSelector::Static(raw_static.name.clone()),
                        ExternBindingOp::Call,
                    ),
                    raw_static.effects,
                ),
                site,
            });
        }
        for raw_operator in &raw.operators {
            let id = ExternOperatorId(self.catalog.types[type_id.0].operators.len());
            let context = self.catalog.types[type_id.0]
                .context
                .operator(raw_operator.op);
            let signature = self.resolve_signature(
                ResolveCtx::new(scope, &context, Some(&owner), site),
                &raw_operator.signature,
            );
            let ty = &mut self.catalog.types[type_id.0];
            ty.operators_by_op.insert(raw_operator.op, id);
            ty.operators.push(ExternOperatorDecl {
                op: raw_operator.op,
                receiver: raw_operator.receiver,
                signature,
                lowering: lowering(
                    member_key(
                        ExternMemberSelector::Operator(raw_operator.op),
                        ExternBindingOp::Call,
                    ),
                    raw_operator.effects,
                ),
                site,
            });
        }
    }

    fn resolve_init_signature(
        &mut self,
        ctx: ResolveCtx<'_>,
        raw_init: &anvyx_externs::ExternInitDescriptor,
        owner: &NominalKey,
    ) -> ResolvedExternSignature {
        let ret = if raw_init.ret == ExternTypeExpr::Void {
            ResolvedExternTy {
                ty: nominal_type(owner),
                abi: ExternTypeExpr::named(None, owner.name.to_string()),
            }
        } else {
            self.resolve_ty(ctx, &raw_init.ret)
        };
        ResolvedExternSignature {
            params: self.resolve_params(ctx, &raw_init.params),
            ret,
        }
    }

    fn resolve_init_fields(
        &mut self,
        type_id: ExternTypeId,
        raw_init: &anvyx_externs::ExternInitDescriptor,
        site: RawExternSite,
        signature: &ResolvedExternSignature,
        context: &ExternCatalogContext,
    ) -> ExternInitFields {
        let required =
            self.resolve_init_field_group(type_id, site, signature, context, &raw_init.field_init);
        let presence = self.resolve_init_field_group(
            type_id,
            site,
            signature,
            context,
            &raw_init.presence_init,
        );
        ExternInitFields { required, presence }
    }

    fn resolve_init_field_group(
        &mut self,
        type_id: ExternTypeId,
        site: RawExternSite,
        signature: &ResolvedExternSignature,
        context: &ExternCatalogContext,
        names: &[String],
    ) -> Vec<ExternInitField> {
        let ty = &self.catalog.types[type_id.0];
        let param_by_name = signature
            .params
            .iter()
            .enumerate()
            .filter_map(|(index, param)| param.name.map(|name| (name, index)))
            .collect::<HashMap<_, _>>();
        let mut fields = vec![];
        for raw_name in names {
            let name = Ident::new(raw_name);
            let Some(id) = ty.fields_by_name.get(&name).copied() else {
                self.errors.push(ExternCatalogError::UnknownInitField {
                    context: context.clone(),
                    field: name,
                    site,
                });
                continue;
            };
            let Some(param_index) = param_by_name.get(&name).copied() else {
                continue;
            };
            let param = &signature.params[param_index];
            let field_ty = &ty.fields[id.0].ty.ty;
            if param.ty.ty != *field_ty {
                self.errors.push(ExternCatalogError::InitFieldTypeMismatch {
                    context: context.clone(),
                    field: name,
                    expected: field_ty.clone(),
                    found: param.ty.ty.clone(),
                    site,
                });
            }
            fields.push(ExternInitField {
                field: id,
                param: param_index,
            });
        }
        fields
    }

    fn resolve_signature(
        &mut self,
        ctx: ResolveCtx<'_>,
        signature: &ExternSignature,
    ) -> ResolvedExternSignature {
        ResolvedExternSignature {
            params: self.resolve_params(ctx, &signature.params),
            ret: self.resolve_ty(ctx, &signature.ret),
        }
    }

    fn resolve_params(
        &mut self,
        ctx: ResolveCtx<'_>,
        params: &[ExternParam],
    ) -> Vec<ResolvedExternParam> {
        params
            .iter()
            .map(|param| {
                let mut ty = self.resolve_ty(ctx, &param.ty);
                if let ExternTypeExpr::Callback(callback) = &mut ty.abi {
                    callback.policy.escape = param.escape;
                }
                ResolvedExternParam {
                    name: param.name.as_deref().map(Ident::new),
                    ty,
                    flow: param.flow,
                    escape: callback_escape_mode(param.escape),
                }
            })
            .collect()
    }

    fn resolve_ty(&mut self, ctx: ResolveCtx<'_>, ty: &ExternTypeExpr) -> ResolvedExternTy {
        let resolved = self.resolve_ty_inner(ctx, ty);
        let abi = if matches!(ctx.context.origin, ExternOrigin::Source) {
            self.abi_from_resolved_ty(&resolved)
                .unwrap_or_else(|| ty.clone())
        } else {
            ty.clone()
        };
        ResolvedExternTy { ty: resolved, abi }
    }

    fn abi_from_resolved_ty(&self, ty: &Type) -> Option<ExternTypeExpr> {
        if let Some((_, inner)) = self.decls.semantic_option_parts(ty) {
            return Some(ExternTypeExpr::option(self.abi_from_resolved_ty(inner)?));
        }
        if let Some((_, ok, err)) = self.decls.semantic_result_parts(ty) {
            return Some(ExternTypeExpr::result(
                self.abi_from_resolved_ty(ok)?,
                self.abi_from_resolved_ty(err)?,
            ));
        }
        match ty {
            Type::Void => Some(ExternTypeExpr::Void),
            Type::Tuple(fields) if fields.is_empty() => Some(ExternTypeExpr::Unit),
            Type::Bool => Some(ExternTypeExpr::Bool),
            Type::Int => Some(ExternTypeExpr::Int),
            Type::Float => Some(ExternTypeExpr::Float),
            Type::String => Some(ExternTypeExpr::String),
            Type::Char => Some(ExternTypeExpr::Char),
            Type::Any => Some(ExternTypeExpr::Any),
            Type::List { elem } => Some(ExternTypeExpr::list(self.abi_from_resolved_ty(elem)?)),
            Type::Map { key, value } => Some(ExternTypeExpr::map(
                self.abi_from_resolved_ty(key)?,
                self.abi_from_resolved_ty(value)?,
            )),
            Type::Optional { inner } => {
                Some(ExternTypeExpr::option(self.abi_from_resolved_ty(inner)?))
            }
            Type::Slice { elem } => Some(ExternTypeExpr::slice(self.abi_from_resolved_ty(elem)?)),
            Type::Tuple(fields) => fields
                .iter()
                .map(|field| self.abi_from_resolved_ty(field))
                .collect::<Option<Vec<_>>>()
                .map(ExternTypeExpr::Tuple),
            Type::Array {
                elem,
                len: ArrayLen::Fixed(len),
            } => Some(ExternTypeExpr::array(
                self.abi_from_resolved_ty(elem)?,
                *len.value() as u64,
            )),
            Type::Func { params, ret } => Some(ExternTypeExpr::Callback(ExternCallbackSignature {
                params: params
                    .iter()
                    .map(|param| {
                        Some(ExternCallbackParam {
                            ty: self.abi_from_resolved_ty(&param.ty)?,
                            escape: match param.escape {
                                EscapeMode::NonEscaping => CallbackEscape::NonEscaping,
                                EscapeMode::Escaping => CallbackEscape::Escaping,
                            },
                        })
                    })
                    .collect::<Option<Vec<_>>>()?,
                ret: Box::new(self.abi_from_resolved_ty(&ret.ty())?),
                policy: CallbackPolicy {
                    escape: CallbackEscape::NonEscaping,
                    thread: CallbackThread::SameThread,
                },
            })),
            Type::Nominal(nominal) if nominal.const_args.is_empty() => {
                Some(ExternTypeExpr::Named {
                    module: nominal
                        .origin
                        .as_ref()
                        .and_then(crate::ast::ModuleOrigin::module_path)
                        .map(|path| anvyx_externs::ModulePath {
                            segments: path.to_vec(),
                        }),
                    name: nominal.name.to_string(),
                    args: nominal
                        .type_args
                        .iter()
                        .map(|arg| self.abi_from_resolved_ty(arg))
                        .collect::<Option<Vec<_>>>()?,
                })
            }
            _ => None,
        }
    }

    fn resolve_ty_inner(&mut self, ctx: ResolveCtx<'_>, ty: &ExternTypeExpr) -> Type {
        match ty {
            ExternTypeExpr::Void => Type::Void,
            ExternTypeExpr::Unit => Type::Tuple(vec![]),
            ExternTypeExpr::Bool => Type::Bool,
            ExternTypeExpr::Int => Type::Int,
            ExternTypeExpr::Float => Type::Float,
            ExternTypeExpr::String => Type::String,
            ExternTypeExpr::Char => Type::Char,
            ExternTypeExpr::Any => Type::Any,
            ExternTypeExpr::List(elem) => Type::List {
                elem: Box::new(self.resolve_ty(ctx, elem).ty),
            },
            ExternTypeExpr::Map(key, value) => Type::Map {
                key: Box::new(self.resolve_ty(ctx, key).ty),
                value: Box::new(self.resolve_ty(ctx, value).ty),
            },
            ExternTypeExpr::Option(inner) => {
                let inner = self.resolve_ty(ctx, inner).ty;
                match self.decls.core_option_of(inner.clone()) {
                    Some(ty) => ty,
                    None => {
                        let ty = Type::optional_syntax(inner);
                        self.errors.push(invalid_type(
                            ctx.context,
                            &ty,
                            InvalidExternTypeReason::MissingCoreOption,
                            ctx.site,
                        ));
                        ty
                    }
                }
            }
            ExternTypeExpr::Result(ok, err) => {
                let ok = self.resolve_ty(ctx, ok).ty;
                let err = self.resolve_ty(ctx, err).ty;
                match self.decls.core_result_of(ok.clone(), err.clone()) {
                    Some(ty) => ty,
                    None => {
                        let ty = Type::UnresolvedNominal {
                            qualifier: None,
                            name: Ident::new("Result"),
                            generic_args: vec![GenericArg::Type(ok), GenericArg::Type(err)],
                        };
                        self.errors.push(invalid_type(
                            ctx.context,
                            &ty,
                            InvalidExternTypeReason::MissingCoreResult,
                            ctx.site,
                        ));
                        ty
                    }
                }
            }
            ExternTypeExpr::Tuple(fields) => Type::Tuple(
                fields
                    .iter()
                    .map(|field| self.resolve_ty(ctx, field).ty)
                    .collect(),
            ),
            ExternTypeExpr::Array { elem, len } => Type::Array {
                elem: Box::new(self.resolve_ty(ctx, elem).ty),
                len: ArrayLen::fixed(usize::try_from(*len).unwrap_or(usize::MAX)),
            },
            ExternTypeExpr::Slice(elem) => Type::Slice {
                elem: Box::new(self.resolve_ty(ctx, elem).ty),
            },
            ExternTypeExpr::Callback(callback) => {
                let params = callback
                    .params
                    .iter()
                    .map(|param| {
                        FuncParam::new(
                            self.resolve_ty(ctx, &param.ty).ty,
                            false,
                            false,
                            callback_escape_mode(param.escape),
                        )
                    })
                    .collect();
                Type::Func {
                    params,
                    ret: Box::new(ReturnSpec::value(self.resolve_ty(ctx, &callback.ret).ty)),
                }
            }
            ExternTypeExpr::Named { module, name, args } => {
                self.resolve_named(ctx, module.as_ref(), name, args)
            }
        }
    }

    fn resolve_named(
        &mut self,
        ctx: ResolveCtx<'_>,
        module: Option<&anvyx_externs::ModulePath>,
        name: &str,
        args: &[ExternTypeExpr],
    ) -> Type {
        let name = Ident::new(name);
        if matches!(ctx.context.origin, ExternOrigin::Source) {
            return self.resolve_source_type_ref(ctx, module, name, args);
        }

        let Some(key) = self.resolve_provider_named(ctx.context, ctx.scope, module, name) else {
            self.errors.push(ExternCatalogError::UnknownType {
                context: ctx.context.clone(),
                module: module.map(|module| missing_type_module(ctx.context, module)),
                name,
                site: ctx.site,
            });
            return Type::UnresolvedName(name);
        };

        let args = args
            .iter()
            .map(|arg| self.resolve_ty(ctx, arg).ty)
            .collect();
        self.finalize_named(ctx.context, &key, args, ctx.site)
    }

    fn resolve_source_type_ref(
        &mut self,
        ctx: ResolveCtx<'_>,
        module: Option<&anvyx_externs::ModulePath>,
        name: Ident,
        args: &[ExternTypeExpr],
    ) -> Type {
        if module.is_none()
            && name.as_str() == "Self"
            && let Some(owner) = ctx.owner
        {
            return self.finalize_named(ctx.context, owner, vec![], ctx.site);
        }

        let qualifier = match module {
            Some(module) if module.segments.len() == 1 => Some(Ident::new(&module.segments[0])),
            Some(module) => {
                self.errors.push(ExternCatalogError::UnknownType {
                    context: ctx.context.clone(),
                    module: Some(missing_type_module(ctx.context, module)),
                    name,
                    site: ctx.site,
                });
                return Type::UnresolvedName(name);
            }
            None => None,
        };
        let generic_args = args
            .iter()
            .map(|arg| GenericArg::Type(self.resolve_ty(ctx, arg).ty))
            .collect();
        let ty = Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        };
        let result = {
            let resolver = TypeRefResolver::module_only(self.decls);
            resolver.finalize_at(
                &ctx.context.module,
                &GenericTypeContext::default(),
                &ty,
                None,
            )
        };
        match result {
            Ok(finalized) => {
                for import in finalized.used_imports {
                    self.decls.mark_import_used(Some(import));
                }
                finalized.ty
            }
            Err(error) => {
                self.decls.mark_import_used(error.import().cloned());
                self.type_ref_error(ctx.context, name, error, ctx.site)
            }
        }
    }

    fn resolve_provider_named(
        &self,
        context: &ExternCatalogContext,
        scope: &ModuleScope,
        module: Option<&anvyx_externs::ModulePath>,
        name: Ident,
    ) -> Option<NominalKey> {
        match module {
            Some(module) => match &context.origin {
                ExternOrigin::Provider { package, .. } => self
                    .decls
                    .exported_nominal_type(&provider_module_scope(package, module), name),
                ExternOrigin::Source => None,
            },
            None => self.decls.local_nominal_type(scope, name),
        }
    }

    fn type_ref_error(
        &mut self,
        context: &ExternCatalogContext,
        fallback: Ident,
        error: TypeRefError,
        site: RawExternSite,
    ) -> Type {
        match error {
            TypeRefError::GenericArity { expected, found } => {
                self.errors.push(ExternCatalogError::GenericArity {
                    context: context.clone(),
                    name: fallback,
                    expected,
                    found,
                    site,
                });
                Type::UnresolvedName(fallback)
            }
            TypeRefError::GenericArgKindMismatch { expected } => {
                self.errors
                    .push(ExternCatalogError::GenericArgKindMismatch {
                        context: context.clone(),
                        name: fallback,
                        expected,
                        site,
                    });
                Type::UnresolvedName(fallback)
            }
            TypeRefError::ExpectedIntConst { .. } => {
                self.errors
                    .push(ExternCatalogError::GenericArgKindMismatch {
                        context: context.clone(),
                        name: fallback,
                        expected: "integer const",
                        site,
                    });
                Type::UnresolvedName(fallback)
            }
            TypeRefError::Unknown {
                qualifier, name, ..
            }
            | TypeRefError::UnknownContract {
                qualifier, name, ..
            } => {
                self.errors.push(ExternCatalogError::UnknownType {
                    context: context.clone(),
                    module: qualifier.map(|qualifier| {
                        ModuleScope::Named(
                            ModulePath::new(vec![qualifier.to_string()])
                                .expect("single segment module path is valid"),
                        )
                    }),
                    name,
                    site,
                });
                Type::UnresolvedName(name)
            }
            TypeRefError::PrivateModuleMember { module, name, .. } => {
                self.errors.push(ExternCatalogError::PrivateType {
                    context: context.clone(),
                    module,
                    name,
                    site,
                });
                Type::UnresolvedName(name)
            }
            TypeRefError::AliasCycle { name }
            | TypeRefError::ContractAsType { name }
            | TypeRefError::NotContract { name }
            | TypeRefError::DuplicateContractRequirement { name }
            | TypeRefError::ConflictingContractRequirement { name } => {
                self.errors.push(ExternCatalogError::UnknownType {
                    context: context.clone(),
                    module: None,
                    name,
                    site,
                });
                Type::UnresolvedName(name)
            }
            TypeRefError::MissingCoreOption => {
                self.errors.push(ExternCatalogError::UnknownType {
                    context: context.clone(),
                    module: None,
                    name: fallback,
                    site,
                });
                Type::UnresolvedName(fallback)
            }
        }
    }

    fn finalize_named(
        &mut self,
        context: &ExternCatalogContext,
        key: &NominalKey,
        args: Vec<Type>,
        site: RawExternSite,
    ) -> Type {
        match self
            .decls
            .finalize_nominal_type_args(&context.module, key, args)
        {
            Ok(ty) => ty,
            Err(error) => self.type_ref_error(context, key.name, error, site),
        }
    }
}

fn missing_type_module(
    context: &ExternCatalogContext,
    module: &anvyx_externs::ModulePath,
) -> ModuleScope {
    match &context.origin {
        ExternOrigin::Provider { package, .. } => provider_module_scope(package, module),
        ExternOrigin::Source => ModuleScope::Named(extern_module_path(module)),
    }
}

fn validate_catalog(
    catalog: &ExternCatalog,
    decls: &DeclarationIndex,
) -> Result<(), Vec<ExternCatalogError>> {
    let mut errors = vec![];
    let mut validate = |item| match item {
        ExternCatalogVisit::Ty {
            context,
            ty,
            position,
            site,
        } => validate_ty(&context, ty, position, site, decls, &mut errors),
        ExternCatalogVisit::OperatorReturn { context, operator } => {
            validate_operator_return(&context, operator, &mut errors);
        }
    };
    catalog.visit_types(&mut validate);
    catalog.visit_functions(&mut validate);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn validate_operator_return(
    context: &ExternCatalogContext,
    operator: &ExternOperatorDecl,
    errors: &mut Vec<ExternCatalogError>,
) {
    let expected = operator.op.return_requirement();
    let valid = match expected {
        OperatorReturn::Bool => operator.signature.ret.ty == Type::Bool,
        OperatorReturn::NonVoid => operator.signature.ret.ty != Type::Void,
    };
    if !valid {
        errors.push(ExternCatalogError::InvalidOperatorReturn {
            context: context.clone(),
            found: operator.signature.ret.ty.clone(),
            expected,
            site: operator.site,
        });
    }
}

fn validate_ty(
    context: &ExternCatalogContext,
    ty: &ResolvedExternTy,
    position: AbiPosition,
    site: RawExternSite,
    decls: &DeclarationIndex,
    errors: &mut Vec<ExternCatalogError>,
) {
    validate_type_facts(context, &ty.ty, site, errors);
    validate_source_schema_flags(context, &ty.ty, site, decls, errors);
    if matches!(context.origin, ExternOrigin::Source) {
        validate_abi_type(context, &ty.abi, position, site, errors);
    }
    validate_map_keys(context, &ty.ty, site, decls, errors);
}

fn validate_abi_type(
    context: &ExternCatalogContext,
    ty: &ExternTypeExpr,
    position: AbiPosition,
    site: RawExternSite,
    errors: &mut Vec<ExternCatalogError>,
) {
    if let Err(violations) = ty.classify_abi(position) {
        errors.extend(violations.into_iter().filter_map(|violation| {
            let source_generic = matches!(context.origin, ExternOrigin::Source)
                && violation.1 == AbiTypeError::GenericNamedArgsUnsupported;
            (!source_generic).then(|| ExternCatalogError::InvalidAbiType {
                context: context.clone(),
                position: violation.0,
                reason: violation.1,
                site,
            })
        }));
    }
}

struct FlagTypeVisitor<'a> {
    decls: &'a DeclarationIndex,
}

impl TypeVisitor for FlagTypeVisitor<'_> {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        self.decls.is_flag_enum_type(ty)
    }
}

fn validate_source_schema_flags(
    context: &ExternCatalogContext,
    ty: &Type,
    site: RawExternSite,
    decls: &DeclarationIndex,
    errors: &mut Vec<ExternCatalogError>,
) {
    let mut flags = FlagTypeVisitor { decls };
    if matches!(context.origin, ExternOrigin::Source) && flags.visit_type(ty) {
        errors.push(invalid_type(
            context,
            ty,
            InvalidExternTypeReason::UnsupportedFlag,
            site,
        ));
    }
}

fn validate_type_facts(
    context: &ExternCatalogContext,
    ty: &Type,
    site: RawExternSite,
    errors: &mut Vec<ExternCatalogError>,
) {
    let facts = type_closure_facts(ty);
    if facts.first_unresolved.is_some() {
        errors.push(invalid_type(
            context,
            ty,
            InvalidExternTypeReason::Unresolved,
            site,
        ));
    }
    if facts.infer.contains_type || facts.infer.contains_return {
        errors.push(invalid_type(
            context,
            ty,
            InvalidExternTypeReason::Infer,
            site,
        ));
    }
    if facts.contains_unresolved_const {
        errors.push(invalid_type(
            context,
            ty,
            InvalidExternTypeReason::UnresolvedConst,
            site,
        ));
    }
}

fn validate_map_keys(
    context: &ExternCatalogContext,
    ty: &Type,
    site: RawExternSite,
    decls: &DeclarationIndex,
    errors: &mut Vec<ExternCatalogError>,
) {
    match ty {
        Type::Map { key, value } => {
            if decls.map_key_error(key).is_some() {
                errors.push(invalid_type(
                    context,
                    key,
                    InvalidExternTypeReason::NonKeyableMapKey,
                    site,
                ));
            }
            validate_map_keys(context, value, site, decls, errors);
        }
        Type::Func { params, ret } => {
            for param in params {
                validate_map_keys(context, &param.ty, site, decls, errors);
            }
            validate_map_keys(context, &ret.ty(), site, decls, errors);
        }
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            validate_map_keys(context, elem, site, decls, errors);
        }
        Type::Optional { inner } => validate_map_keys(context, inner, site, decls, errors),
        Type::Tuple(elems) => {
            for elem in elems {
                validate_map_keys(context, elem, site, decls, errors);
            }
        }
        Type::Nominal(nominal) => {
            for arg in &nominal.type_args {
                validate_map_keys(context, arg, site, decls, errors);
            }
        }
        Type::Void
        | Type::Dyn(_)
        | Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Char
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. } => {}
    }
}

fn invalid_type(
    context: &ExternCatalogContext,
    ty: &Type,
    reason: InvalidExternTypeReason,
    site: RawExternSite,
) -> ExternCatalogError {
    ExternCatalogError::InvalidType {
        context: context.clone(),
        ty: ty.clone(),
        reason,
        site,
    }
}
