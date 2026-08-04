use std::collections::HashMap;

use anvyx_externs::{
    AbiPosition, AbiTypeError, BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread,
    ExternBindingKey, ExternBindingOp, ExternBindingTarget, ExternCallbackParam,
    ExternCallbackSignature, ExternEffects, ExternFunctionKey, ExternMemberKey,
    ExternMemberSelector, ExternOperator, ExternParam, ExternRep, ExternSignature, ExternTypeExpr,
    ExternTypeKey, OperatorReturn, ParamFlow, ProviderId, ReceiverMode, UnaryOp,
};

use crate::{
    ast::{ArrayLen, EscapeMode, FuncParam, GenericArg, Ident, ReturnSpec, Type, TypeVisitor},
    externs::{
        extern_module_path, extern_module_scope,
        raw::{
            ExternProvenance, RawExternFunction, RawExternInit, RawExternModule, RawExternOperator,
            RawExternSite, RawExternStatic, RawExternType, RawExterns,
        },
        raw_module_scope,
    },
    resolve::{ModuleId, ModulePath, PackageId},
    semantic_id::NominalId,
    span::SourceSpan,
    typecheck::{
        DeclarationIndex, GenericTypeContext, ModuleScope, NominalKey, TypeRefError,
        TypeRefResolver, nominal_type, type_closure_facts,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ExternModuleId(usize);

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
pub(crate) enum ExternLoweringInfo {
    Provider(ProviderExternLoweringInfo),
    Source { effects: ExternEffects },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ProviderExternLoweringInfo {
    pub(crate) package: PackageId,
    pub(crate) provider: ProviderId,
    pub(crate) module: anvyx_externs::ModulePath,
    pub(crate) key: ExternBindingKey,
    pub(crate) effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ProviderExternTypeLoweringInfo {
    pub(crate) package: PackageId,
    pub(crate) provider: ProviderId,
    pub(crate) key: ExternTypeKey,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ExternCatalog {
    modules: Vec<ExternModule>,
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
                    &function.provenance,
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

    pub(crate) fn module(&self, id: ExternModuleId) -> &ExternModule {
        &self.modules[id.0]
    }

    pub(crate) fn function(&self, id: ExternFunctionId) -> &ExternFunction {
        &self.functions[id.0]
    }

    pub(crate) fn function_lowering_info(&self, id: ExternFunctionId) -> ExternLoweringInfo {
        let function = self.function(id);
        match &function.provenance {
            ExternProvenance::Provider { package, provider } => {
                let module = provider_module_path(&function.key.module);
                ExternLoweringInfo::Provider(ProviderExternLoweringInfo {
                    package: package.clone(),
                    provider: provider.clone(),
                    module: module.clone(),
                    key: ExternBindingKey {
                        target: ExternBindingTarget::Function(ExternFunctionKey {
                            module,
                            name: function.key.name.to_string(),
                        }),
                        operation: ExternBindingOp::Call,
                    },
                    effects: function.effects,
                })
            }
            ExternProvenance::Source { .. } => ExternLoweringInfo::Source {
                effects: function.effects,
            },
        }
    }

    pub(crate) fn ty(&self, id: ExternTypeId) -> &ExternType {
        &self.types[id.0]
    }

    pub(crate) fn type_lowering_info(
        &self,
        id: ExternTypeId,
    ) -> Option<ProviderExternTypeLoweringInfo> {
        let ty = self.ty(id);
        match &ty.context.provenance {
            ExternProvenance::Provider { package, provider } => {
                Some(ProviderExternTypeLoweringInfo {
                    package: package.clone(),
                    provider: provider.clone(),
                    key: ExternTypeKey {
                        module: provider_module_path(&ty.key.module),
                        name: ty.key.name.to_string(),
                    },
                })
            }
            ExternProvenance::Source { .. } => None,
        }
    }

    pub(crate) fn field_lowering_info(
        &self,
        field_ref: ExternFieldRef,
        operation: ExternBindingOp,
    ) -> ExternLoweringInfo {
        let ty = self.ty(field_ref.owner);
        let field = &ty.fields[field_ref.id.0];
        Self::member_lowering_info(
            ty,
            ExternMemberSelector::Field(field.name.to_string()),
            operation,
            ExternEffects::default(),
        )
    }

    pub(crate) fn method_lowering_info(&self, method_ref: ExternMethodRef) -> ExternLoweringInfo {
        let ty = self.ty(method_ref.owner);
        let method = &ty.methods[method_ref.id.0];
        Self::member_lowering_info(
            ty,
            ExternMemberSelector::Method(method.name.to_string()),
            ExternBindingOp::Call,
            method.effects,
        )
    }

    pub(crate) fn static_lowering_info(&self, static_ref: ExternStaticRef) -> ExternLoweringInfo {
        let ty = self.ty(static_ref.owner);
        let static_method = &ty.statics[static_ref.id.0];
        Self::member_lowering_info(
            ty,
            ExternMemberSelector::Static(static_method.name.to_string()),
            ExternBindingOp::Call,
            static_method.effects,
        )
    }

    pub(crate) fn init_lowering_info(&self, owner: ExternTypeId) -> ExternLoweringInfo {
        let ty = self.ty(owner);
        Self::member_lowering_info(
            ty,
            ExternMemberSelector::Init,
            ExternBindingOp::Call,
            ty.init
                .as_ref()
                .map_or_else(ExternEffects::default, |init| init.effects),
        )
    }

    pub(crate) fn operator_lowering_info(
        &self,
        operator_ref: ExternOperatorRef,
    ) -> ExternLoweringInfo {
        let ty = self.ty(operator_ref.owner);
        let operator = &ty.operators[operator_ref.id.0];
        Self::member_lowering_info(
            ty,
            ExternMemberSelector::Operator(operator.op),
            ExternBindingOp::Call,
            operator.effects,
        )
    }

    fn member_lowering_info(
        ty: &ExternType,
        selector: ExternMemberSelector,
        operation: ExternBindingOp,
        effects: ExternEffects,
    ) -> ExternLoweringInfo {
        match &ty.context.provenance {
            ExternProvenance::Provider { package, provider } => {
                let module = provider_module_path(&ty.key.module);
                ExternLoweringInfo::Provider(ProviderExternLoweringInfo {
                    package: package.clone(),
                    provider: provider.clone(),
                    module: module.clone(),
                    key: ExternBindingKey {
                        target: ExternBindingTarget::Member(ExternMemberKey {
                            owner: ExternTypeKey {
                                module,
                                name: ty.key.name.to_string(),
                            },
                            selector,
                        }),
                        operation,
                    },
                    effects,
                })
            }
            ExternProvenance::Source { .. } => ExternLoweringInfo::Source { effects },
        }
    }

    pub(crate) fn function_by_key(&self, key: &FunctionKey) -> Option<ExternFunctionId> {
        self.functions_by_key.get(key).copied()
    }

    pub(crate) fn type_by_key(&self, key: &TypeKey) -> Option<ExternTypeId> {
        self.types_by_key.get(key).copied()
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

fn debug_assert_consistent(catalog: &ExternCatalog) {
    for module in &catalog.modules {
        debug_assert_eq!(catalog.module(module.id).id, module.id);
        for id in &module.functions {
            let function = catalog.function(*id);
            debug_assert_eq!(catalog.function_by_key(&function.key), Some(*id));
        }
        for id in &module.types {
            let ty = catalog.ty(*id);
            debug_assert_eq!(catalog.type_by_key(&ty.key), Some(*id));
            debug_assert_eq!(catalog.type_by_nominal(&ty.nominal), Some(*id));
            for field in &ty.fields {
                debug_assert_eq!(
                    catalog
                        .field(*id, field.name)
                        .map(|(field_ref, _)| field_ref.id),
                    Some(field.id)
                );
            }
            for method in &ty.methods {
                debug_assert_eq!(
                    catalog
                        .method(*id, method.name)
                        .map(|(method_ref, _)| method_ref.id),
                    Some(method.id)
                );
            }
            for static_method in &ty.statics {
                debug_assert_eq!(
                    catalog
                        .static_method(*id, static_method.name)
                        .map(|(static_ref, _)| static_ref.id),
                    Some(static_method.id)
                );
            }
            for operator in &ty.operators {
                let resolved = match operator.op {
                    ExternOperator::Unary(op) => catalog.unary_operator(*id, op),
                    ExternOperator::Binary { op, self_on_right } => {
                        catalog.binary_operator(*id, op, self_on_right)
                    }
                };
                debug_assert_eq!(
                    resolved.map(|(operator_ref, _)| operator_ref.id),
                    Some(operator.id)
                );
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternModule {
    pub(crate) id: ExternModuleId,
    pub(crate) scope: ModuleScope,
    pub(crate) functions: Vec<ExternFunctionId>,
    pub(crate) types: Vec<ExternTypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternFunction {
    pub(crate) id: ExternFunctionId,
    pub(crate) key: FunctionKey,
    pub(crate) provenance: ExternProvenance,
    pub(crate) site: RawExternSite,
    pub(crate) doc: Option<String>,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternType {
    pub(crate) id: ExternTypeId,
    pub(crate) key: TypeKey,
    pub(crate) nominal: NominalKey,
    pub(crate) context: ExternCatalogContext,
    pub(crate) exported: bool,
    pub(crate) site: RawExternSite,
    pub(crate) doc: Option<String>,
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
    pub(crate) effects: ExternEffects,
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
    pub(crate) doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternEnumVariantField {
    pub(crate) name: Option<Ident>,
    pub(crate) ty: ResolvedExternTy,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternField {
    pub(crate) id: ExternFieldId,
    pub(crate) name: Ident,
    pub(crate) ty: ResolvedExternTy,
    pub(crate) computed: bool,
    pub(crate) readable: bool,
    pub(crate) writable: bool,
    pub(crate) get_receiver: ReceiverMode,
    pub(crate) set_receiver: ReceiverMode,
    pub(crate) site: RawExternSite,
    pub(crate) doc: Option<String>,
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

    pub(crate) fn constructor_fields(
        &self,
    ) -> Option<impl Iterator<Item = (ExternInitField, &ExternField)> + '_> {
        self.required_init_fields()
    }
}

impl ExternInit {
    fn backs_literal(&self, owner: &NominalKey) -> bool {
        init_backs_literal(&self.signature, self.effects, owner)
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
    pub(crate) id: ExternMethodId,
    pub(crate) name: Ident,
    pub(crate) receiver: ReceiverMode,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) effects: ExternEffects,
    pub(crate) site: RawExternSite,
    pub(crate) doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternStatic {
    pub(crate) id: ExternStaticId,
    pub(crate) name: Ident,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) effects: ExternEffects,
    pub(crate) site: RawExternSite,
    pub(crate) doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternOperatorDecl {
    pub(crate) id: ExternOperatorId,
    pub(crate) op: ExternOperator,
    pub(crate) receiver: ReceiverMode,
    pub(crate) signature: ResolvedExternSignature,
    pub(crate) effects: ExternEffects,
    pub(crate) site: RawExternSite,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternCatalogContext {
    pub(crate) provenance: ExternProvenance,
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
    fn function(provenance: &ExternProvenance, module: ModuleScope, name: Ident) -> Self {
        Self {
            provenance: provenance.clone(),
            module,
            item: ExternContextItem::Function { name },
        }
    }

    fn ty(provenance: &ExternProvenance, module: ModuleScope, name: Ident) -> Self {
        Self {
            provenance: provenance.clone(),
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
            provenance: self.provenance.clone(),
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

pub(crate) fn build_catalog(
    raw: RawExterns,
    decls: &mut DeclarationIndex,
) -> Result<ExternCatalog, Vec<ExternCatalogError>> {
    CatalogBuilder::new(decls).build(raw)
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

    fn build(mut self, raw: RawExterns) -> Result<ExternCatalog, Vec<ExternCatalogError>> {
        let groups = raw.groups;
        for group in &groups {
            for module in &group.modules {
                self.allocate_module(module, &group.provenance);
            }
        }
        for group in &groups {
            for module in &group.modules {
                self.resolve_module(module, &group.provenance);
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        validate_catalog(&self.catalog, self.decls)?;
        debug_assert_consistent(&self.catalog);
        Ok(self.catalog)
    }

    fn allocate_module(&mut self, raw: &RawExternModule, provenance: &ExternProvenance) {
        let scope = raw_module_scope(&raw.scope);
        let module_id = self.module_id(scope.clone());

        for raw_ty in &raw.types {
            let name = Ident::new(&raw_ty.name);
            let id = ExternTypeId(self.catalog.types.len());
            let key = TypeKey {
                module: scope.clone(),
                name,
            };
            let nominal = self
                .decls
                .local_nominal_type(&scope, name)
                .expect("catalog extern type missing nominal metadata");
            self.catalog.modules[module_id.0].types.push(id);
            self.catalog.types_by_key.insert(key.clone(), id);
            self.catalog.types_by_nominal.insert(nominal.id.clone(), id);
            self.catalog.types.push(ExternType {
                id,
                key,
                nominal,
                context: ExternCatalogContext::ty(provenance, scope.clone(), name),
                exported: raw_ty.exported,
                site: raw_ty.site,
                doc: raw_ty.doc.clone(),
                rep: raw_ty.rep,
                layout: raw_ty.layout,
                materialization: raw_ty.materialization,
                owns_heap_edges: raw_ty.owns_heap_edges,
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

        for raw_func in &raw.functions {
            let name = Ident::new(&raw_func.decl.name);
            let id = ExternFunctionId(self.catalog.functions.len());
            let key = FunctionKey {
                module: scope.clone(),
                name,
            };
            self.catalog.modules[module_id.0].functions.push(id);
            self.catalog.functions_by_key.insert(key.clone(), id);
            self.catalog.functions.push(ExternFunction {
                id,
                key,
                provenance: provenance.clone(),
                site: raw_func.site,
                doc: raw_func.decl.doc.clone(),
                signature: ResolvedExternSignature::default(),
                effects: raw_func.decl.effects,
            });
        }
    }

    fn module_id(&mut self, scope: ModuleScope) -> ExternModuleId {
        if let Some(module) = self
            .catalog
            .modules
            .iter()
            .find(|module| module.scope == scope)
        {
            return module.id;
        }
        let id = ExternModuleId(self.catalog.modules.len());
        self.catalog.modules.push(ExternModule {
            id,
            scope,
            functions: vec![],
            types: vec![],
        });
        id
    }

    fn resolve_module(&mut self, raw: &RawExternModule, provenance: &ExternProvenance) {
        let scope = raw_module_scope(&raw.scope);
        for raw_func in &raw.functions {
            self.resolve_function(&scope, provenance, raw_func);
        }
        for raw_ty in &raw.types {
            self.resolve_type(&scope, raw_ty);
        }
    }

    fn resolve_function(
        &mut self,
        scope: &ModuleScope,
        provenance: &ExternProvenance,
        raw: &RawExternFunction,
    ) {
        let key = FunctionKey {
            module: scope.clone(),
            name: Ident::new(&raw.decl.name),
        };
        let Some(id) = self.catalog.functions_by_key.get(&key).copied() else {
            return;
        };
        let context = ExternCatalogContext::function(provenance, scope.clone(), key.name);
        let signature = self.resolve_signature(
            ResolveCtx::new(scope, &context, None, raw.site),
            &raw.decl.signature,
        );
        self.catalog.functions[id.0].signature = signature;
    }

    fn resolve_type(&mut self, scope: &ModuleScope, raw: &RawExternType) {
        let key = TypeKey {
            module: scope.clone(),
            name: Ident::new(&raw.name),
        };
        let Some(type_id) = self.catalog.types_by_key.get(&key).copied() else {
            return;
        };
        let owner = self.catalog.types[type_id.0].nominal.clone();

        for raw_field in &raw.fields {
            let id = ExternFieldId(self.catalog.types[type_id.0].fields.len());
            let name = Ident::new(&raw_field.decl.name);
            let context = self.catalog.types[type_id.0].context.field(name);
            let ty = self.resolve_ty(
                ResolveCtx::new(scope, &context, Some(&owner), raw_field.site),
                &raw_field.decl.ty,
            );
            let ty_decl = &mut self.catalog.types[type_id.0];
            ty_decl.fields_by_name.insert(name, id);
            ty_decl.fields.push(ExternField {
                id,
                name,
                ty,
                computed: raw_field.decl.computed,
                readable: raw_field.decl.readable,
                writable: raw_field.decl.writable,
                get_receiver: raw_field.decl.get_receiver,
                set_receiver: raw_field.decl.set_receiver,
                site: raw_field.site,
                doc: raw_field.decl.doc.clone(),
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
                            ResolveCtx::new(scope, &variant_context, Some(&owner), raw.site),
                            &field.ty,
                        ),
                    })
                    .collect(),
                doc: variant.doc.clone(),
            })
            .collect();
        self.catalog.types[type_id.0].variants = variants;

        if let Some(raw_init) = &raw.init {
            let context = self.catalog.types[type_id.0].context.init();
            let signature = self.resolve_init_signature(
                ResolveCtx::new(scope, &context, Some(&owner), raw_init.site),
                raw_init,
                &owner,
            );
            let backs_literal = init_backs_literal(&signature, raw_init.decl.effects, &owner);
            let fields = if backs_literal {
                self.resolve_init_fields(type_id, raw_init, &signature, &context)
            } else {
                ExternInitFields::default()
            };
            self.catalog.types[type_id.0].init = Some(ExternInit {
                fields,
                signature,
                effects: raw_init.decl.effects,
            });
        }

        for raw_method in &raw.methods {
            let id = ExternMethodId(self.catalog.types[type_id.0].methods.len());
            let name = Ident::new(&raw_method.decl.name);
            let context = self.catalog.types[type_id.0].context.method(name);
            let signature = self.resolve_signature(
                ResolveCtx::new(scope, &context, Some(&owner), raw_method.site),
                &raw_method.decl.signature,
            );
            let ty_decl = &mut self.catalog.types[type_id.0];
            ty_decl.methods_by_name.insert(name, id);
            ty_decl.methods.push(ExternMethod {
                id,
                name,
                receiver: raw_method.decl.receiver,
                signature,
                effects: raw_method.decl.effects,
                site: raw_method.site,
                doc: raw_method.decl.doc.clone(),
            });
        }

        for raw_static in &raw.statics {
            self.resolve_static(type_id, scope, &owner, raw_static);
        }
        for raw_operator in &raw.operators {
            self.resolve_operator(type_id, scope, &owner, raw_operator);
        }
    }

    fn resolve_init_signature(
        &mut self,
        ctx: ResolveCtx<'_>,
        raw_init: &RawExternInit,
        owner: &NominalKey,
    ) -> ResolvedExternSignature {
        let ret = if raw_init.decl.ret == ExternTypeExpr::Void {
            ResolvedExternTy {
                ty: nominal_type(owner),
                abi: ExternTypeExpr::named(None, owner.name.to_string()),
            }
        } else {
            self.resolve_ty(ctx, &raw_init.decl.ret)
        };
        ResolvedExternSignature {
            params: self.resolve_params(ctx, &raw_init.decl.params),
            ret,
        }
    }

    fn resolve_init_fields(
        &mut self,
        type_id: ExternTypeId,
        raw_init: &RawExternInit,
        signature: &ResolvedExternSignature,
        context: &ExternCatalogContext,
    ) -> ExternInitFields {
        let required = self.resolve_init_field_group(
            type_id,
            raw_init,
            signature,
            context,
            &raw_init.decl.field_init,
        );
        let presence = self.resolve_init_field_group(
            type_id,
            raw_init,
            signature,
            context,
            &raw_init.decl.presence_init,
        );
        ExternInitFields { required, presence }
    }

    fn resolve_init_field_group(
        &mut self,
        type_id: ExternTypeId,
        raw_init: &RawExternInit,
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
                    site: raw_init.site,
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
                    site: raw_init.site,
                });
            }
            fields.push(ExternInitField {
                field: id,
                param: param_index,
            });
        }
        fields
    }

    fn resolve_static(
        &mut self,
        type_id: ExternTypeId,
        scope: &ModuleScope,
        owner: &NominalKey,
        raw: &RawExternStatic,
    ) {
        let id = ExternStaticId(self.catalog.types[type_id.0].statics.len());
        let name = Ident::new(&raw.decl.name);
        let context = self.catalog.types[type_id.0].context.static_method(name);
        let signature = self.resolve_signature(
            ResolveCtx::new(scope, &context, Some(owner), raw.site),
            &raw.decl.signature,
        );
        let ty = &mut self.catalog.types[type_id.0];
        ty.statics_by_name.insert(name, id);
        ty.statics.push(ExternStatic {
            id,
            name,
            signature,
            effects: raw.decl.effects,
            site: raw.site,
            doc: raw.decl.doc.clone(),
        });
    }

    fn resolve_operator(
        &mut self,
        type_id: ExternTypeId,
        scope: &ModuleScope,
        owner: &NominalKey,
        raw: &RawExternOperator,
    ) {
        let id = ExternOperatorId(self.catalog.types[type_id.0].operators.len());
        let context = self.catalog.types[type_id.0].context.operator(raw.decl.op);
        let signature = self.resolve_signature(
            ResolveCtx::new(scope, &context, Some(owner), raw.site),
            &raw.decl.signature,
        );
        let ty = &mut self.catalog.types[type_id.0];
        ty.operators_by_op.insert(raw.decl.op, id);
        ty.operators.push(ExternOperatorDecl {
            id,
            op: raw.decl.op,
            receiver: raw.decl.receiver,
            signature,
            effects: raw.decl.effects,
            site: raw.site,
        });
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
        let abi = if matches!(ctx.context.provenance, ExternProvenance::Source { .. }) {
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
        if matches!(ctx.context.provenance, ExternProvenance::Source { .. }) {
            return self.resolve_source_type_ref(ctx, module, name, args);
        }

        let Some(key) = self.resolve_provider_named(ctx.scope, module, name) else {
            self.errors.push(ExternCatalogError::UnknownType {
                context: ctx.context.clone(),
                module: module.map(|module| missing_type_module(ctx.scope, ctx.context, module)),
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
                    module: Some(missing_type_module(ctx.scope, ctx.context, module)),
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
        scope: &ModuleScope,
        module: Option<&anvyx_externs::ModulePath>,
        name: Ident,
    ) -> Option<NominalKey> {
        match module {
            Some(module) => self
                .decls
                .exported_nominal_type(&provider_module_scope(scope, module), name),
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
    scope: &ModuleScope,
    context: &ExternCatalogContext,
    module: &anvyx_externs::ModulePath,
) -> ModuleScope {
    match context.provenance {
        ExternProvenance::Provider { .. } => provider_module_scope(scope, module),
        ExternProvenance::Source { .. } => extern_module_scope(module),
    }
}

fn provider_module_path(scope: &ModuleScope) -> anvyx_externs::ModulePath {
    let segments = match scope {
        ModuleScope::Package(current) => current
            .provider_path()
            .expect("provider extern scope has provider path")
            .segments()
            .to_vec(),
        ModuleScope::Named(path) => path.segments().to_vec(),
        ModuleScope::Root => vec![],
    };
    anvyx_externs::ModulePath { segments }
}

fn provider_module_scope(scope: &ModuleScope, module: &anvyx_externs::ModulePath) -> ModuleScope {
    let path = extern_module_path(module);
    match scope {
        ModuleScope::Package(current) => {
            ModuleScope::from_module_id(&ModuleId::provider(current.package().clone(), path))
        }
        ModuleScope::Root | ModuleScope::Named(_) => ModuleScope::Named(path),
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
    validate_abi_type(context, &ty.abi, position, site, errors);
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
            let source_generic = matches!(context.provenance, ExternProvenance::Source { .. })
                && violation.reason == AbiTypeError::GenericNamedArgsUnsupported;
            (!source_generic).then(|| ExternCatalogError::InvalidAbiType {
                context: context.clone(),
                position: violation.position,
                reason: violation.reason,
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
    if matches!(context.provenance, ExternProvenance::Source { .. }) && flags.visit_type(ty) {
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
