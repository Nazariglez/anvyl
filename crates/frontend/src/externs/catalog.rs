use std::collections::HashMap;

use anvyx_externs::{
    BinaryOp, CallbackEscape, ExternEffects, ExternOperator, ExternParam, ExternRep,
    ExternSignature, ExternTypeExpr, OperatorReturn, ParamFlow, ReceiverMode, UnaryOp,
};

use crate::{
    ast::{EscapeMode, FuncParam, GenericArg, Ident, NominalKind, ReturnSpec, Type},
    externs::{
        extern_module_path, extern_module_scope,
        raw::{
            ExternProvenance, RawExternFunction, RawExternModule, RawExternOperator, RawExternSite,
            RawExternStatic, RawExternType, RawExterns,
        },
        raw_module_scope,
    },
    resolve::{ModuleId, ModulePath},
    typecheck::{
        DeclarationIndex, GenericTypeContext, ModuleScope, NominalKey, TypeRefError,
        type_closure_facts,
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
        Type::Func {
            params: self
                .params
                .iter()
                .map(ResolvedExternParam::to_func_param)
                .collect(),
            ret: Box::new(ReturnSpec::value(self.ret.ty.clone())),
        }
    }
}

fn callback_escape_mode(escape: CallbackEscape, ty: &ExternTypeExpr) -> EscapeMode {
    let esc_from_param = matches!(escape, CallbackEscape::Escaping);
    let esc_from_callback = match ty {
        ExternTypeExpr::Callback(callback) => callback.policy.escape == CallbackEscape::Escaping,
        _ => false,
    };
    if esc_from_param || esc_from_callback {
        EscapeMode::Escaping
    } else {
        EscapeMode::NonEscaping
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
}

impl ResolvedExternTy {
    pub(crate) fn contains_any(&self) -> bool {
        type_closure_facts(&self.ty).contains_any
    }
}

impl Default for ResolvedExternTy {
    fn default() -> Self {
        Self { ty: Type::Void }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ExternCatalog {
    modules: Vec<ExternModule>,
    types: Vec<ExternType>,
    functions: Vec<ExternFunction>,
    functions_by_key: HashMap<FunctionKey, ExternFunctionId>,
    types_by_key: HashMap<TypeKey, ExternTypeId>,
    types_by_nominal: HashMap<NominalKey, ExternTypeId>,
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
        for function in &self.functions {
            visit_extern_signature(&function.signature, function.site, &mut visit);
        }
        for ty in &self.types {
            for field in &ty.fields {
                visit_extern_ty(&field.ty, field.site, &mut visit);
            }
            for method in &ty.methods {
                visit_extern_signature(&method.signature, method.site, &mut visit);
            }
            for static_method in &ty.statics {
                visit_extern_signature(&static_method.signature, static_method.site, &mut visit);
            }
            for operator in &ty.operators {
                visit_extern_signature(&operator.signature, operator.site, &mut visit);
            }
        }
    }

    pub(crate) fn module(&self, id: ExternModuleId) -> &ExternModule {
        &self.modules[id.0]
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

    pub(crate) fn type_by_key(&self, key: &TypeKey) -> Option<ExternTypeId> {
        self.types_by_key.get(key).copied()
    }

    pub(crate) fn type_by_nominal(&self, key: &NominalKey) -> Option<ExternTypeId> {
        self.types_by_nominal.get(key).copied()
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

    pub(crate) fn init(&self, owner: ExternTypeId) -> Option<&ExternInit> {
        self.ty(owner).init.as_ref()
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
            if ty.init.is_some() {
                debug_assert!(catalog.init(*id).is_some());
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
    pub(crate) site: RawExternSite,
    pub(crate) doc: Option<String>,
    pub(crate) rep: ExternRep,
    pub(crate) fields: Vec<ExternField>,
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
pub(crate) struct ExternField {
    pub(crate) id: ExternFieldId,
    pub(crate) name: Ident,
    pub(crate) ty: ResolvedExternTy,
    pub(crate) computed: bool,
    pub(crate) site: RawExternSite,
    pub(crate) doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ExternInit {
    pub(crate) field_init: Vec<Ident>,
    pub(crate) site: RawExternSite,
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
    UnknownInitField {
        context: ExternCatalogContext,
        field: Ident,
        site: RawExternSite,
    },
    ComputedInitField {
        context: ExternCatalogContext,
        field: Ident,
        site: RawExternSite,
    },
    UnsupportedInitParams {
        context: ExternCatalogContext,
        count: usize,
        site: RawExternSite,
    },
    InvalidOperatorReturn {
        context: ExternCatalogContext,
        found: Type,
        expected: OperatorReturn,
        site: RawExternSite,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InvalidExternTypeReason {
    Unresolved,
    Infer,
    Void,
    UnresolvedConst,
}

fn visit_extern_signature(
    signature: &ResolvedExternSignature,
    site: RawExternSite,
    visit: &mut impl FnMut(&ResolvedExternTy, RawExternSite),
) {
    for param in &signature.params {
        visit_extern_ty(&param.ty, site, visit);
    }
    visit_extern_ty(&signature.ret, site, visit);
}

fn visit_extern_ty(
    ty: &ResolvedExternTy,
    site: RawExternSite,
    visit: &mut impl FnMut(&ResolvedExternTy, RawExternSite),
) {
    visit(ty, site);
}

pub(crate) fn build_catalog(
    raw: RawExterns,
    decls: &DeclarationIndex,
) -> Result<ExternCatalog, Vec<ExternCatalogError>> {
    CatalogBuilder::new(decls).build(raw)
}

struct CatalogBuilder<'a> {
    decls: &'a DeclarationIndex,
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
    fn new(decls: &'a DeclarationIndex) -> Self {
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

        validate_catalog(&self.catalog)?;
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
            let nominal = NominalKey {
                module: scope.clone(),
                kind: NominalKind::Extern,
                name,
            };
            self.catalog.modules[module_id.0].types.push(id);
            self.catalog.types_by_key.insert(key.clone(), id);
            self.catalog.types_by_nominal.insert(nominal.clone(), id);
            self.catalog.types.push(ExternType {
                id,
                key,
                nominal,
                context: ExternCatalogContext::ty(provenance, scope.clone(), name),
                site: raw_ty.site,
                doc: raw_ty.doc.clone(),
                rep: raw_ty.rep,
                fields: vec![],
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
                site: raw_field.site,
                doc: raw_field.decl.doc.clone(),
            });
        }

        if let Some(raw_init) = &raw.init {
            let context = self.catalog.types[type_id.0].context.init();
            if !raw_init.decl.params.is_empty() {
                self.errors.push(ExternCatalogError::UnsupportedInitParams {
                    context,
                    count: raw_init.decl.params.len(),
                    site: raw_init.site,
                });
            }
            self.catalog.types[type_id.0].init = Some(ExternInit {
                field_init: raw_init.decl.field_init.iter().map(Ident::new).collect(),
                site: raw_init.site,
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
            .map(|param| ResolvedExternParam {
                name: param.name.as_deref().map(Ident::new),
                ty: self.resolve_ty(ctx, &param.ty),
                flow: param.flow,
                escape: callback_escape_mode(param.escape, &param.ty),
            })
            .collect()
    }

    fn resolve_ty(&mut self, ctx: ResolveCtx<'_>, ty: &ExternTypeExpr) -> ResolvedExternTy {
        ResolvedExternTy {
            ty: self.resolve_ty_inner(ctx, ty),
        }
    }

    fn resolve_ty_inner(&mut self, ctx: ResolveCtx<'_>, ty: &ExternTypeExpr) -> Type {
        match ty {
            ExternTypeExpr::Void => Type::Void,
            ExternTypeExpr::Bool => Type::Bool,
            ExternTypeExpr::Int => Type::Int,
            ExternTypeExpr::Float => Type::Float,
            ExternTypeExpr::String => Type::String,
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
                self.decls
                    .core_option_of(inner)
                    .expect("core Option declaration is available")
            }
            ExternTypeExpr::Callback(callback) => {
                let params = callback
                    .params
                    .iter()
                    .map(|param| {
                        FuncParam::new(
                            self.resolve_ty(ctx, &param.ty).ty,
                            false,
                            false,
                            callback_escape_mode(param.escape, &param.ty),
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
        match self
            .decls
            .finalize_type_ref(&ctx.context.module, &GenericTypeContext::default(), &ty)
        {
            Ok(ty) => ty,
            Err(error) => self.type_ref_error(ctx.context, name, error, ctx.site),
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
                .exported_nominal_type(&provider_module_scope(scope, module), name)
                .or_else(|| {
                    self.decls
                        .exported_nominal_type(&provider_source_module_scope(scope, module), name)
                }),
            None => self.decls.local_nominal_type(scope, name).or_else(|| {
                self.decls
                    .local_nominal_type(&provider_source_current_scope(scope)?, name)
            }),
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
            TypeRefError::Unknown { qualifier, name }
            | TypeRefError::UnknownContract { qualifier, name } => {
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
            TypeRefError::AliasCycle { name }
            | TypeRefError::ContractAsType { name }
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
            TypeRefError::UnsupportedContractComposition => {
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

fn provider_module_scope(scope: &ModuleScope, module: &anvyx_externs::ModulePath) -> ModuleScope {
    let path = extern_module_path(module);
    match scope {
        ModuleScope::Package(current) => {
            ModuleScope::from_module_id(&ModuleId::provider(current.package().clone(), path))
        }
        ModuleScope::Root | ModuleScope::Named(_) => ModuleScope::Named(path),
    }
}

fn provider_source_module_scope(
    scope: &ModuleScope,
    module: &anvyx_externs::ModulePath,
) -> ModuleScope {
    let path = extern_module_path(module);
    match scope {
        ModuleScope::Package(current) => {
            ModuleScope::from_module_id(&ModuleId::named(current.package().clone(), path))
        }
        ModuleScope::Root | ModuleScope::Named(_) => ModuleScope::Named(path),
    }
}

fn provider_source_current_scope(scope: &ModuleScope) -> Option<ModuleScope> {
    let ModuleScope::Package(current) = scope else {
        return None;
    };
    let path = current.provider_path()?.clone();
    Some(ModuleScope::from_module_id(&ModuleId::named(
        current.package().clone(),
        path,
    )))
}

fn validate_catalog(catalog: &ExternCatalog) -> Result<(), Vec<ExternCatalogError>> {
    let mut errors = vec![];
    for ty in &catalog.types {
        validate_extern_type(ty, &mut errors);
    }
    for function in &catalog.functions {
        let context = ExternCatalogContext::function(
            &function.provenance,
            function.key.module.clone(),
            function.key.name,
        );
        validate_signature(&context, &function.signature, function.site, &mut errors);
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn validate_extern_type(ty: &ExternType, errors: &mut Vec<ExternCatalogError>) {
    for field in &ty.fields {
        let context = ty.context.field(field.name);
        validate_ty(&context, &field.ty, TypePosition::Param, field.site, errors);
    }

    if let Some(init) = &ty.init {
        validate_init(ty, init, errors);
    }

    for method in &ty.methods {
        let context = ty.context.method(method.name);
        validate_signature(&context, &method.signature, method.site, errors);
    }
    for static_method in &ty.statics {
        let context = ty.context.static_method(static_method.name);
        validate_signature(
            &context,
            &static_method.signature,
            static_method.site,
            errors,
        );
    }
    for operator in &ty.operators {
        let context = ty.context.operator(operator.op);
        validate_signature(&context, &operator.signature, operator.site, errors);
        validate_operator_return(&context, operator, errors);
    }
}

fn validate_init(ty: &ExternType, init: &ExternInit, errors: &mut Vec<ExternCatalogError>) {
    let context = ty.context.init();
    for name in &init.field_init {
        let Some(field) = ty.fields_by_name.get(name).map(|id| &ty.fields[id.0]) else {
            errors.push(ExternCatalogError::UnknownInitField {
                context: context.clone(),
                field: *name,
                site: init.site,
            });
            continue;
        };
        if field.computed {
            errors.push(ExternCatalogError::ComputedInitField {
                context: context.clone(),
                field: *name,
                site: init.site,
            });
        }
    }
}

fn validate_signature(
    context: &ExternCatalogContext,
    signature: &ResolvedExternSignature,
    site: RawExternSite,
    errors: &mut Vec<ExternCatalogError>,
) {
    for param in &signature.params {
        validate_ty(context, &param.ty, TypePosition::Param, site, errors);
    }
    validate_ty(context, &signature.ret, TypePosition::Return, site, errors);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypePosition {
    Param,
    Return,
    Nested,
}

impl TypePosition {
    fn allows_void(self) -> bool {
        matches!(self, TypePosition::Return)
    }

    fn callback_param_position(self) -> Self {
        match self {
            TypePosition::Nested => TypePosition::Nested,
            TypePosition::Param | TypePosition::Return => TypePosition::Param,
        }
    }
}

fn validate_ty(
    context: &ExternCatalogContext,
    ty: &ResolvedExternTy,
    position: TypePosition,
    site: RawExternSite,
    errors: &mut Vec<ExternCatalogError>,
) {
    validate_type_facts(context, &ty.ty, site, errors);
    validate_void_positions(context, &ty.ty, position, site, errors);
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

fn validate_void_positions(
    context: &ExternCatalogContext,
    ty: &Type,
    position: TypePosition,
    site: RawExternSite,
    errors: &mut Vec<ExternCatalogError>,
) {
    match ty {
        Type::Void if !position.allows_void() => {
            errors.push(invalid_type(
                context,
                ty,
                InvalidExternTypeReason::Void,
                site,
            ));
        }
        Type::Func { params, ret } => {
            for param in params {
                validate_void_positions(
                    context,
                    &param.ty,
                    position.callback_param_position(),
                    site,
                    errors,
                );
            }
            validate_void_positions(context, &ret.ty, TypePosition::Return, site, errors);
        }
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            validate_void_positions(context, elem, TypePosition::Nested, site, errors);
        }
        Type::Map { key, value } => {
            validate_void_positions(context, key, TypePosition::Nested, site, errors);
            validate_void_positions(context, value, TypePosition::Nested, site, errors);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                validate_void_positions(context, elem, TypePosition::Nested, site, errors);
            }
        }
        Type::Nominal(nominal) => {
            for arg in &nominal.type_args {
                validate_void_positions(context, arg, TypePosition::Nested, site, errors);
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

#[cfg(test)]
mod tests {
    use anvyx_externs::{
        BinaryOp, CallbackEscape, CallbackPolicy, CallbackThread, ExternCallbackParam,
        ExternCallbackSignature, ExternFieldDescriptor, ExternFunctionDescriptor,
        ExternInitDescriptor, ExternModuleDescriptor, ExternOperator, ExternParam, ExternSignature,
        ExternTypeDescriptor, ExternTypeExpr, ModulePath as ExternModulePath, ProviderDescriptor,
        ProviderId, UnaryOp,
    };

    use super::*;
    use crate::{
        ast::{Ident, ModuleOrigin, NominalKind, Program},
        externs::RawExternScope,
        resolve::PackageId,
        test_support::{parse_program, resolved_modules, root_id, test_source_id},
        typecheck::DeclarationIndex,
    };

    #[derive(Default)]
    struct CatalogBuilder {
        catalog: ExternCatalog,
    }

    impl CatalogBuilder {
        fn finish(self) -> ExternCatalog {
            self.catalog
        }

        fn module(&mut self, scope: ModuleScope) -> ExternModuleId {
            let id = ExternModuleId(self.catalog.modules.len());
            self.catalog.modules.push(ExternModule {
                id,
                scope,
                functions: vec![],
                types: vec![],
            });
            id
        }

        fn function(&mut self, module: ExternModuleId, key: FunctionKey) -> ExternFunctionId {
            let id = ExternFunctionId(self.catalog.functions.len());
            assert!(
                self.catalog
                    .functions_by_key
                    .insert(key.clone(), id)
                    .is_none()
            );
            self.catalog.modules[module.0].functions.push(id);
            self.catalog.functions.push(ExternFunction {
                id,
                key,
                provenance: source_provenance(),
                site: RawExternSite::default(),
                doc: None,
                signature: ResolvedExternSignature::default(),
                effects: ExternEffects::default(),
            });
            id
        }

        fn ty(
            &mut self,
            module: ExternModuleId,
            key: TypeKey,
            nominal: NominalKey,
        ) -> ExternTypeId {
            let id = ExternTypeId(self.catalog.types.len());
            assert!(self.catalog.types_by_key.insert(key.clone(), id).is_none());
            assert!(
                self.catalog
                    .types_by_nominal
                    .insert(nominal.clone(), id)
                    .is_none()
            );
            let context =
                ExternCatalogContext::ty(&source_provenance(), key.module.clone(), key.name);
            self.catalog.modules[module.0].types.push(id);
            self.catalog.types.push(ExternType {
                id,
                key,
                nominal,
                context,
                site: RawExternSite::default(),
                doc: None,
                rep: ExternRep::Shared,
                fields: vec![],
                init: None,
                methods: vec![],
                statics: vec![],
                operators: vec![],
                fields_by_name: HashMap::new(),
                methods_by_name: HashMap::new(),
                statics_by_name: HashMap::new(),
                operators_by_op: HashMap::new(),
            });
            id
        }

        fn field(&mut self, owner: ExternTypeId, name: Ident) -> ExternFieldId {
            let ty = &mut self.catalog.types[owner.0];
            let id = ExternFieldId(ty.fields.len());
            assert!(ty.fields_by_name.insert(name, id).is_none());
            ty.fields.push(ExternField {
                id,
                name,
                ty: resolved_ty(Type::Int),
                computed: false,
                site: RawExternSite::default(),
                doc: None,
            });
            id
        }

        fn field_ty(&mut self, owner: ExternTypeId, field: ExternFieldId, ty: Type) {
            self.catalog.types[owner.0].fields[field.0].ty = resolved_ty(ty);
        }

        fn init(&mut self, owner: ExternTypeId, field_init: Vec<Ident>) {
            let ty = &mut self.catalog.types[owner.0];
            assert!(ty.init.is_none());
            ty.init = Some(ExternInit {
                field_init,
                site: RawExternSite::default(),
            });
        }

        fn method(&mut self, owner: ExternTypeId, name: Ident) -> ExternMethodId {
            let ty = &mut self.catalog.types[owner.0];
            let id = ExternMethodId(ty.methods.len());
            assert!(ty.methods_by_name.insert(name, id).is_none());
            ty.methods.push(ExternMethod {
                id,
                name,
                receiver: ReceiverMode::Shared,
                signature: ResolvedExternSignature::default(),
                effects: ExternEffects::default(),
                site: RawExternSite::default(),
                doc: None,
            });
            id
        }

        fn static_method(&mut self, owner: ExternTypeId, name: Ident) -> ExternStaticId {
            let ty = &mut self.catalog.types[owner.0];
            let id = ExternStaticId(ty.statics.len());
            assert!(ty.statics_by_name.insert(name, id).is_none());
            ty.statics.push(ExternStatic {
                id,
                name,
                signature: ResolvedExternSignature::default(),
                effects: ExternEffects::default(),
                site: RawExternSite::default(),
                doc: None,
            });
            id
        }

        fn operator(&mut self, owner: ExternTypeId, op: ExternOperator) -> ExternOperatorId {
            let ty = &mut self.catalog.types[owner.0];
            let id = ExternOperatorId(ty.operators.len());
            assert!(ty.operators_by_op.insert(op, id).is_none());
            ty.operators.push(ExternOperatorDecl {
                id,
                op,
                signature: ResolvedExternSignature::default(),
                effects: ExternEffects::default(),
                site: RawExternSite::default(),
            });
            id
        }

        fn operator_ret(&mut self, owner: ExternTypeId, operator: ExternOperatorId, ret: Type) {
            self.catalog.types[owner.0].operators[operator.0]
                .signature
                .ret = resolved_ty(ret);
        }
    }

    fn resolved_ty(ty: Type) -> ResolvedExternTy {
        ResolvedExternTy { ty }
    }

    fn unresolved_ty(name: &str) -> ResolvedExternTy {
        resolved_ty(Type::UnresolvedName(ident(name)))
    }

    fn source_provenance() -> ExternProvenance {
        ExternProvenance::Source {
            module: raw_root_scope(),
        }
    }

    fn raw_root_scope() -> RawExternScope {
        RawExternScope::Module(root_id())
    }

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn scope(name: &str) -> ModuleScope {
        ModuleScope::Named(ModulePath::new(vec![name.to_string()]).unwrap())
    }

    fn provider_scope(name: &str) -> ModuleScope {
        ModuleScope::from_module_id(&ModuleId::provider(
            PackageId::synthetic_root(),
            ModulePath::new(vec![name.to_string()]).unwrap(),
        ))
    }

    fn function_key(module: ModuleScope, name: &str) -> FunctionKey {
        FunctionKey {
            module,
            name: ident(name),
        }
    }

    fn type_key(module: ModuleScope, name: &str) -> TypeKey {
        TypeKey {
            module,
            name: ident(name),
        }
    }

    fn nominal(module: ModuleScope, name: &str) -> NominalKey {
        NominalKey {
            module,
            kind: NominalKind::Extern,
            name: ident(name),
        }
    }

    fn provider_nominal(module: &str, name: &str) -> Type {
        Type::nominal_with_origin(
            NominalKind::Extern,
            ident(name),
            vec![],
            vec![],
            Some(ModuleOrigin::Provider {
                package: PackageId::synthetic_root().to_string(),
                path: vec![module.to_string()].into(),
            }),
        )
    }

    fn parse(source: &str) -> Program {
        parse_program(source)
    }

    fn decls(root: &str, modules: &[(&str, &str)], raw: &RawExterns) -> DeclarationIndex {
        let root = parse(root);
        let mut resolved = resolved_modules(&root, modules);
        let option_module = ModuleId::named(
            PackageId::core(),
            ModulePath::new(vec!["option".to_string()]).unwrap(),
        );
        resolved.import_edges.insert(option_module.clone(), vec![]);
        resolved
            .module_groups
            .push(vec![crate::resolve::ResolvedModule {
                key: option_module,
                source: test_source_id(),
                program: parse("pub enum Option<T> { None, Some(T) }"),
            }]);
        DeclarationIndex::from_root_and_modules(&root, &resolved, raw)
    }

    fn extern_module(path: &[&str]) -> ExternModulePath {
        ExternModulePath {
            segments: path.iter().map(|segment| (*segment).to_string()).collect(),
        }
    }

    fn provider_raw(module: ExternModuleDescriptor) -> RawExterns {
        crate::externs::ingest_providers(crate::externs::ExternInputs {
            packages: vec![crate::externs::PackageExternInputs {
                package: PackageId::synthetic_root(),
                providers: vec![ProviderDescriptor {
                    provider: ProviderId {
                        name: "host".to_string(),
                    },
                    modules: vec![module],
                }],
            }],
        })
        .unwrap()
    }

    fn source_raw(root: &str, modules: &[(&str, &str)]) -> RawExterns {
        let root = parse(root);
        let resolved = resolved_modules(&root, modules);
        crate::externs::collect_source_externs(&root, &resolved).unwrap()
    }

    fn ext_param(name: &str, ty: ExternTypeExpr) -> ExternParam {
        ExternParam {
            name: Some(name.to_string()),
            ty,
            flow: ParamFlow::Value,
            escape: CallbackEscape::NonEscaping,
        }
    }

    fn cb_param(ty: ExternTypeExpr, escape: CallbackEscape) -> ExternCallbackParam {
        ExternCallbackParam { ty, escape }
    }

    fn ext_signature(params: Vec<ExternParam>, ret: ExternTypeExpr) -> ExternSignature {
        ExternSignature { params, ret }
    }

    fn named(name: &str) -> ExternTypeExpr {
        ExternTypeExpr::Named {
            module: None,
            name: name.to_string(),
            args: vec![],
        }
    }

    fn module_named(module: &[&str], name: &str) -> ExternTypeExpr {
        ExternTypeExpr::Named {
            module: Some(extern_module(module)),
            name: name.to_string(),
            args: vec![],
        }
    }

    fn descriptor_type(name: &str) -> ExternTypeDescriptor {
        ExternTypeDescriptor {
            name: name.to_string(),
            doc: None,
            rep: ExternRep::Shared,
            fields: vec![],
            init: None,
            methods: vec![],
            statics: vec![],
            operators: vec![],
        }
    }

    mod member_validation {
        use super::*;

        #[test]
        fn visits_member_types() {
            let mut builder = CatalogBuilder::default();
            let module = builder.module(ModuleScope::Root);
            let function = builder.function(module, function_key(ModuleScope::Root, "free"));
            let owner = builder.ty(
                module,
                type_key(ModuleScope::Root, "Host"),
                nominal(ModuleScope::Root, "Host"),
            );
            let field = builder.field(owner, ident("field"));
            let method = builder.method(owner, ident("method"));
            let static_method = builder.static_method(owner, ident("static_method"));
            let operator = builder.operator(owner, ExternOperator::Unary(UnaryOp::Neg));
            let mut catalog = builder.finish();

            catalog.functions[function.0].signature = ResolvedExternSignature {
                params: vec![ResolvedExternParam {
                    name: None,
                    ty: unresolved_ty("FreeParam"),
                    flow: ParamFlow::Value,
                    escape: EscapeMode::NonEscaping,
                }],
                ret: unresolved_ty("FreeRet"),
            };
            catalog.types[owner.0].fields[field.0].ty = unresolved_ty("Field");
            catalog.types[owner.0].init = Some(ExternInit {
                field_init: vec![],
                site: RawExternSite::default(),
            });
            catalog.types[owner.0].methods[method.0].signature = ResolvedExternSignature {
                params: vec![ResolvedExternParam {
                    name: None,
                    ty: unresolved_ty("MethodParam"),
                    flow: ParamFlow::Value,
                    escape: EscapeMode::NonEscaping,
                }],
                ret: unresolved_ty("MethodRet"),
            };
            catalog.types[owner.0].statics[static_method.0].signature = ResolvedExternSignature {
                params: vec![],
                ret: unresolved_ty("StaticRet"),
            };
            catalog.types[owner.0].operators[operator.0].signature = ResolvedExternSignature {
                params: vec![ResolvedExternParam {
                    name: None,
                    ty: unresolved_ty("OperatorParam"),
                    flow: ParamFlow::Value,
                    escape: EscapeMode::NonEscaping,
                }],
                ret: unresolved_ty("OperatorRet"),
            };

            let mut names = vec![];
            catalog.for_each_resolved_ty(|ty, _| {
                if let Type::UnresolvedName(name) = ty.ty {
                    names.push(name.to_string());
                }
            });
            names.sort();

            assert_eq!(
                names,
                [
                    "Field",
                    "FreeParam",
                    "FreeRet",
                    "MethodParam",
                    "MethodRet",
                    "OperatorParam",
                    "OperatorRet",
                    "StaticRet",
                ]
            );
        }
    }

    mod resolution {
        use super::*;

        #[test]
        fn builds_mixed_sources() {
            let mut raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![descriptor_type("Handle")],
                functions: vec![ExternFunctionDescriptor {
                    name: "make".to_string(),
                    doc: None,
                    signature: ext_signature(vec![], named("Handle")),
                    effects: ExternEffects::default(),
                }],
            });
            raw.append(source_raw(
                "extern type Local { value: int; init; } extern fn use_local(x: Local) -> void;",
                &[],
            ));
            let decls = decls(
                "extern type Local { value: int; init; } extern fn use_local(x: Local) -> void;",
                &[],
                &raw,
            );
            let catalog = build_catalog(raw, &decls).unwrap();

            assert!(
                catalog
                    .type_by_key(&type_key(provider_scope("host"), "Handle"))
                    .is_some()
            );
            assert!(
                catalog
                    .function_by_key(&function_key(provider_scope("host"), "make"))
                    .is_some()
            );
            assert!(
                catalog
                    .type_by_key(&type_key(ModuleScope::Root, "Local"))
                    .is_some()
            );
            assert!(
                catalog
                    .function_by_key(&function_key(ModuleScope::Root, "use_local"))
                    .is_some()
            );
        }

        #[test]
        fn provider_unqualified_type_is_local() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["math"]),
                types: vec![descriptor_type("Vec2")],
                functions: vec![ExternFunctionDescriptor {
                    name: "len".to_string(),
                    doc: None,
                    signature: ext_signature(vec![ext_param("v", named("Vec2"))], named("Vec2")),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let function = catalog.function(
                catalog
                    .function_by_key(&function_key(provider_scope("math"), "len"))
                    .unwrap(),
            );

            assert_eq!(
                function.signature.params[0].ty.ty,
                provider_nominal("math", "Vec2")
            );
            assert!(!function.signature.params[0].ty.contains_any());
        }

        #[test]
        fn provider_absolute_type_uses_export() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["math"]),
                types: vec![descriptor_type("Vec2")],
                functions: vec![ExternFunctionDescriptor {
                    name: "make".to_string(),
                    doc: None,
                    signature: ext_signature(vec![], module_named(&["math"], "Vec2")),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let function = catalog.function(
                catalog
                    .function_by_key(&function_key(provider_scope("math"), "make"))
                    .unwrap(),
            );

            assert!(matches!(function.signature.ret.ty, Type::Nominal(_)));
        }

        #[test]
        fn provider_absolute_type_uses_source_export() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "make".to_string(),
                    doc: None,
                    signature: ext_signature(vec![], module_named(&["geom"], "Point")),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[("geom", "pub struct Point { x: int }")], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let function = catalog.function(
                catalog
                    .function_by_key(&function_key(provider_scope("host"), "make"))
                    .unwrap(),
            );

            assert_eq!(
                function.signature.ret.ty,
                Type::nominal(
                    NominalKind::Struct,
                    ident("Point"),
                    vec![],
                    vec![],
                    Some(vec!["geom".to_string()].into())
                )
            );
        }

        #[test]
        fn provider_local_ignores_imports() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "bad".to_string(),
                    doc: None,
                    signature: ext_signature(vec![], named("Point")),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls(
                "",
                &[
                    ("host", "import geom { Point };"),
                    ("geom", "pub struct Point { x: int }"),
                ],
                &raw,
            );
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::UnknownType { name, .. }) if *name == ident("Point")
            ));
        }

        #[test]
        fn provider_local_uses_merged_source() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "make".to_string(),
                    doc: None,
                    signature: ext_signature(vec![], named("Local")),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[("host", "pub struct Local { x: int }")], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let function = catalog.function(
                catalog
                    .function_by_key(&function_key(provider_scope("host"), "make"))
                    .unwrap(),
            );

            assert_eq!(
                function.signature.ret.ty,
                Type::nominal(
                    NominalKind::Struct,
                    ident("Local"),
                    vec![],
                    vec![],
                    Some(vec!["host".to_string()].into())
                )
            );
        }

        #[test]
        fn source_rejects_absolute_provider_path() {
            let raw = RawExterns {
                groups: vec![crate::externs::raw::RawExternGroup {
                    provenance: ExternProvenance::Source {
                        module: raw_root_scope(),
                    },
                    modules: vec![RawExternModule {
                        scope: raw_root_scope(),
                        types: vec![],
                        functions: vec![RawExternFunction {
                            decl: ExternFunctionDescriptor {
                                name: "bad".to_string(),
                                doc: None,
                                signature: ext_signature(
                                    vec![],
                                    module_named(&["geom", "types"], "Point"),
                                ),
                                effects: ExternEffects::default(),
                            },
                            exported: true,
                            site: RawExternSite::default(),
                        }],
                    }],
                }],
            };
            let root = parse("");
            let resolved =
                resolved_modules(&root, &[("geom.types", "pub struct Point { x: int }")]);
            let decls = DeclarationIndex::from_root_and_modules(&root, &resolved, &raw);
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::UnknownType {
                    name,
                    module: Some(ModuleScope::Named(module)),
                    ..
                }) if *name == ident("Point") && module.segments() == ["geom", "types"]
            ));
        }

        #[test]
        fn source_type_uses_import_alias() {
            let raw = source_raw(
                "import geom as g; extern fn make() -> g.Point;",
                &[("geom", "pub struct Point { x: int }")],
            );
            let decls = decls(
                "import geom as g; extern fn make() -> g.Point;",
                &[("geom", "pub struct Point { x: int }")],
                &raw,
            );
            let catalog = build_catalog(raw, &decls).unwrap();
            let function = catalog.function(
                catalog
                    .function_by_key(&function_key(ModuleScope::Root, "make"))
                    .unwrap(),
            );

            assert_eq!(
                function.signature.ret.ty,
                Type::nominal(
                    NominalKind::Struct,
                    ident("Point"),
                    vec![],
                    vec![],
                    Some(vec!["geom".to_string()].into())
                )
            );
        }

        #[test]
        fn source_option_name_uses_visible_type_lookup() {
            let source = "struct Option<T> { value: T } extern fn maybe() -> Option<int>;";
            let raw = source_raw(source, &[]);
            let decls = decls(source, &[], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let function = catalog.function(
                catalog
                    .function_by_key(&function_key(ModuleScope::Root, "maybe"))
                    .unwrap(),
            );

            assert_eq!(
                function.signature.ret.ty,
                Type::nominal(
                    NominalKind::Struct,
                    ident("Option"),
                    vec![Type::Int],
                    vec![],
                    None,
                )
            );
        }

        #[test]
        fn self_resolves_in_members() {
            let source = "extern type Handle { next: Self; init; fn same(self, other: Self) -> Self; fn make() -> Self; op Self + Self -> Self; op - Self -> Self; }";
            let raw = source_raw(source, &[]);
            let decls = decls(source, &[], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let owner = catalog
                .type_by_key(&type_key(ModuleScope::Root, "Handle"))
                .unwrap();
            let self_ty = Type::nominal(NominalKind::Extern, ident("Handle"), vec![], vec![], None);

            assert_eq!(
                catalog.field(owner, ident("next")).unwrap().1.ty.ty,
                self_ty
            );
            assert!(catalog.init(owner).is_some());
            assert_eq!(
                catalog
                    .method(owner, ident("same"))
                    .unwrap()
                    .1
                    .signature
                    .params[0]
                    .ty
                    .ty,
                self_ty
            );
            assert_eq!(
                catalog
                    .method(owner, ident("same"))
                    .unwrap()
                    .1
                    .signature
                    .ret
                    .ty,
                self_ty
            );
            assert_eq!(
                catalog
                    .static_method(owner, ident("make"))
                    .unwrap()
                    .1
                    .signature
                    .ret
                    .ty,
                self_ty
            );
            assert_eq!(
                catalog
                    .binary_operator(owner, BinaryOp::Add, false)
                    .unwrap()
                    .1
                    .signature
                    .params[0]
                    .ty
                    .ty,
                self_ty
            );
            assert_eq!(
                catalog
                    .unary_operator(owner, UnaryOp::Neg)
                    .unwrap()
                    .1
                    .signature
                    .ret
                    .ty,
                self_ty
            );
        }

        #[test]
        fn provider_callback_policy_sets_param_escape() {
            let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
                params: vec![cb_param(
                    ExternTypeExpr::Callback(ExternCallbackSignature {
                        params: vec![],
                        ret: Box::new(ExternTypeExpr::Void),
                        policy: CallbackPolicy {
                            escape: CallbackEscape::NonEscaping,
                            thread: CallbackThread::SameThread,
                        },
                    }),
                    CallbackEscape::NonEscaping,
                )],
                ret: Box::new(ExternTypeExpr::Void),
                policy: CallbackPolicy {
                    escape: CallbackEscape::Escaping,
                    thread: CallbackThread::SameThread,
                },
            });
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "use_cb".to_string(),
                    doc: None,
                    signature: ext_signature(
                        vec![ext_param("callback", callback)],
                        ExternTypeExpr::Void,
                    ),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let signature = &catalog
                .function(
                    catalog
                        .function_by_key(&function_key(provider_scope("host"), "use_cb"))
                        .unwrap(),
                )
                .signature;
            let Type::Func { params, .. } = &signature.params[0].ty.ty else {
                panic!("expected callback type");
            };

            assert_eq!(signature.params[0].escape, EscapeMode::Escaping);
            assert_eq!(params[0].escape, EscapeMode::NonEscaping);
        }

        #[test]
        fn recursive_containers_and_any_resolve() {
            let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
                params: vec![cb_param(ExternTypeExpr::Any, CallbackEscape::NonEscaping)],
                ret: Box::new(ExternTypeExpr::Option(Box::new(ExternTypeExpr::Int))),
                policy: CallbackPolicy {
                    escape: CallbackEscape::Escaping,
                    thread: CallbackThread::SameThread,
                },
            });
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "use_cb".to_string(),
                    doc: None,
                    signature: ext_signature(
                        vec![ext_param(
                            "callbacks",
                            ExternTypeExpr::List(Box::new(ExternTypeExpr::Map(
                                Box::new(ExternTypeExpr::String),
                                Box::new(callback),
                            ))),
                        )],
                        ExternTypeExpr::Void,
                    ),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let param = &catalog
                .function(
                    catalog
                        .function_by_key(&function_key(provider_scope("host"), "use_cb"))
                        .unwrap(),
                )
                .signature
                .params[0]
                .ty;

            assert!(param.contains_any());
            let Type::List { elem } = &param.ty else {
                panic!("expected list");
            };
            let Type::Map { value, .. } = elem.as_ref() else {
                panic!("expected map");
            };
            let Type::Func { params, ret } = value.as_ref() else {
                panic!("expected callback");
            };
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].ty, Type::Any);
            assert_eq!(ret.ty, decls.core_option_of(Type::Int).unwrap());
        }
    }

    mod validation {
        use super::*;

        #[test]
        fn unknown_provider_type_fails_catalog_build() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "bad".to_string(),
                    doc: None,
                    signature: ext_signature(vec![], named("Missing")),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[], &raw);
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::UnknownType { name, .. }) if *name == ident("Missing")
            ));
        }

        #[test]
        fn wrong_generic_arity_fails_catalog_build() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "bad".to_string(),
                    doc: None,
                    signature: ext_signature(
                        vec![],
                        ExternTypeExpr::Named {
                            module: None,
                            name: "Box".to_string(),
                            args: vec![],
                        },
                    ),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[("host", "pub struct Box<T> { value: T }")], &raw);
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::GenericArity { name, expected: 1, found: 0, .. })
                    if *name == ident("Box")
            ));
        }

        #[test]
        fn rejects_type_const_arg() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "bad".to_string(),
                    doc: None,
                    signature: ext_signature(
                        vec![],
                        ExternTypeExpr::Named {
                            module: None,
                            name: "ArrayBox".to_string(),
                            args: vec![ExternTypeExpr::Int],
                        },
                    ),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls(
                "",
                &[("host", "pub struct ArrayBox<N: int> { value: [int; N] }")],
                &raw,
            );
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::GenericArgKindMismatch { name, expected: "const", .. })
                    if *name == ident("ArrayBox")
            ));
        }

        #[test]
        fn init_params_fail() {
            let source = "extern type Handle { init(x: int); }";
            let raw = source_raw(source, &[]);
            let decls = decls(source, &[], &raw);
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::UnsupportedInitParams {
                    context: ExternCatalogContext {
                        item: ExternContextItem::Init { ty, .. },
                        ..
                    },
                    count: 1,
                    ..
                }) if *ty == ident("Handle")
            ));
        }

        #[test]
        fn init_unknown_field_fails() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![ExternTypeDescriptor {
                    init: Some(ExternInitDescriptor {
                        params: vec![],
                        field_init: vec!["missing".to_string()],
                    }),
                    ..descriptor_type("Handle")
                }],
                functions: vec![],
            });
            let decls = decls("", &[], &raw);
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::UnknownInitField {
                    context: ExternCatalogContext {
                        item: ExternContextItem::Init { ty, .. },
                        ..
                    },
                    field,
                    ..
                }) if *ty == ident("Handle") && *field == ident("missing")
            ));
        }

        #[test]
        fn init_computed_field_fails() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![ExternFieldDescriptor {
                        name: "x".to_string(),
                        ty: ExternTypeExpr::Int,
                        computed: true,
                        doc: None,
                    }],
                    init: Some(ExternInitDescriptor {
                        params: vec![],
                        field_init: vec!["x".to_string()],
                    }),
                    ..descriptor_type("Handle")
                }],
                functions: vec![],
            });
            let decls = decls("", &[], &raw);
            let errors = build_catalog(raw, &decls).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::ComputedInitField {
                    context: ExternCatalogContext {
                        item: ExternContextItem::Init { ty, .. },
                        ..
                    },
                    field,
                    ..
                }) if *ty == ident("Handle") && *field == ident("x")
            ));
        }

        #[test]
        fn init_plain_field_succeeds() {
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![ExternFieldDescriptor {
                        name: "x".to_string(),
                        ty: ExternTypeExpr::Int,
                        computed: false,
                        doc: None,
                    }],
                    init: Some(ExternInitDescriptor {
                        params: vec![],
                        field_init: vec!["x".to_string()],
                    }),
                    ..descriptor_type("Handle")
                }],
                functions: vec![],
            });
            let decls = decls("", &[], &raw);

            assert!(build_catalog(raw, &decls).is_ok());
        }

        #[test]
        fn comparison_operator_requires_bool() {
            let mut builder = CatalogBuilder::default();
            let module = builder.module(ModuleScope::Root);
            let ty = builder.ty(
                module,
                type_key(ModuleScope::Root, "Vec2"),
                nominal(ModuleScope::Root, "Vec2"),
            );
            let op = builder.operator(
                ty,
                ExternOperator::Binary {
                    op: BinaryOp::Eq,
                    self_on_right: false,
                },
            );
            builder.operator_ret(ty, op, Type::Int);
            let errors = validate_catalog(&builder.finish()).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::InvalidOperatorReturn {
                    context: ExternCatalogContext {
                        item: ExternContextItem::Operator { ty, .. },
                        ..
                    },
                    expected: OperatorReturn::Bool,
                    ..
                }) if *ty == ident("Vec2")
            ));
        }

        #[test]
        fn arithmetic_operator_rejects_void() {
            let mut builder = CatalogBuilder::default();
            let module = builder.module(ModuleScope::Root);
            let ty = builder.ty(
                module,
                type_key(ModuleScope::Root, "Vec2"),
                nominal(ModuleScope::Root, "Vec2"),
            );
            let op = builder.operator(ty, ExternOperator::Unary(UnaryOp::Neg));
            builder.operator_ret(ty, op, Type::Void);
            let errors = validate_catalog(&builder.finish()).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::InvalidOperatorReturn {
                    context: ExternCatalogContext {
                        item: ExternContextItem::Operator { ty, .. },
                        ..
                    },
                    expected: OperatorReturn::NonVoid,
                    ..
                }) if *ty == ident("Vec2")
            ));
        }

        #[test]
        fn nested_callback_names() {
            let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
                params: vec![cb_param(named("Handle"), CallbackEscape::NonEscaping)],
                ret: Box::new(ExternTypeExpr::Option(Box::new(named("Handle")))),
                policy: CallbackPolicy {
                    escape: CallbackEscape::NonEscaping,
                    thread: CallbackThread::SameThread,
                },
            });
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![descriptor_type("Handle")],
                functions: vec![ExternFunctionDescriptor {
                    name: "listen".to_string(),
                    doc: None,
                    signature: ext_signature(vec![ext_param("cb", callback)], ExternTypeExpr::Void),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[], &raw);

            assert!(build_catalog(raw, &decls).is_ok());
        }

        #[test]
        fn unresolved_helper_type_fails_validation() {
            let mut builder = CatalogBuilder::default();
            let module = builder.module(ModuleScope::Root);
            let ty = builder.ty(
                module,
                type_key(ModuleScope::Root, "Handle"),
                nominal(ModuleScope::Root, "Handle"),
            );
            let field = builder.field(ty, ident("x"));
            builder.field_ty(ty, field, Type::UnresolvedName(ident("Missing")));
            let errors = validate_catalog(&builder.finish()).unwrap_err();

            assert!(matches!(
                errors.first(),
                Some(ExternCatalogError::InvalidType {
                    reason: InvalidExternTypeReason::Unresolved,
                    ..
                })
            ));
        }

        #[test]
        fn any_survives_catalog_validation() {
            let callback = ExternTypeExpr::Callback(ExternCallbackSignature {
                params: vec![cb_param(ExternTypeExpr::Any, CallbackEscape::NonEscaping)],
                ret: Box::new(ExternTypeExpr::Any),
                policy: CallbackPolicy {
                    escape: CallbackEscape::Escaping,
                    thread: CallbackThread::SameThread,
                },
            });
            let raw = provider_raw(ExternModuleDescriptor {
                path: extern_module(&["host"]),
                types: vec![ExternTypeDescriptor {
                    fields: vec![ExternFieldDescriptor {
                        name: "value".to_string(),
                        ty: ExternTypeExpr::Any,
                        computed: false,
                        doc: None,
                    }],
                    ..descriptor_type("Box")
                }],
                functions: vec![ExternFunctionDescriptor {
                    name: "pass".to_string(),
                    doc: None,
                    signature: ext_signature(
                        vec![
                            ext_param("value", ExternTypeExpr::Any),
                            ext_param("cb", callback),
                        ],
                        ExternTypeExpr::Any,
                    ),
                    effects: ExternEffects::default(),
                }],
            });
            let decls = decls("", &[], &raw);
            let catalog = build_catalog(raw, &decls).unwrap();
            let function = catalog.function(
                catalog
                    .function_by_key(&function_key(provider_scope("host"), "pass"))
                    .unwrap(),
            );
            let ty = catalog.ty(catalog
                .type_by_key(&type_key(provider_scope("host"), "Box"))
                .unwrap());

            assert!(function.signature.params[0].ty.contains_any());
            assert!(function.signature.params[1].ty.contains_any());
            assert!(function.signature.ret.contains_any());
            assert!(ty.fields[0].ty.contains_any());
        }
    }

    mod lookups {
        use super::*;

        #[test]
        fn ids_retrieve_inserted_declarations() {
            let mut builder = CatalogBuilder::default();
            let module_scope = scope("math");
            let module = builder.module(module_scope.clone());
            let ty_key = type_key(module_scope.clone(), "Vec2");
            let nominal = nominal(module_scope.clone(), "Vec2");
            let ty = builder.ty(module, ty_key.clone(), nominal.clone());
            let function_key = function_key(module_scope, "dot");
            let function = builder.function(module, function_key.clone());
            let catalog = builder.finish();

            assert_eq!(catalog.module(module).id, module);
            assert_eq!(catalog.ty(ty).id, ty);
            assert_eq!(catalog.function(function).id, function);
            assert_eq!(catalog.module(module).types, vec![ty]);
            assert_eq!(catalog.module(module).functions, vec![function]);
            assert_eq!(catalog.type_by_key(&ty_key), Some(ty));
            assert_eq!(catalog.type_by_nominal(&nominal), Some(ty));
            assert_eq!(catalog.function_by_key(&function_key), Some(function));
        }

        #[test]
        fn member_names_use_separate_lookup_spaces() {
            let mut builder = CatalogBuilder::default();
            let module_scope = ModuleScope::Root;
            let module = builder.module(module_scope.clone());
            let ty = builder.ty(
                module,
                type_key(module_scope.clone(), "Vec2"),
                nominal(module_scope, "Vec2"),
            );
            let name = ident("x");
            let field = builder.field(ty, name);
            let method = builder.method(ty, name);
            let static_method = builder.static_method(ty, name);
            builder.init(ty, vec![name]);
            let catalog = builder.finish();

            assert_eq!(
                catalog.field(ty, name).map(|(field_ref, _)| field_ref.id),
                Some(field)
            );
            assert_eq!(
                catalog
                    .method(ty, name)
                    .map(|(method_ref, _)| method_ref.id),
                Some(method)
            );
            assert_eq!(
                catalog
                    .static_method(ty, name)
                    .map(|(static_ref, _)| static_ref.id),
                Some(static_method)
            );
            assert_eq!(catalog.init(ty).unwrap().field_init, vec![name]);
        }

        #[test]
        fn operator_kinds_are_distinct() {
            let mut builder = CatalogBuilder::default();
            let module_scope = ModuleScope::Root;
            let module = builder.module(module_scope.clone());
            let ty = builder.ty(
                module,
                type_key(module_scope.clone(), "Vec2"),
                nominal(module_scope, "Vec2"),
            );
            let unary = builder.operator(ty, ExternOperator::Unary(UnaryOp::Neg));
            let add_left = builder.operator(
                ty,
                ExternOperator::Binary {
                    op: BinaryOp::Add,
                    self_on_right: false,
                },
            );
            let add_right = builder.operator(
                ty,
                ExternOperator::Binary {
                    op: BinaryOp::Add,
                    self_on_right: true,
                },
            );
            let catalog = builder.finish();

            assert_eq!(
                catalog
                    .unary_operator(ty, UnaryOp::Neg)
                    .map(|(operator_ref, _)| operator_ref.id),
                Some(unary)
            );
            assert_eq!(
                catalog
                    .binary_operator(ty, BinaryOp::Add, false)
                    .map(|(operator_ref, _)| operator_ref.id),
                Some(add_left)
            );
            assert_eq!(
                catalog
                    .binary_operator(ty, BinaryOp::Add, true)
                    .map(|(operator_ref, _)| operator_ref.id),
                Some(add_right)
            );
        }

        #[test]
        fn missing_lookups_return_none() {
            let mut builder = CatalogBuilder::default();
            let module_scope = ModuleScope::Root;
            let module = builder.module(module_scope.clone());
            let ty = builder.ty(
                module,
                type_key(module_scope.clone(), "Vec2"),
                nominal(module_scope, "Vec2"),
            );
            let catalog = builder.finish();

            assert_eq!(
                catalog.function_by_key(&function_key(ModuleScope::Root, "dot")),
                None
            );
            assert_eq!(
                catalog.type_by_key(&type_key(ModuleScope::Root, "Mat4")),
                None
            );
            assert_eq!(catalog.field(ty, ident("x")), None);
            assert_eq!(catalog.method(ty, ident("x")), None);
            assert_eq!(catalog.static_method(ty, ident("x")), None);
            assert!(catalog.init(ty).is_none());
            assert_eq!(catalog.unary_operator(ty, UnaryOp::Neg), None);
            assert_eq!(catalog.binary_operator(ty, BinaryOp::Add, false), None);
        }
    }
}
