use std::collections::{HashMap, HashSet};

use super::{
    annotations::AppliedAnnotations,
    const_eval::{ConstDef, ConstValue},
    constraint::{Constraint, TypeRef},
    error::{Diagnostic, DiagnosticKind},
    infer::{build_subst, subst_type},
    range::range_element_type,
    unify::{contains_infer, is_assignable, unify_equal},
    visit::{fold_type, type_any},
};
use crate::{
    ast::{
        AggregateKind, ArrayLen, BinaryOp, BlockNode, CallNode, ConstParam, EnumDecl, ExprId,
        ExtendMethodNode, FieldAccessNode, FuncNode, FuncParam, Ident, IndexNode, Method,
        MethodReceiver, ModulePath, Mutability, Param, StmtNode, StructDecl, StructField, Type,
        TypeParam, TypeVarId, UnaryOp, VariantKind,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub(super) struct EnumVariantDef {
    pub name: Ident,
    pub kind: VariantKind,
    pub annotations: AppliedAnnotations,
    pub field_annotations: Option<HashMap<Ident, AppliedAnnotations>>,
}

#[derive(Debug, Clone)]
pub(super) struct EnumDef {
    pub(super) defining_module: Option<ModulePath>,
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<EnumVariantDef>,
    pub annotations: AppliedAnnotations,
}

impl EnumDef {
    pub(super) fn from_ast(decl: &EnumDecl) -> Self {
        let variants = decl
            .variants
            .iter()
            .map(|v| EnumVariantDef {
                name: v.name,
                kind: v.kind.clone(),
                annotations: AppliedAnnotations::default(),
                field_annotations: None,
            })
            .collect();
        Self {
            defining_module: None,
            type_params: decl.type_params.clone(),
            variants,
            annotations: AppliedAnnotations::default(),
        }
    }

    pub(super) fn check_deprecation(
        &self,
        enum_name: Ident,
        variant: &EnumVariantDef,
        span: Span,
        errors: &mut Vec<Diagnostic>,
    ) {
        self.annotations
            .check_deprecation(span, "enum", enum_name, errors);
        variant
            .annotations
            .check_deprecation(span, "variant", variant.name, errors);
    }

    pub fn make_type(&self, name: Ident, type_args: Vec<Type>) -> Type {
        Type::Enum {
            name,
            type_args,
            origin: self.defining_module.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodDef {
    pub type_params: Vec<TypeParam>,
    pub receiver: Option<MethodReceiver>,
    pub params: Vec<Param>,
    pub param_defaults: Vec<Option<ConstValue>>,
    pub ret: Type,
    pub body: BlockNode,
    pub(super) annotations: AppliedAnnotations,
}

impl MethodDef {
    pub(super) fn from_ast(method: &Method) -> (Ident, Self) {
        (
            method.name,
            Self {
                type_params: method.type_params.clone(),
                receiver: method.receiver,
                params: method.params.clone(),
                param_defaults: vec![],
                ret: method.ret.clone(),
                body: method.body.clone(),
                annotations: AppliedAnnotations::default(),
            },
        )
    }
}

#[derive(Debug, Clone)]
pub enum FieldDefault {
    Const(ConstValue),
    EmptyArray,
    EmptyMap,
}

pub(super) fn type_references_generic(ty: &Type, type_params: &[TypeParam]) -> bool {
    type_any(
        ty,
        &mut |t| matches!(t, Type::Var(id) if type_params.iter().any(|p| p.id == *id)),
    )
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub(super) defining_module: Option<ModulePath>,
    pub kind: AggregateKind,
    pub span: Span,
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<StructField>,
    pub methods: HashMap<Ident, MethodDef>,
    pub field_defaults: HashMap<Ident, FieldDefault>,
    pub(super) annotations: AppliedAnnotations,
    pub(super) field_annotations: HashMap<Ident, AppliedAnnotations>,
}

impl StructDef {
    pub(super) fn from_ast(decl: &StructDecl, span: Span) -> Self {
        let methods = decl.methods.iter().map(MethodDef::from_ast).collect();
        Self {
            defining_module: None,
            kind: decl.kind,
            span,
            type_params: decl.type_params.clone(),
            fields: decl.fields.clone(),
            methods,
            field_defaults: HashMap::new(),
            annotations: AppliedAnnotations::default(),
            field_annotations: HashMap::new(),
        }
    }

    pub fn make_type(&self, name: Ident, type_args: Vec<Type>) -> Type {
        self.kind
            .make_type(name, type_args, self.defining_module.clone())
    }
}

#[derive(Debug, Clone)]
pub struct ExternFieldDef {
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct ExternMethodDef {
    pub receiver: Option<MethodReceiver>,
    pub params: Vec<Param>,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct ExternOpDef {
    pub op: BinaryOp,
    pub other_ty: Type,
    pub ret: Type,
    pub self_on_right: bool,
}

#[derive(Debug, Clone)]
pub struct ExternUnaryOpDef {
    pub op: UnaryOp,
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub struct ExternTypeDef {
    pub(super) defining_module: Option<ModulePath>,
    pub has_init: bool,
    pub field_order: Vec<Ident>,
    pub fields: HashMap<Ident, ExternFieldDef>,
    pub methods: HashMap<Ident, ExternMethodDef>,
    pub statics: HashMap<Ident, ExternMethodDef>,
    pub operators: Vec<ExternOpDef>,
    pub unary_operators: Vec<ExternUnaryOpDef>,
}

impl ExternTypeDef {
    pub fn make_type(&self, name: Ident) -> Type {
        Type::Extern {
            name,
            origin: self.defining_module.clone(),
        }
    }

    pub fn as_struct_fields(&self) -> Vec<StructField> {
        self.field_order
            .iter()
            .map(|name| {
                let def = &self.fields[name];
                StructField {
                    annotations: vec![],
                    name: *name,
                    ty: def.ty.clone(),
                    default: None,
                    doc: None,
                }
            })
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct CastEntry {
    pub internal_name: Ident,
    pub source_type: Type,
    pub target_type: Type,
}

#[derive(Debug, Clone)]
pub(super) struct ExtendMethodDef {
    pub params: Vec<Param>, // full param list including self
    pub ret: Type,
    pub internal_name: Ident,
    pub annotations: AppliedAnnotations,
}

#[derive(Debug, Clone)]
pub(super) struct ExtendEntry {
    pub source_module: Vec<String>,
    pub binding: Ident,
    pub def: ExtendMethodDef,
}

#[derive(Debug, Clone)]
pub struct ModuleExtendEntry {
    pub ty: Type,
    pub name: Ident,
    pub def: ExtendMethodDef,
}

#[derive(Debug, Clone)]
pub struct GenericExtendTemplate {
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub target_type: Type,
    pub method: ExtendMethodNode,
    pub source_module: Vec<String>,
    pub binding: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtendSpecKey {
    pub base_name: Ident,
    pub method_name: Ident,
    pub type_args: Vec<Type>,
    pub const_args: Vec<usize>,
    pub target_type: Type,
}

#[derive(Debug, Clone)]
pub struct ModuleGenericExtendEntry {
    pub base_name: Ident,
    pub type_params: Vec<TypeParam>,
    pub const_params: Vec<ConstParam>,
    pub target_type: Type,
    pub method_name: Ident,
    pub method: ExtendMethodNode,
}

pub(super) type InferenceSlots = HashMap<TypeVarId, Ident>;

#[derive(Debug, Clone, PartialEq)]
pub(super) struct RetType {
    pub ty: Type,
    pub has_explicit: bool,
    pub span: Option<Span>,
}

#[derive(Debug, Clone)]
pub(super) struct MethodContext {
    pub struct_name: Ident,
    pub receiver: Option<MethodReceiver>,
}

/// Key for caching specialized generic functions (instantiated with concrete types)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SpecializationKey {
    pub func_name: Ident,
    pub type_args: Vec<Type>,
    pub const_args: Vec<usize>,
}

/// Key for caching specialized generic method bodies
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodSpecKey {
    pub struct_name: Ident,
    pub method_name: Ident,
    /// Struct type args concatenated with method type args
    pub type_args: Vec<Type>,
    pub const_args: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct SpecializationResult {
    pub ret_ty: Type,
    pub err: Option<(Span, DiagnosticKind)>,
    pub body_types: HashMap<ExprId, (Span, Type)>,
    pub binding_types: HashMap<ExprId, Type>,
}

#[derive(Debug, Clone)]
pub(super) enum SpecializationState {
    InProgress,
    Done(SpecializationResult),
}

#[derive(Debug)]
pub(super) struct InstantiationContext<'a> {
    pub(super) module_env: Option<&'a ModuleCheckContext>,
    pub(super) params: Vec<(Ident, Type, bool)>,
    pub(super) ret_ty: Type,
    pub(super) method_ctx: Option<MethodContext>,
    pub(super) type_subst: Vec<(Ident, Type)>,
}

#[derive(Debug)]
pub(super) struct TypedBodyResult {
    pub(super) ret_ty: Type,
    pub(super) body_types: HashMap<ExprId, (Span, Type)>,
    pub(super) binding_types: HashMap<ExprId, Type>,
    pub(super) first_error: Option<(Span, DiagnosticKind)>,
}

impl TypedBodyResult {
    pub(super) fn into_spec_result(self) -> SpecializationResult {
        SpecializationResult {
            ret_ty: self.ret_ty,
            err: self.first_error,
            body_types: self.body_types,
            binding_types: self.binding_types,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct VarInfo {
    pub ty: Type,
    pub mutable: bool,
    pub is_function: bool,
}

#[derive(Debug, Clone, Default)]
pub(super) struct ModuleDef {
    pub source_path: Vec<String>,
    pub funcs: HashMap<Ident, Type>,
    pub func_param_info: HashMap<Ident, Vec<(Ident, Mutability)>>,
    pub func_cast_accept: HashMap<Ident, Vec<bool>>,
    pub struct_defs: HashMap<Ident, StructDef>,
    pub enum_defs: HashMap<Ident, EnumDef>,
    pub extern_types: HashMap<Ident, ExternTypeDef>,
    pub func_type_params: HashMap<Ident, Vec<TypeParam>>,
    pub func_const_params: HashMap<Ident, Vec<ConstParam>>,
    pub generic_func_templates: HashMap<Ident, FuncNode>,

    //  all top-level declaration names (public and private) for private vs missing checks
    pub all_names: HashSet<Ident>,

    /// sub modules re-exported via `pub import X;` or `pub import X as alias;`
    pub re_exported_modules: HashMap<Ident, ModuleDef>,

    /// public const definitions exported by this module
    pub const_defs: HashMap<Ident, ConstDef>,

    /// extend methods exported by this module
    pub extend_methods: Vec<ModuleExtendEntry>,

    /// generic extend method templates exported by this module
    pub generic_extend_methods: Vec<ModuleGenericExtendEntry>,

    /// evaluated parameter defaults for exported functions, parallel to params (None = required)
    pub func_param_defaults: HashMap<Ident, Vec<Option<ConstValue>>>,

    /// cast_from entries exported by this module
    pub cast_entries: Vec<CastEntry>,
}

impl ModuleDef {
    /// iterator over all publicly visible symbols (funcs + structs + enums)
    pub fn all_public_names(&self) -> impl Iterator<Item = Ident> + '_ {
        self.funcs
            .keys()
            .chain(self.struct_defs.keys())
            .chain(self.enum_defs.keys())
            .chain(self.extern_types.keys())
            .chain(self.const_defs.keys())
            .copied()
    }
}

#[derive(Debug, Clone, Default)]
pub struct ModuleCheckContext {
    pub(super) struct_defs: HashMap<Ident, StructDef>,
    pub(super) enum_defs: HashMap<Ident, EnumDef>,
    pub(super) extern_type_defs: HashMap<Ident, ExternTypeDef>,
    pub(super) const_defs: HashMap<Ident, ConstDef>,
    pub(super) module_defs: HashMap<Ident, ModuleDef>,
    pub(super) extend_defs: HashMap<(Type, Ident), Vec<ExtendEntry>>,
    pub(super) generic_extend_templates: HashMap<(Ident, Ident), Vec<GenericExtendTemplate>>,
    pub(super) cast_defs: HashMap<(Type, Type), CastEntry>,
    pub(super) module_path: Option<Vec<String>>,

    /// Function bindings saved for specialization, including private sibling functions
    pub(super) func_bindings: HashMap<Ident, VarInfo>,
}

#[derive(Debug, Default)]
pub(super) struct TypeChecker {
    /// Resolved type for each expression
    pub(super) types: HashMap<ExprId, (Span, Type)>,

    /// Effective type for each binding initializer expression
    pub(super) binding_types: HashMap<ExprId, Type>,

    /// Stack of scopes for variable lookup
    pub(super) scopes: Vec<HashMap<Ident, VarInfo>>,

    /// Stack of return types for function calls
    pub(super) return_types: Vec<RetType>,

    /// Stack tracking the current method context (if any)
    pub(super) method_contexts: Vec<MethodContext>,

    /// Type constraints to be resolved by inference pass
    pub(super) constraints: Vec<Constraint>,

    /// Generic type params declared for function
    pub(super) func_type_params: HashMap<Ident, Vec<TypeParam>>,

    /// Generic const params declared for function (N: int)
    pub(super) func_const_params: HashMap<Ident, Vec<ConstParam>>,

    /// Const params currently in scope for resolve_type (push/pop around generic decl processing)
    pub(super) current_const_params: Vec<ConstParam>,

    /// Type variable substitutions currently in scope for resolve_type (push/pop around generic body checking)
    pub(super) current_type_subst: Vec<(Ident, Type)>,

    /// Identify inference slots uniquely across multiple generic calls
    pub(super) next_infer_call_id: usize,

    /// Stores the generic function templates for later instantiation at call sites
    /// the bodies are checked when instantiated with concrete type arguments in a later pass
    pub(super) generic_func_templates: HashMap<Ident, FuncNode>,

    /// Stores specialized functions avoiding re-checking for same type arguments
    pub(super) specialization_cache: HashMap<SpecializationKey, SpecializationState>,

    /// Stores specialized generic method bodies avoiding re-checking for same type arguments
    pub(super) method_spec_cache: HashMap<MethodSpecKey, SpecializationResult>,

    /// Module-local namespace and import-sensitive checking environment
    pub(super) ctx: ModuleCheckContext,

    /// Stores param info for free functions
    pub(super) func_param_info: HashMap<Ident, Vec<(Ident, Mutability)>>,

    /// Stores cast_accept flags per param position, for functions with `as Type` params
    pub(super) func_cast_accept: HashMap<Ident, Vec<bool>>,

    /// Stores cast_accept flags for lambda expressions that have `as` params, keyed by ExprId
    pub(super) lambda_cast_accepts: HashMap<ExprId, Vec<bool>>,

    /// Records where the lowering pass must wrap an argument with a cast-from call
    pub(super) cast_call_insertions: HashMap<(ExprId, usize), (Ident, Type)>,

    /// Stores evaluated parameter defaults for free functions, parallel to params (None = required)
    pub(super) func_param_defaults: HashMap<Ident, Vec<Option<ConstValue>>>,

    /// Tracks depth of nested loops to validate break/continue usage
    pub(super) loop_depth: usize,

    /// True when type checking inside a defer body
    pub(super) in_defer: bool,

    /// Stmts from resolved imported modules, keyed by import path segments
    pub(super) resolved_module_stmts: HashMap<Vec<String>, Vec<StmtNode>>,

    /// Pre-built ModuleDefs for each resolved module, keyed by import path segments
    pub(super) resolved_module_defs: HashMap<Vec<String>, ModuleDef>,

    /// Full body-check environments for modules, keyed by import path segments
    pub(super) module_check_contexts: HashMap<Vec<String>, ModuleCheckContext>,

    /// Resolved type args and const args per call site, keyed by callee ExprId
    pub(super) resolved_call_type_args: HashMap<ExprId, (Ident, Vec<Type>, Vec<usize>)>,

    /// Tracks which module a generic function was imported from
    pub(super) generic_func_source_module: HashMap<Ident, Vec<String>>,

    /// Maps expression IDs to their const values for inlining by the lowering pass
    pub(super) const_values: HashMap<ExprId, ConstValue>,

    /// Maps pattern spans (start, end) to resolved const values for match-pattern lowering
    pub(super) const_pattern_values: HashMap<(usize, usize), ConstValue>,

    /// Tracks const names introduced per block scope inside function bodies
    pub(super) const_scope_stack: Vec<HashSet<Ident>>,

    /// Specialization cache for generic extend methods
    pub(super) extend_spec_cache: HashMap<ExtendSpecKey, SpecializationResult>,

    /// Maps call expression ExprId to resolved extend method internal_name (for lowering)
    pub(super) extend_call_targets: HashMap<ExprId, Ident>,

    /// Maps cast expression ExprId to the conversion function's internal_name (for lowering)
    pub(super) user_cast_targets: HashMap<ExprId, Ident>,

    /// Per-call-site ref mask for extend methods (which params are var)
    pub(super) extend_call_ref_masks: HashMap<ExprId, Vec<bool>>,

    /// Set of dataref type names that can form reference cycles
    pub(super) cycle_capable_types: HashSet<Ident>,

    /// Stack of scope depths marking lambda boundaries
    /// Each entry is the scope stack length when a lambda was entered
    pub(super) lambda_boundaries: Vec<usize>,

    /// Captures being collected for each active lambda (stack for nesting)
    /// Each entry maps captured variable names to their types
    pub(super) current_lambda_captures: Vec<HashMap<Ident, Type>>,

    /// Final capture lists, keyed by the lambda expression's ExprId
    /// Used by the lowering pass
    pub(super) lambda_captures: HashMap<ExprId, Vec<(Ident, Type)>>,

    /// Accumulated warnings (diagnostics with Severity::Warning)
    /// Populated during typechecking; reported after success
    pub(super) warnings: Vec<Diagnostic>,

    pub(super) lint: super::lint::LintConfig,

    /// Normalized annotations for free functions, keyed by function name
    pub(super) func_annotations: HashMap<Ident, AppliedAnnotations>,
}

#[derive(Debug)]
pub struct TypecheckResult {
    pub(super) types: HashMap<ExprId, (Span, Type)>,
    pub(super) binding_types: HashMap<ExprId, Type>,
    pub(super) struct_defs: HashMap<Ident, StructDef>,
    pub(super) enum_defs: HashMap<Ident, EnumDef>,
    pub(super) extern_type_defs: HashMap<Ident, ExternTypeDef>,
    pub(super) cycle_capable_types: HashSet<Ident>,
    pub(super) specialization_cache: HashMap<SpecializationKey, SpecializationResult>,
    pub(super) method_spec_cache: HashMap<MethodSpecKey, SpecializationResult>,
    pub(super) extend_spec_cache: HashMap<ExtendSpecKey, SpecializationResult>,
    pub(super) resolved_call_type_args: HashMap<ExprId, (Ident, Vec<Type>, Vec<usize>)>,
    pub(super) const_values: HashMap<ExprId, ConstValue>,
    pub(super) const_pattern_values: HashMap<(usize, usize), ConstValue>,
    pub(super) lambda_captures: HashMap<ExprId, Vec<(Ident, Type)>>,
    pub(super) extend_call_targets: HashMap<ExprId, Ident>,
    pub(super) extend_call_ref_masks: HashMap<ExprId, Vec<bool>>,
    pub(super) user_cast_targets: HashMap<ExprId, Ident>,
    pub(super) func_param_info: HashMap<Ident, Vec<(Ident, Mutability)>>,
    pub(super) cast_call_insertions: HashMap<(ExprId, usize), (Ident, Type)>,
    pub(super) func_param_defaults: HashMap<Ident, Vec<Option<ConstValue>>>,
    pub(super) module_defs: HashMap<Ident, ModuleDef>,
    pub(super) resolved_module_defs: HashMap<Vec<String>, ModuleDef>,
    pub(super) warnings: Vec<Diagnostic>,
    pub(super) generic_func_templates: HashMap<Ident, FuncNode>,
    pub(super) generic_extend_templates: HashMap<(Ident, Ident), Vec<GenericExtendTemplate>>,
    pub(super) module_check_contexts: HashMap<Vec<String>, ModuleCheckContext>,
}

pub(super) enum DeepLookup<'a, T> {
    Found(&'a T),
    NotFound,
    Ambiguous(String, String),
}

impl TypeChecker {
    pub(super) fn into_result(self) -> TypecheckResult {
        TypecheckResult {
            types: self.types,
            binding_types: self.binding_types,
            struct_defs: self.ctx.struct_defs,
            enum_defs: self.ctx.enum_defs,
            extern_type_defs: self.ctx.extern_type_defs,
            cycle_capable_types: self.cycle_capable_types,
            specialization_cache: self
                .specialization_cache
                .into_iter()
                .filter_map(|(key, state)| match state {
                    SpecializationState::InProgress => None,
                    SpecializationState::Done(result) => Some((key, result)),
                })
                .collect(),
            method_spec_cache: self.method_spec_cache,
            extend_spec_cache: self.extend_spec_cache,
            resolved_call_type_args: self.resolved_call_type_args,
            const_values: self.const_values,
            const_pattern_values: self.const_pattern_values,
            lambda_captures: self.lambda_captures,
            extend_call_targets: self.extend_call_targets,
            extend_call_ref_masks: self.extend_call_ref_masks,
            user_cast_targets: self.user_cast_targets,
            func_param_info: self.func_param_info,
            cast_call_insertions: self.cast_call_insertions,
            func_param_defaults: self.func_param_defaults,
            module_defs: self.ctx.module_defs,
            resolved_module_defs: self.resolved_module_defs,
            warnings: self.warnings,
            generic_func_templates: self.generic_func_templates,
            generic_extend_templates: self.ctx.generic_extend_templates,
            module_check_contexts: self.module_check_contexts,
        }
    }

    pub(super) fn next_call_id(&mut self) -> usize {
        let id = self.next_infer_call_id;
        self.next_infer_call_id += 1;
        id
    }

    pub(super) fn push_method_context(&mut self, ctx: MethodContext) {
        self.method_contexts.push(ctx);
    }

    pub(super) fn pop_method_context(&mut self) {
        self.method_contexts.pop();
    }

    pub(super) fn push_const_params(&mut self, params: &[ConstParam]) {
        self.current_const_params.extend_from_slice(params);
    }

    pub(super) fn pop_const_params(&mut self, count: usize) {
        self.current_const_params
            .truncate(self.current_const_params.len() - count);
    }

    pub(super) fn pop_type_subst(&mut self, count: usize) {
        self.current_type_subst
            .truncate(self.current_type_subst.len() - count);
    }

    pub(super) fn current_method(&self) -> Option<&MethodContext> {
        self.method_contexts.last()
    }

    pub(super) fn func_param_defaults(&self, name: Ident) -> &[Option<ConstValue>] {
        self.func_param_defaults
            .get(&name)
            .map_or(&[], Vec::as_slice)
    }

    pub(super) fn get_struct(&self, name: Ident) -> Option<&StructDef> {
        self.ctx.struct_defs.get(&name)
    }

    pub(super) fn get_enum(&self, name: Ident) -> Option<&EnumDef> {
        self.ctx.enum_defs.get(&name)
    }

    pub(super) fn get_const(&self, name: Ident) -> Option<&ConstDef> {
        self.ctx.const_defs.get(&name)
    }

    pub(super) fn get_extern_type(&self, name: Ident) -> Option<&ExternTypeDef> {
        self.ctx.extern_type_defs.get(&name)
    }

    fn find_in_modules<T>(&self, accessor: impl Fn(&ModuleDef) -> Option<&T>) -> DeepLookup<'_, T> {
        let mut found: Option<(&T, &[String])> = None;
        for (path, module_def) in &self.resolved_module_defs {
            if let Some(def) = accessor(module_def) {
                if let Some((_, prev_path)) = found {
                    return DeepLookup::Ambiguous(prev_path.join("."), path.join("."));
                }
                found = Some((def, path));
            }
        }
        match found {
            Some((def, _)) => DeepLookup::Found(def),
            None => DeepLookup::NotFound,
        }
    }

    fn get_deep<'a, T>(
        &'a self,
        name: Ident,
        origin: Option<&[String]>,
        local: impl FnOnce(Ident) -> Option<&'a T>,
        module_accessor: impl Fn(&ModuleDef) -> Option<&T>,
    ) -> DeepLookup<'a, T> {
        if let Some(def) = origin
            .and_then(|p| self.resolved_module_defs.get(p))
            .and_then(&module_accessor)
        {
            return DeepLookup::Found(def);
        }
        if let Some(def) = local(name) {
            return DeepLookup::Found(def);
        }
        self.find_in_modules(module_accessor)
    }

    pub(super) fn get_struct_deep(
        &self,
        name: Ident,
        origin: Option<&[String]>,
    ) -> DeepLookup<'_, StructDef> {
        self.get_deep(
            name,
            origin,
            |n| self.get_struct(n),
            |m| m.struct_defs.get(&name),
        )
    }

    pub(super) fn is_cross_module(&self, defining_module: Option<&[String]>) -> bool {
        self.ctx.module_path.as_deref() != defining_module
    }

    pub(super) fn check_member_access(
        &self,
        ann: &AppliedAnnotations,
        span: Span,
        kind: &'static str,
        name: Ident,
        type_name: Ident,
        defining_module: Option<&[String]>,
        errors: &mut Vec<Diagnostic>,
    ) {
        ann.check_deprecation(span, kind, name, errors);
        let cross_module = self.is_cross_module(defining_module);
        ann.check_internal(
            span,
            kind,
            name,
            type_name,
            cross_module,
            self.lint.internal_access,
            errors,
        );
    }

    pub(super) fn get_enum_deep(
        &self,
        name: Ident,
        origin: Option<&[String]>,
    ) -> DeepLookup<'_, EnumDef> {
        self.get_deep(
            name,
            origin,
            |n| self.get_enum(n),
            |m| m.enum_defs.get(&name),
        )
    }

    pub(super) fn get_extern_type_deep(
        &self,
        name: Ident,
        origin: Option<&[String]>,
    ) -> DeepLookup<'_, ExternTypeDef> {
        self.get_deep(
            name,
            origin,
            |n| self.get_extern_type(n),
            |m| m.extern_types.get(&name),
        )
    }

    pub(super) fn get_extend_methods(&self, ty: &Type, name: Ident) -> &[ExtendEntry] {
        self.ctx
            .extend_defs
            .get(&(ty.clone(), name))
            .map_or(&[], Vec::as_slice)
    }

    pub(super) fn get_module(&self, name: Ident) -> Option<&ModuleDef> {
        self.ctx.module_defs.get(&name)
    }

    pub(super) fn check_name_ambiguity(
        &self,
        name: Ident,
        span: Span,
        errors: &mut Vec<Diagnostic>,
    ) -> bool {
        fn push_if_ambiguous<T>(
            result: DeepLookup<'_, T>,
            name: Ident,
            span: Span,
            errors: &mut Vec<Diagnostic>,
        ) -> bool {
            if let DeepLookup::Ambiguous(first, second) = result {
                errors.push(Diagnostic::new(
                    span,
                    DiagnosticKind::AmbiguousType {
                        name,
                        first_module: first,
                        second_module: second,
                    },
                ));
                return true;
            }
            false
        }
        push_if_ambiguous(self.get_struct_deep(name, None), name, span, errors)
            || push_if_ambiguous(self.get_enum_deep(name, None), name, span, errors)
            || push_if_ambiguous(self.get_extern_type_deep(name, None), name, span, errors)
    }

    pub(super) fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::UnresolvedName(name) if self.ctx.extern_type_defs.contains_key(name) => {
                self.ctx.extern_type_defs[name].make_type(*name)
            }
            Type::UnresolvedName(name) if self.ctx.struct_defs.contains_key(name) => {
                self.ctx.struct_defs[name].make_type(*name, vec![])
            }
            Type::UnresolvedName(name) if self.ctx.enum_defs.contains_key(name) => {
                self.ctx.enum_defs[name].make_type(*name, vec![])
            }
            // the parser creates a struct for any named type with type args, this can be an enum or a struct
            Type::Struct {
                name, type_args, ..
            } if self.ctx.enum_defs.contains_key(name) => {
                let resolved = type_args.iter().map(|t| self.resolve_type(t)).collect();
                self.ctx.enum_defs[name].make_type(*name, resolved)
            }
            Type::Struct {
                name, type_args, ..
            } if self
                .ctx
                .struct_defs
                .get(name)
                .is_some_and(|d| d.kind.is_dataref()) =>
            {
                let resolved = type_args.iter().map(|t| self.resolve_type(t)).collect();
                self.ctx.struct_defs[name].make_type(*name, resolved)
            }
            Type::Struct {
                name,
                type_args,
                origin,
            } => {
                let resolved_origin = origin.clone().or_else(|| {
                    self.ctx
                        .struct_defs
                        .get(name)
                        .and_then(|d| d.defining_module.clone())
                });
                Type::Struct {
                    name: *name,
                    type_args: type_args.iter().map(|t| self.resolve_type(t)).collect(),
                    origin: resolved_origin,
                }
            }
            Type::DataRef {
                name,
                type_args,
                origin,
            } => {
                let resolved_origin = origin.clone().or_else(|| {
                    self.ctx
                        .struct_defs
                        .get(name)
                        .and_then(|d| d.defining_module.clone())
                });
                Type::DataRef {
                    name: *name,
                    type_args: type_args.iter().map(|t| self.resolve_type(t)).collect(),
                    origin: resolved_origin,
                }
            }
            Type::Enum {
                name,
                type_args,
                origin,
            } => {
                let resolved_origin = origin.clone().or_else(|| {
                    self.ctx
                        .enum_defs
                        .get(name)
                        .and_then(|d| d.defining_module.clone())
                });
                Type::Enum {
                    name: *name,
                    type_args: type_args.iter().map(|t| self.resolve_type(t)).collect(),
                    origin: resolved_origin,
                }
            }
            Type::Extern { name, origin } => {
                let resolved_origin = origin.clone().or_else(|| {
                    self.ctx
                        .extern_type_defs
                        .get(name)
                        .and_then(|d| d.defining_module.clone())
                });
                Type::Extern {
                    name: *name,
                    origin: resolved_origin,
                }
            }
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|t| self.resolve_type(t)).collect()),
            Type::NamedTuple(fields) => Type::NamedTuple(
                fields
                    .iter()
                    .map(|(n, t)| (*n, self.resolve_type(t)))
                    .collect(),
            ),
            Type::Func { params, ret } => Type::Func {
                params: params
                    .iter()
                    .map(|p| FuncParam::new(self.resolve_type(&p.ty), p.mutable))
                    .collect(),
                ret: Box::new(self.resolve_type(ret)),
            },
            Type::Array { elem, len } => {
                let resolved_len = match len {
                    ArrayLen::Named(ident) => {
                        if let Some(cp) = self
                            .current_const_params
                            .iter()
                            .find(|cp| cp.name == *ident)
                        {
                            ArrayLen::Param(cp.id)
                        } else {
                            match self.ctx.const_defs.get(ident) {
                                Some(def) => match &def.value {
                                    ConstValue::Int(n) if *n >= 0 => ArrayLen::Fixed(*n as usize),
                                    _ => ArrayLen::Named(*ident),
                                },
                                None => ArrayLen::Named(*ident),
                            }
                        }
                    }
                    other => *other,
                };
                Type::Array {
                    elem: self.resolve_type(elem).boxed(),
                    len: resolved_len,
                }
            }
            Type::Slice { elem } => Type::Slice {
                elem: self.resolve_type(elem).boxed(),
            },
            Type::List { elem } => Type::List {
                elem: self.resolve_type(elem).boxed(),
            },
            Type::Map { key, value } => Type::Map {
                key: self.resolve_type(key).boxed(),
                value: self.resolve_type(value).boxed(),
            },
            Type::UnresolvedName(name)
                if self
                    .current_type_subst
                    .iter()
                    .rev()
                    .any(|(n, _)| *n == *name) =>
            {
                let (_, concrete) = self
                    .current_type_subst
                    .iter()
                    .rev()
                    .find(|(n, _)| *n == *name)
                    .unwrap();
                self.resolve_type(&concrete.clone())
            }
            _ => ty.clone(),
        }
    }

    pub(super) fn resolve_type_with_module_fallback(&self, ty: &Type) -> Type {
        let resolved = self.resolve_type(ty);
        self.resolve_unresolved_from_modules(&resolved)
    }

    fn resolve_unresolved_from_modules(&self, ty: &Type) -> Type {
        fold_type(ty, &mut |t| {
            let Type::UnresolvedName(name) = t else {
                return t;
            };
            if let DeepLookup::Found(def) = self.find_in_modules(|m| m.struct_defs.get(&name)) {
                return def.make_type(name, vec![]);
            }
            if let DeepLookup::Found(def) = self.find_in_modules(|m| m.enum_defs.get(&name)) {
                return def.make_type(name, vec![]);
            }
            if let DeepLookup::Found(def) = self.find_in_modules(|m| m.extern_types.get(&name)) {
                return def.make_type(name);
            }
            Type::UnresolvedName(name)
        })
    }

    pub(super) fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub(super) fn collect_current_scope_bindings(&self) -> Vec<(Ident, Type, bool)> {
        let Some(scope) = self.scopes.last() else {
            return vec![];
        };
        scope
            .iter()
            .map(|(name, info)| (*name, info.ty.clone(), info.mutable))
            .collect()
    }

    pub(super) fn set_type(&mut self, id: ExprId, ty: Type, span: Span) {
        self.types.insert(id, (span, ty));
    }

    pub(super) fn set_binding_type(&mut self, id: ExprId, ty: Type) {
        self.binding_types.insert(id, ty);
    }

    pub(super) fn get_type(&self, id: ExprId) -> Option<&(Span, Type)> {
        self.types.get(&id)
    }

    pub(super) fn types(&self) -> impl Iterator<Item = (&ExprId, &(Span, Type))> {
        self.types.iter()
    }

    pub(super) fn store_extend_ref_mask(&mut self, call_id: ExprId, params: &[Param]) {
        let ref_mask = params
            .iter()
            .map(|p| p.mutability == Mutability::Mutable)
            .collect();
        self.extend_call_ref_masks.insert(call_id, ref_mask);
    }

    pub(super) fn set_var(&mut self, name: Ident, ty: Type, mutable: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                VarInfo {
                    ty,
                    mutable,
                    is_function: false,
                },
            );
        }
    }

    pub(super) fn set_func_var(&mut self, name: Ident, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(
                name,
                VarInfo {
                    ty,
                    mutable: false,
                    is_function: true,
                },
            );
        }
    }

    pub(super) fn enter_loop(&mut self) {
        self.loop_depth += 1;
    }

    pub(super) fn exit_loop(&mut self) {
        self.loop_depth = self.loop_depth.saturating_sub(1);
    }

    pub(super) fn in_loop(&self) -> bool {
        self.loop_depth > 0
    }

    pub(super) fn get_var(&self, name: Ident) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(&name) {
                return Some(info);
            }
        }
        None
    }

    pub(super) fn get_var_with_scope_depth(&self, name: Ident) -> Option<(usize, &VarInfo)> {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(info) = scope.get(&name) {
                return Some((i, info));
            }
        }
        None
    }

    pub(super) fn is_captured_var(&self, name: Ident) -> bool {
        self.current_lambda_captures
            .last()
            .is_some_and(|captures| captures.contains_key(&name))
    }

    pub(super) fn track_capture(&mut self, name: Ident) {
        let Some(&boundary) = self.lambda_boundaries.last() else {
            return;
        };
        let Some((scope_depth, info)) = self.get_var_with_scope_depth(name) else {
            return;
        };
        // builtins, user fns, extern fns, and imports are resolved
        // by name in lowering not through captures.
        if info.is_function {
            return;
        }
        let ty = info.ty.clone();
        if scope_depth < boundary {
            // We add this capture to every enclosing lambda not just the one that mentions
            // the variable directly. Nested closures need the whole chain to carry the
            // value so the outer closure can pass it into the inner one
            for (i, &lambda_boundary) in self.lambda_boundaries.iter().enumerate().rev() {
                if scope_depth < lambda_boundary {
                    if let Some(captures) = self.current_lambda_captures.get_mut(i) {
                        captures.entry(name).or_insert(ty.clone());
                    }
                } else {
                    break;
                }
            }
        }
    }

    pub(super) fn push_return_type(&mut self, ty: Type, span: Option<Span>) {
        self.return_types.push(RetType {
            ty,
            has_explicit: false,
            span,
        });
    }

    pub(super) fn pop_return_type(&mut self) {
        self.return_types.pop();
    }

    pub(super) fn current_return_type(&self) -> Option<&Type> {
        self.return_types.last().map(|r| &r.ty)
    }

    pub(super) fn mark_explicit_return(&mut self) {
        if let Some(ret_ty) = self.return_types.last_mut() {
            ret_ty.has_explicit = true;
        }
    }

    pub(super) fn has_explicit_return(&self) -> bool {
        self.return_types.last().is_some_and(|r| r.has_explicit)
    }

    pub(super) fn add_constraint(&mut self, span: Span, left: TypeRef, right: TypeRef) {
        self.constraints.push(Constraint { span, left, right });
    }

    pub(super) fn get_type_ref(&self, r: &TypeRef) -> Option<Type> {
        match r {
            TypeRef::Expr(id) => self.get_type(*id).map(|(_, ty)| ty.clone()),
            TypeRef::Var(ident) => self.get_var(*ident).map(|info| info.ty.clone()),
            TypeRef::Concrete(t) => Some(t.clone()),
        }
    }

    pub(super) fn set_type_ref(&mut self, r: &TypeRef, ty: Type, span: Span) {
        match r {
            TypeRef::Expr(id) => self.set_type(*id, ty, span),
            TypeRef::Var(ident) => self.set_var(*ident, ty, true),
            TypeRef::Concrete(_) => {} // Cannot write to concrete types
        }
    }

    /// Constrains two types that must be the same
    /// ie: let x:int = 10; x += 10; (int to int)
    pub(super) fn constrain_equal(
        &mut self,
        span: Span,
        left: TypeRef,
        right: TypeRef,
        errors: &mut Vec<Diagnostic>,
    ) {
        // try to unify the types immediately
        let unified = unify_equal(self, span, &left, &right, errors);

        // otherwise add a constraint to be resolved later
        if !unified {
            self.add_constraint(span, left, right);
        }
    }

    /// Constraints 'value' to be assignable to 'target'
    /// ie: let x:int? = 10; fn() -> int? { return 10; } (T to T?)
    pub(super) fn constrain_assignable(
        &mut self,
        span: Span,
        from: TypeRef,
        to: TypeRef,
        errors: &mut Vec<Diagnostic>,
    ) {
        let from_ty = self.get_type_ref(&from).unwrap_or(Type::Infer);
        let to_ty = self.get_type_ref(&to).unwrap_or(Type::Infer);

        // if both types are resolved, check assignability and set the types
        let both_resolved = !(contains_infer(&from_ty) || contains_infer(&to_ty));
        if both_resolved {
            if !is_assignable(&from_ty, &to_ty) {
                errors.push(Diagnostic::new(
                    span,
                    DiagnosticKind::MismatchedTypes {
                        expected: to_ty.clone(),
                        found: from_ty.clone(),
                    },
                ));
            }
            self.set_type_ref(&to, to_ty, span);
            self.set_type_ref(&from, from_ty, span);
            return;
        }

        // at this point at least one type is unresolved
        // if both are options, constrain the inner types
        if from_ty.is_option() && to_ty.is_option() {
            let inner_from = from_ty.option_inner().cloned().unwrap_or(Type::Infer);
            let inner_to = to_ty.option_inner().cloned().unwrap_or(Type::Infer);
            let inner_from_ref = TypeRef::concrete(&inner_from);
            let inner_to_ref = TypeRef::concrete(&inner_to);
            self.constrain_equal(span, inner_from_ref, inner_to_ref, errors);
            self.set_type_ref(&from, Type::option_of(inner_to), span);
            return;
        }

        // if to is an option and from has inference, constrain from to the inner type of to
        if to_ty.is_option() && contains_infer(&from_ty) {
            let inner_to = to_ty.option_inner().cloned().unwrap_or(Type::Infer);
            self.constrain_equal(span, from, TypeRef::concrete(&inner_to), errors);
            return;
        }

        // optional values cannot be assigned to non-optional targets once the target is resolved
        let from_is_optional = from_ty.is_option();
        let to_is_optional = to_ty.is_option();
        if from_is_optional && !to_is_optional && !contains_infer(&to_ty) {
            errors.push(Diagnostic::new(
                span,
                DiagnosticKind::MismatchedTypes {
                    expected: to_ty.clone(),
                    found: from_ty.clone(),
                },
            ));
            return;
        }

        // single-element containers constrain inner element types
        #[allow(clippy::match_same_arms)]
        // each arm documents a distinct permitted coercion; likely to diverge as type rules evolve
        let inner_pair = match (&from_ty, &to_ty) {
            (Type::Array { elem: f, .. }, Type::Array { elem: t, .. }) => Some((f, t)),
            (Type::Slice { elem: f }, Type::Slice { elem: t }) => Some((f, t)),
            (Type::Array { elem: f, .. }, Type::Slice { elem: t }) => Some((f, t)),
            (Type::List { elem: f }, Type::Slice { elem: t }) => Some((f, t)),
            // skip when either side has Infer, constrain_equal handles unification
            (Type::List { elem: f }, Type::List { elem: t })
                if !contains_infer(f) && !contains_infer(t) =>
            {
                Some((f, t))
            }
            _ => None,
        };

        if let Some((from_elem, to_elem)) = inner_pair {
            let from_ref = TypeRef::concrete(from_elem);
            let to_ref = TypeRef::concrete(to_elem);
            self.constrain_assignable(span, from_ref, to_ref, errors);
            return;
        }

        // if both are maps, constrain key and value types
        // skip when any inner type has Infer, constrain_equal handles unification
        if let (Type::Map { key: kf, value: vf }, Type::Map { key: kt, value: vt }) =
            (&from_ty, &to_ty)
        {
            let has_infer = contains_infer(kf)
                || contains_infer(kt)
                || contains_infer(vf)
                || contains_infer(vt);
            if !has_infer {
                let kf_ref = TypeRef::concrete(kf);
                let kt_ref = TypeRef::concrete(kt);
                self.constrain_assignable(span, kf_ref, kt_ref, errors);
                let vf_ref = TypeRef::concrete(vf);
                let vt_ref = TypeRef::concrete(vt);
                self.constrain_assignable(span, vf_ref, vt_ref, errors);
                return;
            }
        }

        let type_arg_pairs = match (&from_ty, &to_ty) {
            (
                Type::Struct {
                    name: nf,
                    type_args: af,
                    ..
                },
                Type::Struct {
                    name: nt,
                    type_args: at,
                    ..
                },
            )
            | (
                Type::Enum {
                    name: nf,
                    type_args: af,
                    ..
                },
                Type::Enum {
                    name: nt,
                    type_args: at,
                    ..
                },
            )
            | (
                Type::DataRef {
                    name: nf,
                    type_args: af,
                    ..
                },
                Type::DataRef {
                    name: nt,
                    type_args: at,
                    ..
                },
            ) if nf == nt
                && af.len() == at.len()
                && !af.iter().any(contains_infer)
                && !at.iter().any(contains_infer) =>
            {
                Some(af.iter().zip(at.iter()))
            }
            _ => None,
        };

        if let Some(pairs) = type_arg_pairs {
            for (arg_from, arg_to) in pairs {
                let from_ref = TypeRef::concrete(arg_from);
                let to_ref = TypeRef::concrete(arg_to);
                self.constrain_assignable(span, from_ref, to_ref, errors);
            }
            return;
        }

        // otherwise just constrain them to be the same as fallback
        self.constrain_equal(span, from, to, errors);
    }
}

impl TypecheckResult {
    pub fn binding_type(&self, id: ExprId) -> Option<&Type> {
        self.binding_types.get(&id)
    }

    pub fn module_check_context(&self, path: &[String]) -> Option<&ModuleCheckContext> {
        self.module_check_contexts.get(path)
    }

    pub fn func_param_info(&self, name: Ident) -> &[(Ident, Mutability)] {
        self.func_param_info.get(&name).map_or(&[], Vec::as_slice)
    }

    pub fn func_param_defaults(&self, name: Ident) -> &[Option<ConstValue>] {
        self.func_param_defaults
            .get(&name)
            .map_or(&[], Vec::as_slice)
    }

    pub fn method_param_defaults(
        &self,
        struct_name: Ident,
        method_name: Ident,
    ) -> &[Option<ConstValue>] {
        self.struct_defs
            .get(&struct_name)
            .and_then(|sd| sd.methods.get(&method_name))
            .map_or(&[], |m| m.param_defaults.as_slice())
    }

    pub fn module_func_param_info(
        &self,
        module_name: Ident,
        func_name: Ident,
    ) -> &[(Ident, Mutability)] {
        self.module_defs
            .get(&module_name)
            .and_then(|m| m.func_param_info.get(&func_name))
            .map_or(&[], Vec::as_slice)
    }

    pub fn module_func_param_defaults(
        &self,
        module_name: Ident,
        func_name: Ident,
    ) -> &[Option<ConstValue>] {
        self.module_defs
            .get(&module_name)
            .and_then(|m| m.func_param_defaults.get(&func_name))
            .map_or(&[], Vec::as_slice)
    }

    fn find_in_module_defs<T>(&self, accessor: impl Fn(&ModuleDef) -> Option<&T>) -> Option<&T> {
        for module_def in self.resolved_module_defs.values() {
            if let Some(def) = accessor(module_def) {
                return Some(def);
            }
        }
        None
    }

    pub fn get_struct(&self, name: Ident) -> Option<&StructDef> {
        self.struct_defs
            .get(&name)
            .or_else(|| self.find_in_module_defs(|m| m.struct_defs.get(&name)))
    }

    pub(super) fn get_enum(&self, name: Ident) -> Option<&EnumDef> {
        self.enum_defs
            .get(&name)
            .or_else(|| self.find_in_module_defs(|m| m.enum_defs.get(&name)))
    }

    pub fn get_extern_type(&self, name: Ident) -> Option<&ExternTypeDef> {
        self.extern_type_defs
            .get(&name)
            .or_else(|| self.find_in_module_defs(|m| m.extern_types.get(&name)))
    }

    pub fn extern_type_field_order(&self, name: Ident) -> Option<&[Ident]> {
        self.get_extern_type(name)
            .map(|def| def.field_order.as_slice())
    }

    pub fn struct_names(&self) -> Vec<Ident> {
        let mut names: HashSet<Ident> = self.struct_defs.keys().copied().collect();
        for module_def in self.resolved_module_defs.values() {
            names.extend(module_def.struct_defs.keys().copied());
        }
        names.into_iter().collect()
    }

    pub fn aggregate_kind(&self, name: Ident) -> Option<AggregateKind> {
        self.get_struct(name).map(|d| d.kind)
    }

    pub fn is_cycle_capable(&self, name: Ident) -> bool {
        self.cycle_capable_types.contains(&name)
    }

    pub fn struct_field_names(&self, name: Ident) -> Option<Vec<Ident>> {
        self.get_struct(name)
            .map(|def| def.fields.iter().map(|f| f.name).collect())
    }

    pub fn struct_field_index(&self, struct_name: Ident, field_name: Ident) -> Option<usize> {
        self.get_struct(struct_name)?
            .fields
            .iter()
            .position(|f| f.name == field_name)
    }

    pub fn struct_field_type(&self, struct_name: Ident, field_name: Ident) -> Option<Type> {
        self.get_struct(struct_name)?
            .fields
            .iter()
            .find(|f| f.name == field_name)
            .map(|f| f.ty.clone())
    }

    pub fn struct_field_default(
        &self,
        struct_name: Ident,
        field_name: Ident,
    ) -> Option<&FieldDefault> {
        self.get_struct(struct_name)?
            .field_defaults
            .get(&field_name)
    }

    pub fn struct_to_string_body(&self, name: Ident) -> Option<(&BlockNode, &Type)> {
        use internment::Intern;
        let def = self.get_struct(name)?;
        if !def.type_params.is_empty() {
            return None;
        }
        let to_string = Ident(Intern::new("to_string".to_string()));
        let method = def.methods.get(&to_string)?;
        if method.receiver != Some(MethodReceiver::Value) {
            return None;
        }
        if method.ret != Type::String {
            return None;
        }
        if !method.params.is_empty() {
            return None;
        }
        if !method.type_params.is_empty() {
            return None;
        }
        Some((&method.body, &method.ret))
    }

    pub fn enum_names(&self) -> Vec<Ident> {
        let mut names: HashSet<Ident> = self.enum_defs.keys().copied().collect();
        for module_def in self.resolved_module_defs.values() {
            names.extend(module_def.enum_defs.keys().copied());
        }
        names.into_iter().collect()
    }

    pub fn enum_variant_index(&self, enum_name: Ident, variant_name: Ident) -> Option<u16> {
        let def = self.get_enum(enum_name)?;
        let idx = def.variants.iter().position(|v| v.name == variant_name)?;
        Some(idx as u16)
    }

    pub fn enum_variant_field_names(
        &self,
        enum_name: Ident,
        variant_name: Ident,
    ) -> Option<Vec<Ident>> {
        let def = self.get_enum(enum_name)?;
        let variant = def.variants.iter().find(|v| v.name == variant_name)?;
        match &variant.kind {
            VariantKind::Struct(fields) => Some(fields.iter().map(|f| f.name).collect()),
            _ => None,
        }
    }

    pub fn enum_variant_field_types(
        &self,
        enum_name: Ident,
        variant_name: Ident,
        type_args: &[Type],
    ) -> Option<Vec<Type>> {
        let def = self.get_enum(enum_name)?;
        let variant = def.variants.iter().find(|v| v.name == variant_name)?;
        let subst = build_subst(&def.type_params, type_args);
        match &variant.kind {
            VariantKind::Tuple(types) => Some(
                types
                    .iter()
                    .map(|t| subst_type(t, &subst, &HashMap::new()))
                    .collect(),
            ),
            VariantKind::Struct(fields) => Some(
                fields
                    .iter()
                    .map(|f| subst_type(&f.ty, &subst, &HashMap::new()))
                    .collect(),
            ),
            VariantKind::Unit => Some(vec![]),
        }
    }

    pub fn enum_variant_kinds(&self, name: Ident) -> Option<Vec<(Ident, &VariantKind)>> {
        self.get_enum(name)
            .map(|def| def.variants.iter().map(|v| (v.name, &v.kind)).collect())
    }

    pub fn is_module_name(&self, name: Ident) -> bool {
        self.module_defs.contains_key(&name)
    }

    pub fn get_type(&self, id: ExprId) -> Option<&(Span, Type)> {
        self.types.get(&id)
    }

    pub fn specializations(&self) -> &HashMap<SpecializationKey, SpecializationResult> {
        &self.specialization_cache
    }

    pub fn method_specializations(&self) -> &HashMap<MethodSpecKey, SpecializationResult> {
        &self.method_spec_cache
    }

    pub fn call_type_args(
        &self,
        callee_expr_id: ExprId,
    ) -> Option<&(Ident, Vec<Type>, Vec<usize>)> {
        self.resolved_call_type_args.get(&callee_expr_id)
    }

    pub fn extend_call_target(&self, id: ExprId) -> Option<Ident> {
        self.extend_call_targets.get(&id).copied()
    }

    pub fn user_cast_target(&self, id: ExprId) -> Option<Ident> {
        self.user_cast_targets.get(&id).copied()
    }

    pub fn cast_call_insertion(
        &self,
        call_func_id: ExprId,
        param_idx: usize,
    ) -> Option<&(Ident, Type)> {
        self.cast_call_insertions.get(&(call_func_id, param_idx))
    }

    pub fn extend_call_ref_mask(&self, id: ExprId) -> &[bool] {
        self.extend_call_ref_masks
            .get(&id)
            .map_or(&[], Vec::as_slice)
    }

    pub fn method_param_mutabilities(
        &self,
        struct_name: Ident,
        method_name: Ident,
    ) -> (Option<MethodReceiver>, &[Param]) {
        self.struct_defs
            .get(&struct_name)
            .and_then(|s| s.methods.get(&method_name))
            .map_or((None, &[]), |m| (m.receiver, m.params.as_slice()))
    }

    pub fn extend_specializations(&self) -> &HashMap<ExtendSpecKey, SpecializationResult> {
        &self.extend_spec_cache
    }

    pub fn generic_template(&self, name: Ident) -> Option<&FuncNode> {
        self.generic_func_templates.get(&name)
    }

    pub fn get_generic_extend_template(
        &self,
        base_name: Ident,
        method_name: Ident,
        target_type: &Type,
    ) -> Option<&GenericExtendTemplate> {
        self.generic_extend_templates
            .get(&(base_name, method_name))
            .and_then(|v| v.iter().find(|t| &t.target_type == target_type))
    }

    pub fn const_value(&self, id: ExprId) -> Option<&ConstValue> {
        self.const_values.get(&id)
    }

    pub fn const_pattern_value(&self, key: (usize, usize)) -> Option<&ConstValue> {
        self.const_pattern_values.get(&key)
    }

    pub fn lambda_captures(&self, id: ExprId) -> &[(Ident, Type)] {
        self.lambda_captures.get(&id).map_or(&[], Vec::as_slice)
    }

    pub fn warnings(&self) -> &[Diagnostic] {
        &self.warnings
    }
}

#[derive(Copy, Clone)]
pub(super) enum PostfixNodeRef<'a> {
    Field {
        expr_id: ExprId,
        node: &'a FieldAccessNode,
    },
    Index {
        expr_id: ExprId,
        node: &'a IndexNode,
    },
    Call {
        expr_id: ExprId,
        node: &'a CallNode,
    },
}

impl PostfixNodeRef<'_> {
    pub fn safe(&self) -> bool {
        match self {
            PostfixNodeRef::Field { node, .. } => node.node.safe,
            PostfixNodeRef::Index { node, .. } => node.node.safe,
            PostfixNodeRef::Call { node, .. } => node.node.safe,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            PostfixNodeRef::Field { node, .. } => node.span,
            PostfixNodeRef::Index { node, .. } => node.span,
            PostfixNodeRef::Call { node, .. } => node.span,
        }
    }

    pub fn expr_id(&self) -> ExprId {
        match self {
            PostfixNodeRef::Field { expr_id, .. }
            | PostfixNodeRef::Index { expr_id, .. }
            | PostfixNodeRef::Call { expr_id, .. } => *expr_id,
        }
    }
}

pub(super) fn type_field_on_base(
    base_ty: &Type,
    field: Ident,
    span: Span,
    type_checker: &TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    if let Some(agg) = base_ty.as_aggregate() {
        let struct_def = match type_checker.get_struct_deep(agg.name, agg.origin) {
            DeepLookup::Found(def) => def,
            DeepLookup::Ambiguous(first, second) => {
                errors.push(Diagnostic::new(
                    span,
                    DiagnosticKind::AmbiguousType {
                        name: agg.name,
                        first_module: first,
                        second_module: second,
                    },
                ));
                return Type::Infer;
            }
            DeepLookup::NotFound => {
                errors.push(Diagnostic::new(
                    span,
                    DiagnosticKind::UnknownStruct { name: agg.name },
                ));
                return Type::Infer;
            }
        };

        let subst = build_subst(&struct_def.type_params, agg.type_args);

        for struct_field in &struct_def.fields {
            if struct_field.name == field {
                if let Some(ann) = struct_def.field_annotations.get(&field) {
                    type_checker.check_member_access(
                        ann,
                        span,
                        "field",
                        field,
                        agg.name,
                        struct_def.defining_module.as_deref(),
                        errors,
                    );
                }
                let field_ty = subst_type(&struct_field.ty, &subst, &HashMap::new());
                return type_checker.resolve_type(&field_ty);
            }
        }

        errors.push(Diagnostic::new(
            span,
            DiagnosticKind::StructUnknownField {
                kind: agg.keyword(),
                struct_name: agg.name,
                field,
            },
        ));
        return Type::Infer;
    }

    match base_ty {
        Type::NamedTuple(fields) => {
            for (label, ty) in fields {
                if *label == field {
                    return ty.clone();
                }
            }
            errors.push(Diagnostic::new(
                span,
                DiagnosticKind::NoSuchFieldOnTuple {
                    field,
                    tuple_type: base_ty.clone(),
                },
            ));
            Type::Infer
        }
        Type::Extern { name, origin } => {
            let extern_def = match type_checker.get_extern_type_deep(*name, origin.as_deref()) {
                DeepLookup::Found(def) => def,
                DeepLookup::Ambiguous(first, second) => {
                    errors.push(Diagnostic::new(
                        span,
                        DiagnosticKind::AmbiguousType {
                            name: *name,
                            first_module: first,
                            second_module: second,
                        },
                    ));
                    return Type::Infer;
                }
                DeepLookup::NotFound => {
                    errors.push(Diagnostic::new(
                        span,
                        DiagnosticKind::FieldAccessOnNonNamedTuple {
                            field,
                            found: base_ty.clone(),
                        },
                    ));
                    return Type::Infer;
                }
            };

            if let Some(field_def) = extern_def.fields.get(&field) {
                return field_def.ty.clone();
            }

            errors.push(Diagnostic::new(
                span,
                DiagnosticKind::ExternUnknownField {
                    type_name: *name,
                    field,
                },
            ));
            Type::Infer
        }
        Type::UnresolvedName(name) => {
            // it could be an ambiguous/transitive type, so we do deep lookup before falling through
            if type_checker.check_name_ambiguity(*name, span, errors) {
                return Type::Infer;
            }
            errors.push(Diagnostic::new(
                span,
                DiagnosticKind::FieldAccessOnNonNamedTuple {
                    field,
                    found: base_ty.clone(),
                },
            ));
            Type::Infer
        }
        Type::Infer => Type::Infer,
        _ => {
            errors.push(Diagnostic::new(
                span,
                DiagnosticKind::FieldAccessOnNonNamedTuple {
                    field,
                    found: base_ty.clone(),
                },
            ));
            Type::Infer
        }
    }
}

pub(super) fn type_index_on_base(
    base_ty: &Type,
    index_ty: &Type,
    index_expr_id: ExprId,
    span: Span,
    index_span: Span,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    if let Some(range_elem) = range_element_type(index_ty) {
        if matches!(base_ty, Type::Map { .. }) {
            errors.push(Diagnostic::new(span, DiagnosticKind::RangeIndexOnMap));
            return Type::Infer;
        }

        if !matches!(range_elem, Type::Int | Type::Infer) {
            errors.push(Diagnostic::new(
                index_span,
                DiagnosticKind::RangeIndexNotInt {
                    found: range_elem.clone(),
                },
            ));
            return Type::Infer;
        }

        return match base_ty {
            Type::Array { elem, .. } | Type::Slice { elem } => Type::Slice { elem: elem.clone() },
            Type::List { elem } => Type::List { elem: elem.clone() },
            _ => {
                errors.push(Diagnostic::new(
                    span,
                    DiagnosticKind::IndexOnNonArray {
                        found: base_ty.clone(),
                    },
                ));
                Type::Infer
            }
        };
    }

    if let Type::Map { key, value } = base_ty {
        let key_ref = TypeRef::Expr(index_expr_id);
        let expected_ref = TypeRef::concrete(key);
        type_checker.constrain_equal(index_span, key_ref, expected_ref, errors);
        return (**value).clone();
    }

    let maybe_int = matches!(index_ty, Type::Int | Type::Infer);
    if !maybe_int {
        errors.push(Diagnostic::new(
            index_span,
            DiagnosticKind::IndexNotInt {
                found: index_ty.clone(),
            },
        ));
        return Type::Infer;
    }

    if let Some(elem_ty) = indexable_element_type(base_ty) {
        elem_ty
    } else {
        errors.push(Diagnostic::new(
            span,
            DiagnosticKind::IndexOnNonArray {
                found: base_ty.clone(),
            },
        ));
        Type::Infer
    }
}

pub(super) fn indexable_element_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Array { elem, .. } | Type::List { elem } | Type::Slice { elem } => {
            Some((**elem).clone())
        }
        _ => None,
    }
}

pub(super) fn unwrap_opt_typ(ty: &Type) -> &Type {
    ty.option_inner().unwrap_or(ty)
}

#[derive(Clone, Copy)]
enum TypeProperty {
    Keyable,
    Equatable,
}

impl TypeProperty {
    fn name(self) -> &'static str {
        match self {
            TypeProperty::Keyable => "keyable",
            TypeProperty::Equatable => "equatable",
        }
    }
}

fn check_type_property(ty: &Type, tc: &TypeChecker, prop: TypeProperty) -> Result<(), String> {
    let prop_name = prop.name();
    match ty {
        Type::Tuple(elems) => {
            for (i, t) in elems.iter().enumerate() {
                check_type_property(t, tc, prop).map_err(|_| {
                    format!("tuple element {i} has type '{t}' which is not {prop_name}")
                })?;
            }
            Ok(())
        }
        Type::NamedTuple(fields) => {
            for (label, t) in fields {
                check_type_property(t, tc, prop).map_err(|_| {
                    format!("field '{label}' has type '{t}' which is not {prop_name}")
                })?;
            }
            Ok(())
        }
        Type::Enum {
            name,
            type_args,
            origin,
        } => {
            if ty.is_option() {
                return match prop {
                    TypeProperty::Keyable => {
                        Err("optional types cannot be used as map keys".to_string())
                    }
                    TypeProperty::Equatable => {
                        let inner = ty.option_inner().cloned().unwrap_or(Type::Infer);
                        check_type_property(&inner, tc, prop)
                    }
                };
            }
            let DeepLookup::Found(enum_def) = tc.get_enum_deep(*name, origin.as_deref()) else {
                return Err(format!("enum '{name}' is not known"));
            };
            let subst = build_subst(&enum_def.type_params, type_args);
            for v in &enum_def.variants {
                let resolved_types: Vec<Type> = match &v.kind {
                    VariantKind::Unit => continue,
                    VariantKind::Tuple(types) => types
                        .iter()
                        .map(|t| subst_type(t, &subst, &HashMap::new()))
                        .collect(),
                    VariantKind::Struct(fields) => fields
                        .iter()
                        .map(|f| subst_type(&f.ty, &subst, &HashMap::new()))
                        .collect(),
                };
                for resolved in &resolved_types {
                    check_type_property(resolved, tc, prop).map_err(|msg| {
                        format!("variant '{}' has payload type '{resolved}': {msg}", v.name)
                    })?;
                }
            }
            Ok(())
        }
        Type::Struct {
            name,
            type_args,
            origin,
        } => {
            let DeepLookup::Found(struct_def) = tc.get_struct_deep(*name, origin.as_deref()) else {
                return Err(format!("struct '{name}' is not known"));
            };
            let subst = build_subst(&struct_def.type_params, type_args);
            for f in &struct_def.fields {
                let resolved = subst_type(&f.ty, &subst, &HashMap::new());
                check_type_property(&resolved, tc, prop)
                    .map_err(|msg| format!("field '{}' has type '{resolved}': {msg}", f.name))?;
            }
            Ok(())
        }
        _ => check_type_property_leaf(ty, tc, prop),
    }
}

fn check_type_property_leaf(ty: &Type, tc: &TypeChecker, prop: TypeProperty) -> Result<(), String> {
    match prop {
        TypeProperty::Keyable => match ty {
            Type::Int | Type::Bool | Type::String | Type::DataRef { .. } => Ok(()),
            Type::Float | Type::Double => Err(
                "float is not keyable due to NaN and precision issues; use int or string instead"
                    .to_string(),
            ),
            other => Err(format!("type '{other}' is not keyable")),
        },
        TypeProperty::Equatable => match ty {
            Type::Infer | Type::Int | Type::Float | Type::Double | Type::Bool | Type::String => {
                Ok(())
            }
            Type::Func { .. } | Type::DataRef { .. } => Ok(()),
            Type::List { elem } | Type::Array { elem, .. } => check_type_property(elem, tc, prop)
                .map_err(|_| format!("element type '{elem}' is not equatable")),
            Type::Map { key, value } => {
                check_type_property(key, tc, prop)
                    .map_err(|_| format!("key type '{key}' is not equatable"))?;
                check_type_property(value, tc, prop)
                    .map_err(|_| format!("value type '{value}' is not equatable"))
            }
            other => Err(format!("type '{other}' is not equatable")),
        },
    }
}

fn check_keyable(ty: &Type, tc: &TypeChecker) -> Result<(), String> {
    check_type_property(ty, tc, TypeProperty::Keyable)
}

pub(super) fn is_keyable(ty: &Type, tc: &TypeChecker) -> bool {
    check_keyable(ty, tc).is_ok()
}

fn check_equatable(ty: &Type, tc: &TypeChecker) -> Result<(), String> {
    check_type_property(ty, tc, TypeProperty::Equatable)
}

pub(super) fn is_equatable(ty: &Type, tc: &TypeChecker) -> bool {
    check_equatable(ty, tc).is_ok()
}

pub(super) fn equatable_reason(ty: &Type, tc: &TypeChecker) -> Option<String> {
    check_equatable(ty, tc).err()
}

pub(super) fn keyable_reason(ty: &Type, tc: &TypeChecker) -> Option<String> {
    check_keyable(ty, tc).err()
}

pub(super) fn validate_map_key_type(
    span: Span,
    key_ty: &Type,
    type_checker: &TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    if matches!(key_ty, Type::Infer) {
        return;
    }
    if is_keyable(key_ty, type_checker) {
        return;
    }
    if key_ty.is_option() {
        errors.push(Diagnostic::new(
            span,
            DiagnosticKind::MapOptionalKeyNotAllowed {
                found: key_ty.clone(),
            },
        ));
    } else if matches!(key_ty, Type::Float | Type::Double) {
        errors.push(Diagnostic::new(span, DiagnosticKind::MapKeyFloat));
    } else {
        let mut err = Diagnostic::new(
            span,
            DiagnosticKind::MapKeyNotKeyable {
                found: key_ty.clone(),
            },
        );
        if let Some(reason) = keyable_reason(key_ty, type_checker) {
            err.notes.push(reason);
        }
        errors.push(err);
    }
}

pub(super) fn build_param_info(params: &[Param]) -> Vec<(Ident, Mutability)> {
    params.iter().map(|p| (p.name, p.mutability)).collect()
}

pub(super) fn build_cast_accept(params: &[Param]) -> Vec<bool> {
    params.iter().map(|p| p.cast_accept).collect()
}
