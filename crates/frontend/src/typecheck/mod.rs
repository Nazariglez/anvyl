use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use self::{
    const_term::ConstTerm,
    generic_bind::bind_exact_generic_args,
    infer::{
        GenericSolverSeeds, GenericSolverVars, LocalTypeId, Solver, SolverFinalizeError,
        SolverRelationError, TypeHandle,
    },
    place::{PlaceAccess, check_place},
    postfix::{check_postfix_chain, collect_postfix_chain},
    type_ops::TypeFolder,
    type_refs::{GenericParamError, GenericTypeContext},
};
pub(crate) use self::{
    decls::*, generic::*, result::*, semantic_use::*, type_ops::type_closure_facts,
};
use crate::{
    ast::*,
    externs::{
        RawExterns,
        catalog::{
            ExternCatalog, ExternCatalogError, ExternField, ExternFieldRef, ExternType,
            ExternTypeId,
        },
    },
    resolve::{ModuleKey, ResolveResult},
    span::Span,
};

mod const_eval;
mod const_term;
mod decls;
mod extern_boundary;
mod extern_ops;
mod generic;
mod generic_bind;
mod infer;
mod place;
mod postfix;
mod result;
mod semantic_use;
mod type_ops;
mod type_refs;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ConstDiagnostic {
    Value(ConstValue),
    Name(Ident),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MemberAccessKind {
    Field,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeError {
    Decl(DeclError),
    ExternCatalog(ExternCatalogError),
    UndefinedVariable {
        name: Ident,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        span: Span,
    },
    ConstMismatch {
        expected: ConstDiagnostic,
        found: ConstDiagnostic,
        span: Span,
    },
    RecursiveInference {
        span: Span,
    },
    CannotInferType {
        span: Span,
    },
    UnknownType {
        qualifier: Option<Ident>,
        name: Ident,
        span: Span,
    },
    CannotInferConst {
        span: Span,
    },
    NotCallable {
        ty: Type,
        span: Span,
    },
    WrongArgCount {
        expected: usize,
        found: usize,
        span: Span,
    },
    DuplicateName {
        name: Ident,
        span: Span,
    },
    ImmutableAssignment {
        name: Ident,
        span: Span,
    },
    RequiresMutablePlace {
        name: Ident,
        span: Span,
    },
    InvalidOperand {
        op: String,
        operand_type: Type,
        span: Span,
    },
    MissingReturn {
        expected: Type,
        span: Span,
    },
    UnusedValue {
        span: Span,
    },
    IfConditionNotBool {
        found: Type,
        span: Span,
    },
    WhileConditionNotBool {
        found: Type,
        span: Span,
    },
    BreakOutsideLoop {
        span: Span,
    },
    ContinueOutsideLoop {
        span: Span,
    },
    ForIterableNotSupported {
        found: Type,
        span: Span,
    },
    UnsupportedPattern {
        pattern: &'static str,
        span: Span,
    },
    TuplePatternArityMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },
    NamedPatternOnPositional {
        span: Span,
    },
    TuplePatternLabelMismatch {
        expected: Ident,
        found: Ident,
        span: Span,
    },
    OrPatternUnsupported {
        span: Span,
    },
    EmptyMatch {
        span: Span,
    },
    UnreachableFalsePattern {
        span: Span,
    },
    MemberAccessOnNonAggregate {
        ty: Type,
        member: Ident,
        kind: MemberAccessKind,
        span: Span,
    },
    UnknownMember {
        ty: Type,
        member: Ident,
        kind: MemberAccessKind,
        span: Span,
    },
    UndefinedModuleMember {
        module: ModuleScope,
        name: Ident,
        span: Span,
    },
    AmbiguousExtendMethod {
        receiver: Type,
        name: Ident,
        span: Span,
    },
    DuplicateField {
        name: Ident,
        span: Span,
    },
    MissingField {
        name: Ident,
        span: Span,
    },
    InvalidStructLiteral {
        name: Ident,
        kind: String,
        span: Span,
    },
    UnknownStructLiteral {
        qualifier: Option<Ident>,
        name: Ident,
        span: Span,
    },
    UnknownEnumVariant {
        enum_name: Ident,
        variant: Ident,
        span: Span,
    },
    GenericArity(ArityError),
    UnboundGenericParam {
        name: Ident,
        span: Span,
    },
    UnknownConst {
        name: Ident,
        span: Span,
    },
    ConstCycle {
        name: Ident,
        span: Span,
    },
    NonConstExpression {
        span: Span,
    },
    ConstTypeMismatch {
        expected: Type,
        found: Type,
        span: Span,
    },
    InvalidConstCast {
        from: Type,
        to: Type,
        span: Span,
    },
    ConstDivisionByZero {
        span: Span,
    },
    ConstOverflow {
        span: Span,
    },
    ExpectedIntConst {
        found: Type,
        span: Span,
    },
    NegativeArrayLength {
        value: i64,
        span: Span,
    },
    GenericArgKindMismatch {
        expected: &'static str,
        span: Span,
    },
    ExternAnyEscape {
        span: Span,
    },
    AnyOutsideExternBoundary {
        span: Span,
    },
    DuplicateGenericParam {
        name: Ident,
        span: Span,
    },
}

impl From<SolverFinalizeError> for TypeError {
    fn from(error: SolverFinalizeError) -> Self {
        match error {
            SolverFinalizeError::UnresolvedType { span } => Self::CannotInferType { span },
            SolverFinalizeError::UnresolvedConst { span } => Self::CannotInferConst { span },
        }
    }
}

#[derive(Clone)]
struct VarInfo {
    type_id: LocalTypeId,
    mutable: bool,
    const_value: Option<ConstValue>,
}

#[derive(Debug, Clone)]
struct CallableTemplate {
    span: Span,
    receiver: Option<MethodReceiver>,
    generics: GenericTypeContext,
    params: Vec<Param>,
    body: BlockNode,
}

struct TypeChecker {
    solver: Solver,
    calls: HashMap<ExprId, CallTarget>,
    extern_uses: ExternUseMap,
    decls: DeclarationIndex,
    externs: ExternCatalog,
    scopes: Vec<HashMap<Ident, VarInfo>>,
    return_types: Vec<Type>,
    return_seen: Vec<bool>,
    loop_depth: usize,
    errors: Vec<TypeError>,
    current_module: ModuleScope,
    module_programs: HashMap<ModuleScope, Rc<Program>>,
    type_substs: Vec<TypeSubst>,
    const_substs: Vec<ConstSubst>,
    generic_contexts: Vec<GenericTypeContext>,
    callable_templates: HashMap<CallableId, CallableTemplate>,
    specializations: HashMap<SpecializationKey, SpecializationState>,
    consts: HashMap<(ModuleScope, Ident), const_eval::ConstEntry>,
}

struct ConstNormalizer<'tc> {
    tc: &'tc mut TypeChecker,
    span: Span,
}

impl TypeFolder for ConstNormalizer<'_> {
    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        self.tc.normalize_const_arg(arg, self.span)
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        self.tc.normalize_array_len(len, self.span)
    }
}

struct CheckedSubstituter<'a, 'tc> {
    tc: &'tc mut TypeChecker,
    span: Span,
    types: &'a TypeSubst,
    consts: &'a ConstSubst,
}

impl TypeFolder for CheckedSubstituter<'_, '_> {
    fn fold_var(&mut self, id: TypeVarId) -> Type {
        self.types.get(&id).cloned().unwrap_or(Type::Var(id))
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Param(id) => self
                .consts
                .get(id)
                .and_then(ConstTerm::to_arg_no_infer)
                .unwrap_or_else(|| arg.clone()),
            ConstArg::Value(_) | ConstArg::Name(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Param(id) => match self.consts.get(&id).cloned() {
                Some(term) => self
                    .tc
                    .array_len_from_term(term, self.span)
                    .unwrap_or(ArrayLen::Infer),
                None => ArrayLen::Param(id),
            },
            other => self
                .tc
                .array_len_from_term(ConstTerm::from_array_len(other), self.span)
                .unwrap_or(ArrayLen::Infer),
        }
    }
}

impl TypeChecker {
    fn new(decls: DeclarationIndex, externs: ExternCatalog) -> Self {
        Self {
            solver: Solver::default(),
            calls: HashMap::new(),
            extern_uses: HashMap::new(),
            decls,
            externs,
            scopes: vec![],
            return_types: vec![],
            return_seen: vec![],
            loop_depth: 0,
            errors: vec![],
            current_module: ModuleScope::Root,
            module_programs: HashMap::new(),
            type_substs: vec![],
            const_substs: vec![],
            generic_contexts: vec![],
            callable_templates: HashMap::new(),
            specializations: HashMap::new(),
            consts: HashMap::new(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: Ident, ty: Type, mutable: bool) {
        self.define_value(name, ty, mutable, None);
    }

    fn define_const(&mut self, name: Ident, ty: Type, value: ConstValue) {
        self.define_value(name, ty, false, Some(value));
    }

    fn define_value(
        &mut self,
        name: Ident,
        ty: Type,
        mutable: bool,
        const_value: Option<ConstValue>,
    ) {
        let Some(scope) = self.scopes.last() else {
            return;
        };
        if scope.contains_key(&name) {
            self.errors.push(TypeError::DuplicateName {
                name,
                span: Span::new(0, 0),
            });
            return;
        }

        let type_id = self.solver.alloc_local_type(&ty);
        self.scopes.last_mut().expect("scope exists").insert(
            name,
            VarInfo {
                type_id,
                mutable,
                const_value,
            },
        );
    }

    fn lookup(&self, name: Ident) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(&name) {
                return Some(info);
            }
        }
        None
    }

    fn lookup_type(&self, name: Ident) -> Option<Type> {
        self.lookup(name)
            .map(|info| self.solver.local_type_to_type(info.type_id))
    }

    fn type_handle(&self, ty: &Type) -> TypeHandle {
        self.solver.concrete_type(ty)
    }

    fn local_handle(&self, id: LocalTypeId) -> TypeHandle {
        self.solver.local_handle(id)
    }

    fn set_type(&mut self, id: ExprId, ty: Type, span: Span) -> TypeHandle {
        if matches!(ty, Type::Infer) {
            self.solver.error_expr_type(id, span)
        } else {
            self.solver.set_expr_type_from_type(id, span, &ty);
            self.solver.expr_handle(id)
        }
    }

    fn set_nil_type(&mut self, id: ExprId, span: Span) -> TypeHandle {
        self.solver.nil_expr_type(id, span)
    }

    fn fresh_nil_handle(&mut self, span: Span) -> TypeHandle {
        self.solver.fresh_nil_handle(span)
    }

    fn set_type_from_handle(&mut self, id: ExprId, span: Span, handle: TypeHandle) -> TypeHandle {
        self.solver.set_expr_type_from_handle(id, span, handle)
    }

    fn set_local_type_from_handle(&mut self, id: LocalTypeId, handle: TypeHandle) {
        self.solver.set_local_type_from_handle(id, handle);
    }

    fn fresh_temp_handle(&mut self, span: Span) -> TypeHandle {
        self.solver.fresh_temp_handle(span)
    }

    fn array_handle(&mut self, elem: TypeHandle, len: ArrayLen) -> TypeHandle {
        self.solver.array_handle(elem, len)
    }

    fn list_handle(&mut self, elem: TypeHandle) -> TypeHandle {
        self.solver.list_handle(elem)
    }

    fn tuple_handle(&mut self, elems: Vec<TypeHandle>) -> TypeHandle {
        self.solver.tuple_handle(elems)
    }

    fn named_tuple_handle(&mut self, fields: Vec<(Ident, TypeHandle)>) -> TypeHandle {
        self.solver.named_tuple_handle(fields)
    }

    fn handle_type(&self, handle: &TypeHandle) -> Type {
        self.solver.handle_to_type(handle)
    }

    fn expect_assignable(&mut self, span: Span, from: TypeHandle, to: TypeHandle) {
        self.solver.add_handle_assignable(span, from, to);
    }

    fn expect_equal(&mut self, span: Span, left: TypeHandle, right: TypeHandle) {
        self.solver.add_handle_equal(span, left, right);
    }

    fn solve_constraints(&mut self) -> bool {
        let errors = self.solver.solve_pending();
        let has_errors = !errors.is_empty();
        for error in errors {
            self.push_solver_error(error);
        }
        has_errors
    }

    fn push_solver_error(&mut self, error: SolverRelationError) {
        let err = match error {
            SolverRelationError::TypeMismatch {
                expected,
                found,
                span,
            } => TypeError::TypeMismatch {
                expected,
                found,
                span,
            },
            SolverRelationError::ConstMismatch {
                expected,
                found,
                span,
            } => TypeError::ConstMismatch {
                expected,
                found,
                span,
            },
            SolverRelationError::RecursiveInference { span } => {
                TypeError::RecursiveInference { span }
            }
        };
        self.push_error(err);
    }

    fn push_finalize_errors(&mut self, errors: Vec<SolverFinalizeError>) -> bool {
        let has_errors = !errors.is_empty();
        for error in errors {
            self.push_error_once(TypeError::from(error));
        }
        has_errors
    }

    fn get_type(&self, id: ExprId) -> Option<(Span, Type)> {
        self.solver.expr_type_to_type(id)
    }

    fn expr_types(&self) -> HashMap<ExprId, (Span, Type)> {
        self.solver.expr_types_to_types()
    }

    fn record_call(&mut self, expr_id: ExprId, target: CallTarget) {
        self.calls.insert(expr_id, target);
    }

    fn record_extern_use(&mut self, expr_id: ExprId, target: ExternUseTarget) {
        self.extern_uses.entry(expr_id).or_default().push(target);
    }

    fn reject_extern_any_escape(&mut self, checked: &CheckedType, span: Span) {
        self.reject_extern_any_escape_fact(checked.contains_extern_any, span);
    }

    fn reject_extern_any_escape_fact(&mut self, contains_extern_any: bool, span: Span) {
        if contains_extern_any {
            self.push_error(TypeError::ExternAnyEscape { span });
        }
    }

    fn reject_user_any_type(&mut self, ty: &Type, span: Span) -> bool {
        if !type_closure_facts(ty).contains_any {
            return false;
        }
        self.push_error(TypeError::AnyOutsideExternBoundary { span });
        true
    }

    fn extern_type_id(&self, ty: &Type) -> Option<ExternTypeId> {
        let key = self.decls.key_for_type(ty)?;
        (key.kind == NominalKind::Extern)
            .then(|| self.externs.type_by_nominal(&key))
            .flatten()
    }

    fn extern_type(&self, owner: ExternTypeId) -> &ExternType {
        self.externs.ty(owner)
    }

    fn extern_field(
        &self,
        owner: ExternTypeId,
        name: Ident,
    ) -> Option<(ExternFieldRef, &ExternField)> {
        self.externs.field(owner, name)
    }

    fn push_return_type(&mut self, ty: Type) {
        self.return_types.push(ty);
        self.return_seen.push(false);
    }

    fn pop_return_type(&mut self) {
        self.return_types.pop();
        self.return_seen.pop();
    }

    fn return_type(&self) -> Option<&Type> {
        self.return_types.last()
    }

    fn saw_return(&self) -> bool {
        self.return_seen.last().copied().unwrap_or(false)
    }

    fn mark_return(&mut self) {
        if let Some(seen) = self.return_seen.last_mut() {
            *seen = true;
        }
    }

    fn push_error(&mut self, err: TypeError) {
        self.errors.push(err);
    }

    fn push_error_once(&mut self, err: TypeError) {
        if !self.errors.contains(&err) {
            self.push_error(err);
        }
    }

    fn push_unbound_generic_errors(&mut self, names: Vec<Ident>, span: Span) {
        for name in names {
            self.push_error(TypeError::UnboundGenericParam { name, span });
        }
    }

    fn enter_loop(&mut self) {
        self.loop_depth += 1;
    }

    fn exit_loop(&mut self) {
        self.loop_depth = self.loop_depth.saturating_sub(1);
    }

    fn in_loop(&self) -> bool {
        self.loop_depth > 0
    }

    fn push_type_subst(&mut self, subst: TypeSubst) {
        self.type_substs.push(subst);
    }

    fn pop_type_subst(&mut self) {
        self.type_substs.pop();
    }

    fn push_const_subst(&mut self, subst: ConstSubst) {
        self.const_substs.push(subst);
    }

    fn pop_const_subst(&mut self) {
        self.const_substs.pop();
    }

    fn generic_context(
        &mut self,
        type_params: &[TypeParam],
        const_params: &[ConstParam],
        span: Span,
    ) -> GenericTypeContext {
        match GenericTypeContext::try_from_params(type_params, const_params) {
            Ok(generics) => generics,
            Err(error) => {
                self.push_error(generic_param_type_error(error, span));
                GenericTypeContext::default()
            }
        }
    }

    fn extended_generic_context(
        &mut self,
        owner: &GenericTypeContext,
        type_params: &[TypeParam],
        const_params: &[ConstParam],
        span: Span,
    ) -> GenericTypeContext {
        match owner.try_with_shadowing_params(type_params, const_params) {
            Ok(generics) => generics,
            Err(error) => {
                self.push_error(generic_param_type_error(error, span));
                owner.clone()
            }
        }
    }

    fn push_generic_context(&mut self, generics: GenericTypeContext) {
        self.generic_contexts.push(generics);
    }

    fn pop_generic_context(&mut self) {
        self.generic_contexts.pop();
    }

    fn store_callable_template(&mut self, id: CallableId, template: CallableTemplate) {
        self.callable_templates.insert(id, template);
    }

    fn callable_template(&self, id: &CallableId) -> Option<&CallableTemplate> {
        self.callable_templates.get(id)
    }

    fn specialization(&self, key: &SpecializationKey) -> Option<&SpecializationState> {
        self.specializations.get(key)
    }

    fn store_specialization(&mut self, key: SpecializationKey, state: SpecializationState) {
        self.specializations.insert(key, state);
    }

    fn restore_specialization(&mut self, body_types: SpecializedBodyTypes) {
        for (id, (span, ty)) in body_types {
            self.set_type(id, ty, span);
        }
    }

    fn resolved_value(value: ResolvedValue) -> (ModuleScope, Ident, ValueDecl) {
        (value.module, value.name, value.decl)
    }

    fn imports_module(&self, imported: &ModuleScope) -> bool {
        self.decls.imports_module(&self.current_module, imported)
    }

    fn extend_visible(&self, origin: &ModuleScope) -> bool {
        origin == &self.current_module
            || self.decls.always_active_module(origin)
            || self.imports_module(origin)
    }

    fn find_extend_method(&self, receiver: &Type, name: Ident) -> Option<ExtendMethodMatch<'_>> {
        self.decls
            .find_extend_method(receiver, name, |ext| self.extend_visible(&ext.origin))
    }

    fn exported_value_in_module(
        &self,
        scope: &ModuleScope,
        name: Ident,
    ) -> Option<(ModuleScope, Ident, ValueDecl)> {
        self.decls
            .exported_value(scope, name)
            .map(Self::resolved_value)
    }

    fn exported_type_in_module(&self, scope: &ModuleScope, name: Ident) -> Option<NominalKey> {
        self.decls.exported_type(scope, name)
    }

    fn exported_module_in_module(&self, scope: &ModuleScope, name: Ident) -> Option<ModuleScope> {
        self.decls.exported_module(scope, name)
    }

    fn current_module_value(&self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        self.decls
            .local_value(&self.current_module, name)
            .map(Self::resolved_value)
    }

    fn resolve_type_for_tc(&mut self, ty: &Type) -> Type {
        self.resolve_type_for_tc_at(ty, Span::new(0, 0))
    }

    fn resolve_type_for_tc_at(&mut self, ty: &Type, span: Span) -> Type {
        let generics = self.generic_contexts.last().cloned().unwrap_or_default();
        let finalized = match self
            .decls
            .finalize_type_ref(&self.current_module, &generics, ty)
        {
            Ok(ty) => ty,
            Err(error) => {
                self.push_error_once(type_ref_error(error, span));
                return Type::Infer;
            }
        };
        self.validate_nominal_uses(&finalized, span);
        let substituted = match self.type_substs.last().cloned() {
            Some(ts) => {
                let cs = self.const_substs.last().cloned().unwrap_or_default();
                self.substitute_checked(&finalized, &ts, &cs, span)
            }
            None => finalized,
        };
        let ty = self.normalize_type_consts(&substituted, span);
        self.reject_user_any_type(&ty, span);
        ty
    }

    fn normalize_type_consts(&mut self, ty: &Type, span: Span) -> Type {
        ConstNormalizer { tc: self, span }.fold_type(ty)
    }

    fn substitute_checked(
        &mut self,
        ty: &Type,
        types: &TypeSubst,
        consts: &ConstSubst,
        span: Span,
    ) -> Type {
        CheckedSubstituter {
            tc: self,
            span,
            types,
            consts,
        }
        .fold_type(ty)
    }

    fn eval_const_term(&mut self, term: ConstTerm, span: Span) -> Option<ConstTerm> {
        match term {
            ConstTerm::Value(_) => Some(term),
            ConstTerm::Name(name) => match self.eval_visible_const(name, span) {
                Some(Ok(value)) => Some(ConstTerm::Value(value)),
                Some(Err(err)) => {
                    self.push_error(err);
                    None
                }
                None => {
                    self.push_error_once(TypeError::UnknownConst { name, span });
                    None
                }
            },
            ConstTerm::Param(id) => match self
                .const_substs
                .last()
                .and_then(|subst| subst.get(&id).cloned())
            {
                Some(term) => self.eval_const_term(term, span),
                None => Some(ConstTerm::Param(id)),
            },
            ConstTerm::ArrayInfer | ConstTerm::Infer(_) => None,
        }
    }

    fn require_usize_const(&mut self, term: ConstTerm, span: Span) -> Option<usize> {
        match self.eval_const_term(term, span)? {
            ConstTerm::Value(value) => match const_eval::const_usize(&value, span) {
                Ok(value) => Some(value),
                Err(err) => {
                    self.push_error(err);
                    None
                }
            },
            ConstTerm::Name(name) => {
                self.push_error(TypeError::UnknownConst { name, span });
                None
            }
            ConstTerm::Param(_) | ConstTerm::ArrayInfer | ConstTerm::Infer(_) => None,
        }
    }

    fn array_len_from_term(&mut self, term: ConstTerm, span: Span) -> Option<ArrayLen> {
        match term {
            ConstTerm::ArrayInfer => Some(ArrayLen::Infer),
            ConstTerm::Param(id) => match self
                .const_substs
                .last()
                .and_then(|subst| subst.get(&id).cloned())
            {
                Some(term) => self.array_len_from_term(term, span),
                None => Some(ArrayLen::Param(id)),
            },
            ConstTerm::Value(_) | ConstTerm::Name(_) => {
                self.require_usize_const(term, span).map(ArrayLen::Fixed)
            }
            ConstTerm::Infer(_) => None,
        }
    }

    fn normalize_const_arg(&mut self, arg: &ConstArg, span: Span) -> ConstArg {
        let Some(term) = self.eval_const_term(ConstTerm::from_arg(arg), span) else {
            return arg.clone();
        };
        term.to_arg_no_infer().unwrap_or_else(|| arg.clone())
    }

    fn normalize_array_len(&mut self, len: ArrayLen, span: Span) -> ArrayLen {
        self.array_len_from_term(ConstTerm::from_array_len(len), span)
            .unwrap_or(ArrayLen::Infer)
    }

    fn imported_value(&self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        self.decls
            .imported_value(&self.current_module, name)
            .map(Self::resolved_value)
    }

    fn lookup_imported_value(&self, name: Ident) -> Option<Type> {
        self.imported_value(name)
            .map(|(_, _, value)| value.ty().clone())
    }

    fn lookup_named_value(&self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        let contains_local = self
            .scopes
            .iter()
            .skip(1)
            .rev()
            .any(|scope| scope.contains_key(&name));
        if contains_local {
            return None;
        }
        self.current_module_value(name)
            .or_else(|| self.imported_value(name))
    }

    fn lookup_module_alias(&self, name: Ident) -> Option<ModuleScope> {
        self.decls.imported_module(&self.current_module, name)
    }

    fn resolve_visible_type_key(
        &self,
        qualifier: Option<Ident>,
        name: Ident,
    ) -> Option<NominalKey> {
        self.decls
            .resolve_visible_type_key(&self.current_module, qualifier, name)
    }

    fn func_type_from_sig(&mut self, params: &[Param], ret: &Type) -> Type {
        let resolved_params: Vec<FuncParam> = params
            .iter()
            .map(|p| {
                FuncParam::new(
                    self.resolve_type_for_tc_at(&p.ty, Span::new(0, 0)),
                    matches!(p.mutability, Mutability::Mutable),
                )
            })
            .collect();
        let resolved_ret = Box::new(self.resolve_type_for_tc_at(ret, Span::new(0, 0)));
        Type::Func {
            params: resolved_params,
            ret: resolved_ret,
        }
    }

    fn into_result(mut self) -> Result<TypecheckResult, Vec<TypeError>> {
        self.solve_constraints();
        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        let (types, finalize_errors) = self.solver.finalize_expr_types();
        let has_finalize_errors = self.push_finalize_errors(finalize_errors);
        if !has_finalize_errors {
            for error in self.result_closure_errors(&types) {
                self.push_error_once(error);
            }
        }
        if self.errors.is_empty() {
            Ok(TypecheckResult {
                types,
                calls: self.calls,
                extern_uses: self.extern_uses,
                decls: self.decls,
                externs: self.externs,
                consts: const_eval::evaluated_consts(self.consts),
            })
        } else {
            Err(self.errors)
        }
    }

    fn result_closure_errors(&self, types: &HashMap<ExprId, (Span, Type)>) -> Vec<TypeError> {
        let mut errors = vec![];
        for (span, ty) in types.values() {
            push_type_closure_error(&mut errors, ty, *span);
        }
        for (id, target) in &self.calls {
            let span = types
                .get(id)
                .map_or_else(|| Span::new(0, 0), |(span, _)| *span);
            push_call_target_closure_error(&mut errors, target, span);
        }
        self.externs.for_each_resolved_ty(|ty, site| {
            push_extern_ty_closure_error(&mut errors, ty, extern_site_span(site));
        });
        errors
    }

    fn nominal_generics(&self, key: &NominalKey) -> Option<GenericParams> {
        self.nominal_generics_in(&self.decls, key)
    }

    fn nominal_generics_in(
        &self,
        decls: &DeclarationIndex,
        key: &NominalKey,
    ) -> Option<GenericParams> {
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                decls.aggregate(key).map(|schema| schema.generics.clone())
            }
            NominalKind::Enum => decls.enum_schema(key).map(|schema| schema.generics.clone()),
            NominalKind::Extern => Some(GenericParams::default()),
        }
    }

    fn validate_nominal_uses(&mut self, ty: &Type, span: Span) {
        match ty {
            Type::Nominal(nominal) => {
                for arg in &nominal.type_args {
                    self.validate_nominal_uses(arg, span);
                }
                let Some(key) = self.decls.key_for_type(ty) else {
                    return;
                };
                let Some(generics) = self.nominal_generics(&key) else {
                    return;
                };
                let args = GenericArgs {
                    type_args: nominal.type_args.clone(),
                    const_args: nominal.const_args.iter().map(ConstTerm::from_arg).collect(),
                };
                let decls = self.decls.clone();
                self.validate_nominal_args(&decls, &key, &generics, &args, span);
            }
            Type::Func { params, ret } => {
                for param in params {
                    self.validate_nominal_uses(&param.ty, span);
                }
                self.validate_nominal_uses(ret, span);
            }
            Type::Tuple(elems) => {
                for elem in elems {
                    self.validate_nominal_uses(elem, span);
                }
            }
            Type::NamedTuple(fields) => {
                for (_, field) in fields {
                    self.validate_nominal_uses(field, span);
                }
            }
            Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
                self.validate_nominal_uses(elem, span);
            }
            Type::Map { key, value } => {
                self.validate_nominal_uses(key, span);
                self.validate_nominal_uses(value, span);
            }
            Type::Infer
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => {}
        }
    }

    fn finalize_declarations(&mut self) {
        let saved_module = self.current_module.clone();
        let mut decls = std::mem::take(&mut self.decls);
        let lookup = decls.clone();
        let generic_errors = decls.map_canonical_type_uses(|site, ty| {
            self.current_module = site.module.clone();
            let span = site.span;
            let ty = self.finalize_decl_type(&lookup, site, ty);
            let ty = self.normalize_type_consts(&ty, span);
            self.validate_nominal_uses_in(&lookup, &ty, span);
            self.reject_user_any_type(&ty, span);
            ty
        });
        for error in generic_errors {
            self.push_error(generic_param_decl_type_error(error));
        }
        self.current_module = saved_module;
        self.decls = decls;
    }

    fn finalize_decl_type(
        &mut self,
        decls: &DeclarationIndex,
        site: DeclTypeSite,
        ty: Type,
    ) -> Type {
        match decls.finalize_type_ref(&site.module, &site.generics, &ty) {
            Ok(ty) => ty,
            Err(TypeRefError::Unknown { qualifier, name }) => {
                self.push_error(TypeError::Decl(DeclError::UnknownType {
                    module: site.module,
                    qualifier,
                    name,
                    span: site.span,
                }));
                Type::Infer
            }
            Err(error) => {
                self.push_error(type_ref_error(error, site.span));
                Type::Infer
            }
        }
    }

    fn validate_nominal_uses_in(&mut self, decls: &DeclarationIndex, ty: &Type, span: Span) {
        match ty {
            Type::Nominal(nominal) => {
                for arg in &nominal.type_args {
                    self.validate_nominal_uses_in(decls, arg, span);
                }
                let Some(key) = decls.key_for_type(ty) else {
                    return;
                };
                let Some(generics) = self.nominal_generics_in(decls, &key) else {
                    return;
                };
                let args = GenericArgs {
                    type_args: nominal.type_args.clone(),
                    const_args: nominal.const_args.iter().map(ConstTerm::from_arg).collect(),
                };
                self.validate_nominal_args(decls, &key, &generics, &args, span);
            }
            Type::Func { params, ret } => {
                for param in params {
                    self.validate_nominal_uses_in(decls, &param.ty, span);
                }
                self.validate_nominal_uses_in(decls, ret, span);
            }
            Type::Tuple(elems) => {
                for elem in elems {
                    self.validate_nominal_uses_in(decls, elem, span);
                }
            }
            Type::NamedTuple(fields) => {
                for (_, field) in fields {
                    self.validate_nominal_uses_in(decls, field, span);
                }
            }
            Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
                self.validate_nominal_uses_in(decls, elem, span);
            }
            Type::Map { key, value } => {
                self.validate_nominal_uses_in(decls, key, span);
                self.validate_nominal_uses_in(decls, value, span);
            }
            Type::Infer
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => {}
        }
    }

    fn validate_nominal_args(
        &mut self,
        decls: &DeclarationIndex,
        key: &NominalKey,
        generics: &GenericParams,
        args: &GenericArgs,
        span: Span,
    ) {
        let error_count = self.errors.len();
        for term in &args.const_args {
            self.require_usize_const(term.clone(), span);
        }
        if self.errors.len() != error_count {
            return;
        }
        let (type_subst, const_subst) = generics.substitutions(args);
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                if let Some(schema) = decls.aggregate(key).cloned() {
                    for field in schema.fields.values() {
                        self.substitute_checked(&field.ty, &type_subst, &const_subst, span);
                    }
                }
            }
            NominalKind::Enum => {
                if let Some(variants) = decls.enum_schema(key).map(|schema| schema.variants.clone())
                {
                    for variant in variants.values() {
                        match variant {
                            VariantSchema::Unit => {}
                            VariantSchema::Tuple(params) => {
                                for param in params {
                                    self.substitute_checked(param, &type_subst, &const_subst, span);
                                }
                            }
                            VariantSchema::Struct(fields) => {
                                for field in fields.values() {
                                    self.substitute_checked(
                                        &field.ty,
                                        &type_subst,
                                        &const_subst,
                                        span,
                                    );
                                }
                            }
                        }
                    }
                }
            }
            NominalKind::Extern => {}
        }
    }
}

pub(crate) fn check_with_modules(
    program: &Program,
    resolved: &ResolveResult,
    always_active_modules: HashSet<ModuleScope>,
    externs: RawExterns,
) -> Result<TypecheckResult, Vec<TypeError>> {
    let mut decls =
        DeclarationIndex::from_root_and_modules(program, resolved, always_active_modules, &externs);
    if decls.has_errors() {
        return Err(decl_errors(decls.errors()));
    }
    let catalog = crate::externs::catalog::build_catalog(externs, &decls).map_err(|errors| {
        errors
            .into_iter()
            .map(TypeError::ExternCatalog)
            .collect::<Vec<_>>()
    })?;
    decls.sync_extern_headers(&catalog);

    let mut tc = TypeChecker::new(decls, catalog);
    tc.collect_const_decls(ModuleScope::Root, program);
    collect_callable_templates(ModuleScope::Root, program, &mut tc);

    for group in &resolved.module_groups {
        for module in group {
            let ModuleKey::Named(path) = &module.key else {
                continue;
            };
            let scope = ModuleScope::Named(path.clone());
            tc.module_programs
                .insert(scope.clone(), Rc::new(module.program.clone()));
            tc.collect_const_decls(scope.clone(), &module.program);
            collect_callable_templates(scope, &module.program, &mut tc);
        }
    }

    tc.eval_module_consts(&ModuleScope::Root);
    tc.finalize_declarations();
    if !tc.errors.is_empty() {
        return Err(tc.errors);
    }
    push_source_scope(&mut tc);
    register_declarations(program, &mut tc);
    check_stmts(&program.stmts, &mut tc);
    tc.pop_scope();
    tc.into_result()
}

fn decl_errors(errors: &[DeclError]) -> Vec<TypeError> {
    errors.iter().cloned().map(TypeError::Decl).collect()
}

fn generic_param_decl_type_error(error: GenericContextError) -> TypeError {
    TypeError::Decl(DeclError::DuplicateGenericParam {
        module: error.module,
        name: error.error.name(),
        span: error.span,
    })
}

fn generic_param_type_error(error: GenericParamError, span: Span) -> TypeError {
    TypeError::DuplicateGenericParam {
        name: error.name(),
        span,
    }
}

fn type_ref_error(error: TypeRefError, span: Span) -> TypeError {
    match error {
        TypeRefError::Unknown { qualifier, name } => TypeError::UnknownType {
            qualifier,
            name,
            span,
        },
        TypeRefError::GenericArity { expected, found } => {
            TypeError::GenericArity(ArityError::TypeArgs { expected, found })
        }
        TypeRefError::GenericArgKindMismatch { expected } => {
            TypeError::GenericArgKindMismatch { expected, span }
        }
    }
}

fn is_generic(func: &Func) -> bool {
    !func.type_params.is_empty() || !func.const_params.is_empty()
}

fn collect_callable_templates(module: ModuleScope, program: &Program, tc: &mut TypeChecker) {
    let mut extend_index = 0;

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                if !is_generic(func) {
                    continue;
                }
                let generics =
                    tc.generic_context(&func.type_params, &func.const_params, func_node.span);
                tc.store_callable_template(
                    CallableId::function(module.clone(), func.name),
                    CallableTemplate {
                        span: func_node.span,
                        receiver: None,
                        generics,
                        params: func.params.clone(),
                        body: func.body.clone(),
                    },
                );
            }
            Stmt::Aggregate(agg_node) => {
                let agg = &agg_node.node;
                let owner = NominalKey {
                    module: module.clone(),
                    kind: agg.kind.into(),
                    name: agg.name,
                };
                let aggregate_is_generic =
                    !agg.type_params.is_empty() || !agg.const_params.is_empty();
                for method in &agg.methods {
                    let method_is_generic =
                        !method.type_params.is_empty() || !method.const_params.is_empty();
                    if !aggregate_is_generic && !method_is_generic {
                        continue;
                    }
                    let owner_generics =
                        tc.generic_context(&agg.type_params, &agg.const_params, agg_node.span);
                    let generics = tc.extended_generic_context(
                        &owner_generics,
                        &method.type_params,
                        &method.const_params,
                        agg_node.span,
                    );
                    tc.store_callable_template(
                        CallableId::aggregate_method(
                            owner.clone(),
                            method.name,
                            method.receiver.is_some(),
                        ),
                        CallableTemplate {
                            span: agg_node.span,
                            receiver: method.receiver,
                            generics,
                            params: method.params.clone(),
                            body: method.body.clone(),
                        },
                    );
                }
            }
            Stmt::Extend(extend_node) => {
                let extend = &extend_node.node;
                let extend_id = ExtendId {
                    module: module.clone(),
                    index: extend_index,
                };
                extend_index += 1;
                if extend.type_params.is_empty() && extend.const_params.is_empty() {
                    continue;
                }
                for method_node in &extend.methods {
                    let method = &method_node.node;
                    let Some((self_param, params)) = method.params.split_first() else {
                        continue;
                    };
                    let receiver = match self_param.mutability {
                        Mutability::Mutable => MethodReceiver::Var,
                        Mutability::Immutable => MethodReceiver::Value,
                    };
                    let generics = tc.generic_context(
                        &extend.type_params,
                        &extend.const_params,
                        extend_node.span,
                    );
                    tc.store_callable_template(
                        CallableId::extend_method(extend_id.clone(), method.name),
                        CallableTemplate {
                            span: extend_node.span,
                            receiver: Some(receiver),
                            generics,
                            params: params.to_vec(),
                            body: method.body.clone(),
                        },
                    );
                }
            }
            _ => {}
        }
    }
}

fn push_source_scope(tc: &mut TypeChecker) {
    tc.push_scope();
    register_builtins(tc);
}

fn register_builtins(tc: &mut TypeChecker) {
    let builtins = [
        ("println", vec![FuncParam::immut(Type::Any)], Type::Void),
        ("assert", vec![FuncParam::immut(Type::Bool)], Type::Void),
        (
            "assert_msg",
            vec![FuncParam::immut(Type::Bool), FuncParam::immut(Type::String)],
            Type::Void,
        ),
    ];

    for (name, params, ret) in builtins {
        tc.define(
            Ident::new(name),
            Type::Func {
                params,
                ret: Box::new(ret),
            },
            false,
        );
    }
}

fn register_declarations(program: &Program, tc: &mut TypeChecker) {
    let extern_functions = tc
        .externs
        .functions_in_scope(&tc.current_module)
        .map(|function| (function.key.name, function.signature.to_func_type()))
        .collect::<Vec<_>>();
    for (name, ty) in extern_functions {
        tc.define(name, ty, false);
    }

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                if is_generic(func) {
                    continue;
                }
                let func_ty = tc.func_type_from_sig(&func.params, &func.ret);
                tc.define(func.name, func_ty, false);
            }
            Stmt::Aggregate(_) | Stmt::Enum(_) => {}
            Stmt::ExternFunc(_) | Stmt::ExternType(_) => {}
            Stmt::Const(const_node) => {
                let c = &const_node.node;
                let ty = match &c.ty {
                    Some(t) => tc.resolve_type_for_tc_at(t, const_node.span),
                    None => Type::Infer,
                };
                tc.define(c.name, ty, false);
            }
            _ => {}
        }
    }
}

fn check_stmts(stmts: &[StmtNode], tc: &mut TypeChecker) {
    for stmt in stmts {
        check_stmt(stmt, tc);
    }
}

fn check_stmt(stmt: &StmtNode, tc: &mut TypeChecker) {
    match &stmt.node {
        Stmt::Func(func_node) => {
            let func = &func_node.node;
            if is_generic(func) {
                return;
            }
            check_func(func_node, tc);
        }
        Stmt::Binding(binding_node) => {
            check_binding(binding_node, tc);
        }
        Stmt::Return(ret_node) => {
            check_return(ret_node, tc);
        }
        Stmt::Expr(expr_node) => {
            check_expr_checked(expr_node, tc);
        }
        Stmt::While(while_node) => {
            check_while(while_node, tc);
        }
        Stmt::WhileLet(while_let_node) => {
            check_while_let(while_let_node, tc);
        }
        Stmt::For(for_node) => {
            check_for(for_node, tc);
        }
        Stmt::Break => {
            check_break(stmt.span, tc);
        }
        Stmt::Continue => {
            check_continue(stmt.span, tc);
        }
        Stmt::Extend(extend_node) => {
            check_extend(extend_node, tc);
        }
        Stmt::Aggregate(_) | Stmt::Enum(_) => {}
        Stmt::Const(const_node) => {
            if tc.scopes.len() > 1 {
                check_const(const_node, tc);
            }
        }
        Stmt::Import(_)
        | Stmt::ExternFunc(_)
        | Stmt::ExternType(_)
        | Stmt::LetElse(_)
        | Stmt::Defer(_) => {}
    }
}

fn check_func(func_node: &FuncNode, tc: &mut TypeChecker) {
    let func = &func_node.node;
    let Some(func_ty) = tc.lookup_type(func.name) else {
        return;
    };
    let Type::Func {
        params: param_types,
        ret,
    } = &func_ty
    else {
        return;
    };
    check_func_body(
        None,
        &func.params,
        param_types,
        (**ret).clone(),
        &func.body,
        func_node.span,
        &[],
        tc,
    );
}

fn check_extend(extend_node: &ExtendDeclNode, tc: &mut TypeChecker) {
    let extend = &extend_node.node;
    let is_generic_extend = !extend.type_params.is_empty() || !extend.const_params.is_empty();
    if is_generic_extend {
        return;
    }

    let self_ty = tc.resolve_type_for_tc_at(&extend.ty, extend_node.span);
    for method_node in &extend.methods {
        let method = &method_node.node;
        let Some((self_param, params)) = method.params.split_first() else {
            continue;
        };
        let receiver = match self_param.mutability {
            Mutability::Mutable => MethodReceiver::Var,
            Mutability::Immutable => MethodReceiver::Value,
        };
        let param_types: Vec<_> = params
            .iter()
            .map(|param| {
                FuncParam::new(
                    tc.resolve_type_for_tc_at(&param.ty, method_node.span),
                    matches!(param.mutability, Mutability::Mutable),
                )
            })
            .collect();
        let ret_ty = tc.resolve_type_for_tc_at(&method.ret, method_node.span);
        check_func_body(
            Some((receiver, self_ty.clone())),
            params,
            &param_types,
            ret_ty,
            &method.body,
            extend_node.span,
            &[],
            tc,
        );
    }
}

fn check_func_body(
    self_binding: Option<(MethodReceiver, Type)>,
    params: &[Param],
    param_types: &[FuncParam],
    ret_ty: Type,
    body: &BlockNode,
    span: Span,
    const_bindings: &[(Ident, ConstValue)],
    tc: &mut TypeChecker,
) {
    tc.push_scope();
    for (name, value) in const_bindings {
        tc.define_const(*name, const_eval::const_type(value), value.clone());
    }
    tc.push_return_type(ret_ty.clone());
    if let Some((receiver, self_ty)) = self_binding {
        tc.define(
            Ident::new("self"),
            self_ty,
            matches!(receiver, MethodReceiver::Var),
        );
    }
    for (param, param_ty) in params.iter().zip(param_types.iter()) {
        tc.define(
            param.name,
            param_ty.ty.clone(),
            matches!(param.mutability, Mutability::Mutable),
        );
    }
    let expects_value = !ret_ty.is_void();
    let body_checked = if expects_value {
        let ret_handle = tc.type_handle(&ret_ty);
        check_block_checked_with_hint(body, Some(ret_handle), tc)
    } else {
        check_block_checked(body, tc)
    };
    let body_is_void = body_checked.ty.is_void();
    let saw_return = tc.saw_return();
    let missing_implicit_return = body_is_void && !saw_return;
    if expects_value {
        if missing_implicit_return {
            tc.push_error(TypeError::MissingReturn {
                expected: ret_ty.clone(),
                span,
            });
        } else if !body_is_void {
            tc.reject_extern_any_escape(&body_checked, span);
            let ret_handle = tc.type_handle(&ret_ty);
            tc.expect_assignable(span, body_checked.handle, ret_handle);
        }
    } else if !body_is_void && let Some(tail) = &body.node.tail {
        let span = tc
            .get_type(tail.node.id)
            .map_or_else(|| Span::new(0, 0), |(span, _)| span);
        tc.push_error(TypeError::UnusedValue { span });
    }
    tc.pop_return_type();
    tc.pop_scope();
}

fn check_specialized_callable_body(
    callee: &CallableRef,
    param_types: &[FuncParam],
    ret_ty: Type,
    args: &GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    const_bindings: Vec<(Ident, ConstValue)>,
    tc: &mut TypeChecker,
) {
    if args.is_empty()
        || matches!(
            callee.def.id.kind,
            CallableKind::ExternFunction | CallableKind::EnumVariant
        )
    {
        return;
    }

    let key = specialization_key(callee.def.id.clone(), args);
    if specialization_is_cached(&key, tc) {
        return;
    }

    let Some(template) = tc.callable_template(&callee.def.id).cloned() else {
        return;
    };
    let receiver = template.receiver.zip(callee.receiver_ty.clone());

    check_with_specialization(key, type_subst, const_subst, template.generics, tc, |tc| {
        with_source_module_scope(&callee.def.id.module, tc, |tc| {
            check_func_body(
                receiver,
                &template.params,
                param_types,
                ret_ty.clone(),
                &template.body,
                template.span,
                &const_bindings,
                tc,
            );
        });
    });
}

fn specialization_is_cached(key: &SpecializationKey, tc: &mut TypeChecker) -> bool {
    match tc.specialization(key).cloned() {
        Some(SpecializationState::InProgress) => true,
        Some(SpecializationState::Done(body_types)) => {
            tc.restore_specialization(body_types);
            true
        }
        None => false,
    }
}

fn check_with_specialization(
    key: SpecializationKey,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    generics: GenericTypeContext,
    tc: &mut TypeChecker,
    check_body: impl FnOnce(&mut TypeChecker),
) {
    let old_types = tc.expr_types();
    tc.store_specialization(key.clone(), SpecializationState::InProgress);
    tc.push_type_subst(type_subst);
    tc.push_const_subst(const_subst);
    tc.push_generic_context(generics);
    check_body(tc);
    tc.solve_constraints();
    tc.pop_generic_context();
    tc.pop_const_subst();
    tc.pop_type_subst();
    tc.store_specialization(
        key,
        SpecializationState::Done(specialized_body_types(&old_types, &tc.expr_types())),
    );
}

fn const_param_bindings(params: &GenericParams, args: &GenericArgs) -> Vec<(Ident, ConstValue)> {
    params
        .const_params
        .iter()
        .zip(&args.const_args)
        .filter_map(|(param, term)| match term {
            ConstTerm::Value(value) => Some((param.name, value.clone())),
            ConstTerm::Name(_)
            | ConstTerm::Param(_)
            | ConstTerm::ArrayInfer
            | ConstTerm::Infer(_) => None,
        })
        .collect()
}

fn callable_const_bindings(
    owner_params: &GenericParams,
    owner_args: &GenericArgs,
    callable_params: &GenericParams,
    callable_args: &GenericArgs,
) -> Vec<(Ident, ConstValue)> {
    let mut bindings = const_param_bindings(owner_params, owner_args);
    bindings.extend(const_param_bindings(callable_params, callable_args));
    bindings
}

fn specialized_body_types(
    old_types: &HashMap<ExprId, (Span, Type)>,
    types: &HashMap<ExprId, (Span, Type)>,
) -> SpecializedBodyTypes {
    types
        .iter()
        .filter_map(|(id, ty)| match old_types.get(id) {
            Some(old) if old == ty => None,
            _ => Some((*id, ty.clone())),
        })
        .collect()
}

fn specialization_key(id: CallableId, args: &GenericArgs) -> SpecializationKey {
    SpecializationKey {
        target: id,
        args: args.clone(),
    }
}

fn with_global_scope<R>(tc: &mut TypeChecker, f: impl FnOnce(&mut TypeChecker) -> R) -> R {
    let scopes = std::mem::take(&mut tc.scopes);
    tc.scopes = scopes.first().cloned().into_iter().collect();
    let ret = f(tc);
    tc.scopes = scopes;
    ret
}

fn with_source_module_scope<R>(
    module: &ModuleScope,
    tc: &mut TypeChecker,
    f: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    let previous_module = std::mem::replace(&mut tc.current_module, module.clone());
    let ret = match module {
        ModuleScope::Root => with_global_scope(tc, f),
        ModuleScope::Named(_) => {
            let scopes = std::mem::take(&mut tc.scopes);
            tc.scopes = vec![];
            push_source_scope(tc);
            if let Some(program) = tc.module_programs.get(module).map(Rc::clone) {
                register_declarations(program.as_ref(), tc);
                tc.eval_module_consts(module);
            }
            let ret = f(tc);
            tc.scopes = scopes;
            ret
        }
    };
    tc.current_module = previous_module;
    ret
}

#[derive(Clone)]
struct CheckedType {
    ty: Type,
    handle: TypeHandle,
    contains_extern_any: bool,
}

fn checked_type(ty: Type, tc: &TypeChecker) -> CheckedType {
    CheckedType {
        handle: tc.type_handle(&ty),
        ty,
        contains_extern_any: false,
    }
}

fn checked_void(tc: &TypeChecker) -> CheckedType {
    checked_type(Type::Void, tc)
}

fn join_checked(
    left: CheckedType,
    left_span: Span,
    right: CheckedType,
    right_span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    let either_void = left.ty.is_void() || right.ty.is_void();
    if either_void {
        return checked_void(tc);
    }
    let result = tc.fresh_temp_handle(right_span);
    let contains_extern_any = left.contains_extern_any || right.contains_extern_any;
    tc.expect_assignable(left_span, left.handle, result.clone());
    tc.expect_assignable(right_span, right.handle, result.clone());
    tc.solve_constraints();
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
        contains_extern_any,
    }
}

fn check_block_checked(block: &BlockNode, tc: &mut TypeChecker) -> CheckedType {
    check_block_checked_with_hint(block, None, tc)
}

fn check_block_checked_with_hint(
    block: &BlockNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_scope();
    for stmt in &block.node.stmts {
        check_stmt(stmt, tc);
    }
    let checked = match &block.node.tail {
        Some(expr) => check_expr_checked_with_hint(expr, expected, tc),
        None => checked_void(tc),
    };
    tc.pop_scope();
    checked
}

fn check_binding(binding_node: &BindingNode, tc: &mut TypeChecker) {
    let binding = &binding_node.node;
    let mutable = matches!(binding.mutability, Mutability::Mutable);
    match &binding.ty {
        Some(annot) => {
            let annot_ty = tc.resolve_type_for_tc_at(annot, binding_node.span);
            let annot_handle = tc.type_handle(&annot_ty);
            let value =
                check_expr_checked_with_hint(&binding.value, Some(annot_handle.clone()), tc);
            tc.reject_extern_any_escape(&value, binding.value.span);
            tc.expect_assignable(binding_node.span, value.handle, annot_handle);
            record_extern_pattern_reads(&binding.pattern, &annot_ty, binding.value.node.id, tc);
            check_pattern(&binding.pattern, &annot_ty, mutable, tc);
        }
        None => {
            let value = check_expr_checked(&binding.value, tc);
            tc.reject_extern_any_escape(&value, binding.value.span);
            tc.reject_user_any_type(&value.ty, binding_node.span);
            record_extern_pattern_reads(&binding.pattern, &value.ty, binding.value.node.id, tc);
            check_pattern_from_handle(&binding.pattern, value.handle, value.ty, mutable, tc);
        }
    }
}

fn check_const(const_node: &ConstDeclNode, tc: &mut TypeChecker) {
    let c = &const_node.node;
    let value = match tc.eval_const_expr(&c.value) {
        Ok(value) => value,
        Err(err) => {
            tc.push_error(err);
            return;
        }
    };
    let value_ty = const_eval::const_type(&value);
    let ty = match &c.ty {
        Some(annot) => {
            let annot_ty = tc.resolve_type_for_tc_at(annot, const_node.span);
            tc.reject_user_any_type(&annot_ty, const_node.span);
            if annot_ty != value_ty {
                tc.push_error(TypeError::ConstTypeMismatch {
                    expected: annot_ty.clone(),
                    found: value_ty,
                    span: const_node.span,
                });
            }
            annot_ty
        }
        None => value_ty,
    };
    tc.define_const(c.name, ty, value);
}

fn check_return(ret_node: &ReturnNode, tc: &mut TypeChecker) {
    let ret = &ret_node.node;
    tc.mark_return();
    if let Some(expr) = &ret.value {
        if let Some(expected_ty) = tc.return_type().cloned() {
            let expected = tc.type_handle(&expected_ty);
            let actual = check_expr_checked_with_hint(expr, Some(expected.clone()), tc);
            tc.reject_extern_any_escape(&actual, expr.span);
            tc.expect_assignable(ret_node.span, actual.handle, expected);
        } else {
            let actual = check_expr_checked(expr, tc);
            tc.reject_extern_any_escape(&actual, expr.span);
        }
    } else if let Some(expected_ty) = tc.return_type().cloned()
        && !expected_ty.is_void()
    {
        tc.push_error(TypeError::MissingReturn {
            expected: expected_ty,
            span: ret_node.span,
        });
    }
}

fn check_expr_checked(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedType {
    check_expr_checked_with_hint(expr, None, tc)
}

fn checked_from_type(expr: &ExprNode, ty: Type, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_type(expr.node.id, ty.clone(), expr.span);
    CheckedType {
        ty,
        handle,
        contains_extern_any: false,
    }
}

fn checked_from_handle(expr: &ExprNode, handle: TypeHandle, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_type_from_handle(expr.node.id, expr.span, handle);
    let ty = tc.handle_type(&handle);
    CheckedType {
        ty,
        handle,
        contains_extern_any: false,
    }
}

fn checked_from_checked(
    expr: &ExprNode,
    checked: CheckedType,
    tc: &mut TypeChecker,
) -> CheckedType {
    let handle = tc.set_type_from_handle(expr.node.id, expr.span, checked.handle);
    let ty = tc.handle_type(&handle);
    CheckedType {
        ty,
        handle,
        contains_extern_any: checked.contains_extern_any,
    }
}

fn solve_and_checked_from_handle(
    expr: &ExprNode,
    handle: TypeHandle,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.solve_constraints();
    checked_from_handle(expr, handle, tc)
}

fn check_expected(expr: &ExprNode, expected: TypeHandle, tc: &mut TypeChecker) -> CheckedType {
    let checked = check_expr_checked_with_hint(expr, Some(expected.clone()), tc);
    tc.expect_assignable(expr.span, checked.handle.clone(), expected);
    checked
}

fn check_arg_count(
    args: &[ExprNode],
    expected: usize,
    call_span: Span,
    tc: &mut TypeChecker,
) -> bool {
    if args.len() == expected {
        return true;
    }

    tc.push_error(TypeError::WrongArgCount {
        expected,
        found: args.len(),
        span: call_span,
    });
    for arg in args {
        check_expr_checked(arg, tc);
    }
    false
}

fn check_expr_checked_with_hint(
    expr: &ExprNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    match &expr.node.kind {
        ExprKind::Lit(Lit::Nil) => match expected {
            Some(expected) => {
                let nil = tc.fresh_nil_handle(expr.span);
                tc.expect_assignable(expr.span, nil, expected.clone());
                checked_from_handle(expr, expected, tc)
            }
            None => {
                let handle = tc.set_nil_type(expr.node.id, expr.span);
                CheckedType {
                    ty: Type::Infer,
                    handle,
                    contains_extern_any: false,
                }
            }
        },
        ExprKind::Lit(lit) => checked_from_type(expr, type_from_lit(lit), tc),
        ExprKind::Ident(name) => match tc.lookup(*name).cloned() {
            Some(info) => {
                let fallback = tc.solver.local_type_to_type(info.type_id);
                if fallback != Type::Infer || info.const_value.is_some() {
                    checked_from_handle(expr, tc.local_handle(info.type_id), tc)
                } else {
                    match tc.eval_visible_const(*name, expr.span) {
                        Some(Ok(value)) => {
                            checked_from_type(expr, const_eval::const_type(&value), tc)
                        }
                        Some(Err(err)) => {
                            tc.push_error(err);
                            checked_from_type(expr, Type::Infer, tc)
                        }
                        None => checked_from_handle(expr, tc.local_handle(info.type_id), tc),
                    }
                }
            }
            None => {
                let ty = match tc.eval_visible_const(*name, expr.span) {
                    Some(Ok(value)) => const_eval::const_type(&value),
                    Some(Err(err)) => {
                        tc.push_error(err);
                        Type::Infer
                    }
                    None => tc.lookup_imported_value(*name).unwrap_or_else(|| {
                        tc.push_error(TypeError::UndefinedVariable {
                            name: *name,
                            span: expr.span,
                        });
                        Type::Infer
                    }),
                };
                checked_from_type(expr, ty, tc)
            }
        },
        ExprKind::Binary(bin_node) => {
            checked_from_checked(expr, check_binary(expr.node.id, bin_node, tc), tc)
        }
        ExprKind::Unary(unary_node) => {
            checked_from_checked(expr, check_unary(expr.node.id, unary_node, tc), tc)
        }
        ExprKind::Block(block_node) => checked_from_checked(
            expr,
            check_block_checked_with_hint(block_node, expected, tc),
            tc,
        ),
        ExprKind::If(if_node) => {
            checked_from_checked(expr, check_if_checked_with_hint(if_node, expected, tc), tc)
        }
        ExprKind::Assign(assign_node) => {
            check_assign(expr.node.id, assign_node, tc);
            checked_from_type(expr, Type::Void, tc)
        }
        ExprKind::StructLiteral(lit) => check_struct_lit_hint(expr, lit, expected, tc),
        ExprKind::InferredEnum(node) => check_inferred_enum_hint(expr, node, expected, tc),
        ExprKind::Field(_) | ExprKind::Call(_) => {
            let chain = collect_postfix_chain(expr).expect("postfix chain");
            check_postfix_chain(&chain, expr, expected.as_ref(), tc)
        }
        ExprKind::Tuple(elems) => check_tuple_checked_with_hint(expr, elems, expected, tc),
        ExprKind::NamedTuple(fields) => {
            check_named_tuple_checked_with_hint(expr, fields, expected, tc)
        }
        ExprKind::ArrayLiteral(lit) => check_array_lit_hint(expr, lit, expected, tc),
        ExprKind::ArrayFill(fill) => check_array_fill_hint(expr, fill, expected, tc),
        ExprKind::IfLet(if_let_node) => checked_from_checked(
            expr,
            check_if_let_checked_with_hint(if_let_node, expected, tc),
            tc,
        ),
        ExprKind::Match(match_node) => checked_from_checked(
            expr,
            check_match_checked_with_hint(match_node, expected, tc),
            tc,
        ),
        _ => checked_from_type(expr, Type::Void, tc),
    }
}

fn type_from_lit(lit: &Lit) -> Type {
    match lit {
        Lit::Int(_) => Type::Int,
        Lit::Float(_) => Type::Float,
        Lit::Bool(_) => Type::Bool,
        Lit::String(_) => Type::String,
        Lit::Nil => Type::Infer,
    }
}

fn check_binary(expr_id: ExprId, bin: &BinaryNode, tc: &mut TypeChecker) -> CheckedType {
    let left = check_expr_checked(&bin.node.left, tc);
    let right = check_expr_checked(&bin.node.right, tc);
    check_binary_checked(
        expr_id,
        bin.node.op,
        &bin.node.left,
        left,
        &bin.node.right,
        right,
        bin.span,
        tc,
    )
}

fn check_binary_checked(
    expr_id: ExprId,
    op: BinaryOp,
    left_expr: &ExprNode,
    left: CheckedType,
    right_expr: &ExprNode,
    right: CheckedType,
    span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    match builtin_binary_type(op, &left.ty, &right.ty, tc) {
        Ok(ty) => checked_type(ty, tc),
        Err(failure) => {
            extern_ops::check_binary(expr_id, op, left_expr, &left, right_expr, &right, span, tc)
                .unwrap_or_else(|| checked_type(emit_binary_failure(failure, span, tc), tc))
        }
    }
}

#[derive(Debug)]
enum BinaryTypeFailure {
    InvalidOperand {
        op: String,
        operand_type: Type,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        fallback: Type,
    },
}

fn builtin_binary_type(
    op: BinaryOp,
    left_ty: &Type,
    right_ty: &Type,
    tc: &TypeChecker,
) -> Result<Type, BinaryTypeFailure> {
    let same = left_ty == right_ty;
    match op {
        BinaryOp::Add => {
            let string_pair = left_ty.is_str() && right_ty.is_str();
            let string_lhs = left_ty.is_str() && right_ty.is_stringable();
            let string_rhs = right_ty.is_str() && left_ty.is_stringable();
            if string_pair || string_lhs || string_rhs {
                return Ok(Type::String);
            }
            if left_ty.is_num() && same {
                return Ok(left_ty.clone());
            }
            Err(BinaryTypeFailure::InvalidOperand {
                op: op.to_string(),
                operand_type: right_ty.clone(),
            })
        }
        BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
            if left_ty.is_num() && same {
                return Ok(left_ty.clone());
            }
            Err(BinaryTypeFailure::InvalidOperand {
                op: op.to_string(),
                operand_type: right_ty.clone(),
            })
        }
        BinaryOp::Eq | BinaryOp::NotEq => {
            let extern_eq = same && tc.extern_type_id(left_ty).is_some();
            if same && !extern_eq {
                Ok(Type::Bool)
            } else if extern_eq {
                Err(BinaryTypeFailure::InvalidOperand {
                    op: op.to_string(),
                    operand_type: right_ty.clone(),
                })
            } else {
                Err(BinaryTypeFailure::TypeMismatch {
                    expected: left_ty.clone(),
                    found: right_ty.clone(),
                    fallback: Type::Bool,
                })
            }
        }
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEq
        | BinaryOp::GreaterThanEq => {
            if left_ty.is_num() && same {
                Ok(Type::Bool)
            } else {
                Err(BinaryTypeFailure::TypeMismatch {
                    expected: left_ty.clone(),
                    found: right_ty.clone(),
                    fallback: Type::Bool,
                })
            }
        }
        BinaryOp::And | BinaryOp::Or => {
            if left_ty.is_bool() && same {
                Ok(Type::Bool)
            } else {
                Err(BinaryTypeFailure::TypeMismatch {
                    expected: Type::Bool,
                    found: left_ty.clone(),
                    fallback: Type::Bool,
                })
            }
        }
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor | BinaryOp::Shl | BinaryOp::Shr => {
            if left_ty.is_int() && same {
                Ok(Type::Int)
            } else {
                Err(BinaryTypeFailure::TypeMismatch {
                    expected: Type::Int,
                    found: left_ty.clone(),
                    fallback: Type::Int,
                })
            }
        }
        BinaryOp::Coalesce => Err(BinaryTypeFailure::InvalidOperand {
            op: op.to_string(),
            operand_type: left_ty.clone(),
        }),
    }
}

fn emit_binary_failure(failure: BinaryTypeFailure, span: Span, tc: &mut TypeChecker) -> Type {
    match failure {
        BinaryTypeFailure::InvalidOperand { op, operand_type } => {
            tc.push_error(TypeError::InvalidOperand {
                op,
                operand_type,
                span,
            });
            Type::Infer
        }
        BinaryTypeFailure::TypeMismatch {
            expected,
            found,
            fallback,
        } => {
            tc.push_error(TypeError::TypeMismatch {
                expected,
                found,
                span,
            });
            fallback
        }
    }
}

fn check_unary(expr_id: ExprId, unary: &UnaryNode, tc: &mut TypeChecker) -> CheckedType {
    let operand = check_expr_checked(&unary.node.expr, tc);
    match builtin_unary_type(unary.node.op, &operand.ty) {
        Ok(ty) => checked_type(ty, tc),
        Err(failure) => extern_ops::check_unary(expr_id, unary.node.op, &operand, tc)
            .unwrap_or_else(|| checked_type(emit_unary_failure(failure, unary.span, tc), tc)),
    }
}

#[derive(Debug)]
struct UnaryTypeFailure {
    op: String,
    operand_type: Type,
}

fn builtin_unary_type(op: UnaryOp, operand_ty: &Type) -> Result<Type, UnaryTypeFailure> {
    match op {
        UnaryOp::Neg => {
            if operand_ty.is_num() {
                Ok(operand_ty.clone())
            } else {
                Err(UnaryTypeFailure {
                    op: op.to_string(),
                    operand_type: operand_ty.clone(),
                })
            }
        }
        UnaryOp::Not => {
            if operand_ty.is_bool() {
                Ok(Type::Bool)
            } else {
                Err(UnaryTypeFailure {
                    op: op.to_string(),
                    operand_type: operand_ty.clone(),
                })
            }
        }
        UnaryOp::BitNot => {
            if operand_ty.is_int() {
                Ok(Type::Int)
            } else {
                Err(UnaryTypeFailure {
                    op: op.to_string(),
                    operand_type: operand_ty.clone(),
                })
            }
        }
    }
}

fn emit_unary_failure(failure: UnaryTypeFailure, span: Span, tc: &mut TypeChecker) -> Type {
    tc.push_error(TypeError::InvalidOperand {
        op: failure.op,
        operand_type: failure.operand_type,
        span,
    });
    Type::Infer
}

fn check_bool_condition(
    cond: CheckedType,
    span: Span,
    error: impl FnOnce(Type, Span) -> TypeError,
    tc: &mut TypeChecker,
) {
    if cond.ty.is_bool() {
        return;
    }
    if cond.ty == Type::Infer {
        let bool_handle = tc.type_handle(&Type::Bool);
        tc.expect_assignable(span, cond.handle, bool_handle);
    } else {
        tc.push_error(error(cond.ty, span));
    }
}

fn check_if_checked_with_hint(
    if_node: &IfNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let cond = check_expr_checked(&if_node.node.cond, tc);
    check_bool_condition(
        cond,
        if_node.node.cond.span,
        |found, span| TypeError::IfConditionNotBool { found, span },
        tc,
    );
    let Some(else_block) = &if_node.node.else_block else {
        check_block_checked(&if_node.node.then_block, tc);
        return checked_void(tc);
    };
    let then = check_block_checked_with_hint(&if_node.node.then_block, expected.clone(), tc);
    let else_checked = check_block_checked_with_hint(else_block, expected.clone(), tc);
    if let Some(expected) = expected {
        let contains_extern_any = then.contains_extern_any || else_checked.contains_extern_any;
        tc.expect_assignable(if_node.node.then_block.span, then.handle, expected.clone());
        tc.expect_assignable(else_block.span, else_checked.handle, expected.clone());
        tc.solve_constraints();
        return CheckedType {
            ty: tc.handle_type(&expected),
            handle: expected,
            contains_extern_any,
        };
    }
    join_checked(
        then,
        if_node.node.then_block.span,
        else_checked,
        else_block.span,
        tc,
    )
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CollectionLiteralKind {
    Array,
    List,
}

fn expected_collection(
    expected: Option<&TypeHandle>,
    tc: &TypeChecker,
) -> Option<(TypeHandle, CollectionLiteralKind)> {
    match expected.map(|handle| tc.handle_type(handle))? {
        Type::Array { elem, .. } | Type::Slice { elem } => {
            Some((tc.type_handle(&elem), CollectionLiteralKind::Array))
        }
        Type::List { elem } => Some((tc.type_handle(&elem), CollectionLiteralKind::List)),
        _ => None,
    }
}

fn collection_literal_handle(
    kind: CollectionLiteralKind,
    elem: TypeHandle,
    len: ArrayLen,
    tc: &mut TypeChecker,
) -> TypeHandle {
    match kind {
        CollectionLiteralKind::Array => tc.array_handle(elem, len),
        CollectionLiteralKind::List => tc.list_handle(elem),
    }
}

fn check_array_lit_hint(
    expr: &ExprNode,
    lit: &ArrayLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let (elem, kind) = expected_collection(expected.as_ref(), tc)
        .unwrap_or_else(|| (tc.fresh_temp_handle(lit.span), CollectionLiteralKind::Array));
    let array = collection_literal_handle(
        kind,
        elem.clone(),
        ArrayLen::Fixed(lit.node.elements.len()),
        tc,
    );
    let mut contains_extern_any = false;
    for value in &lit.node.elements {
        let checked = check_expected(value, elem.clone(), tc);
        contains_extern_any |= checked.contains_extern_any;
    }
    let mut checked = solve_and_checked_from_handle(expr, array, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn check_array_fill_hint(
    expr: &ExprNode,
    fill: &ArrayFillNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let len = match tc.eval_const_expr(&fill.node.len) {
        Ok(const_value) => match const_eval::const_usize(&const_value, fill.node.len.span) {
            Ok(len) => ArrayLen::Fixed(len),
            Err(err) => {
                tc.push_error(err);
                ArrayLen::Infer
            }
        },
        Err(err) => {
            tc.push_error(err);
            ArrayLen::Infer
        }
    };
    let (elem, kind) = expected_collection(expected.as_ref(), tc).unwrap_or_else(|| {
        (
            tc.fresh_temp_handle(fill.node.value.span),
            CollectionLiteralKind::Array,
        )
    });
    let value = check_expected(&fill.node.value, elem.clone(), tc);
    let array = collection_literal_handle(kind, elem, len, tc);
    let mut checked = solve_and_checked_from_handle(expr, array, tc);
    checked.contains_extern_any = value.contains_extern_any;
    checked
}

fn tuple_hints(
    elems: &[ExprNode],
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Vec<TypeHandle> {
    match expected.map(|handle| tc.handle_type(handle)) {
        Some(Type::Tuple(types)) if types.len() == elems.len() => {
            types.iter().map(|ty| tc.type_handle(ty)).collect()
        }
        _ => elems
            .iter()
            .map(|elem| tc.fresh_temp_handle(elem.span))
            .collect(),
    }
}

fn check_tuple_checked_with_hint(
    expr: &ExprNode,
    elems: &[ExprNode],
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let hints = tuple_hints(elems, expected.as_ref(), tc);
    let mut contains_extern_any = false;
    for (elem, hint) in elems.iter().zip(&hints) {
        let checked = check_expected(elem, hint.clone(), tc);
        contains_extern_any |= checked.contains_extern_any;
    }
    let tuple = tc.tuple_handle(hints);
    let mut checked = solve_and_checked_from_handle(expr, tuple, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn named_tuple_hints(
    fields: &[(Ident, ExprNode)],
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Vec<TypeHandle> {
    if let Some(Type::NamedTuple(expected_fields)) = expected.map(|handle| tc.handle_type(handle)) {
        let same_labels = expected_fields.len() == fields.len()
            && expected_fields
                .iter()
                .zip(fields)
                .all(|((expected, _), (found, _))| expected == found);
        if same_labels {
            return expected_fields
                .iter()
                .map(|(_, ty)| tc.type_handle(ty))
                .collect();
        }
    }
    fields
        .iter()
        .map(|(_, expr)| tc.fresh_temp_handle(expr.span))
        .collect()
}

fn check_named_tuple_checked_with_hint(
    expr: &ExprNode,
    fields: &[(Ident, ExprNode)],
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let hints = named_tuple_hints(fields, expected.as_ref(), tc);
    let mut contains_extern_any = false;
    for ((_, value), hint) in fields.iter().zip(&hints) {
        let checked = check_expected(value, hint.clone(), tc);
        contains_extern_any |= checked.contains_extern_any;
    }
    let fields = fields
        .iter()
        .zip(hints)
        .map(|((name, _), handle)| (*name, handle))
        .collect();
    let tuple = tc.named_tuple_handle(fields);
    let mut checked = solve_and_checked_from_handle(expr, tuple, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

struct NominalLiteralSolver {
    vars: GenericSolverVars,
}

impl NominalLiteralSolver {
    fn new(
        generics: &GenericParams,
        args: &[GenericArg],
        span: Span,
        tc: &mut TypeChecker,
    ) -> Option<Self> {
        let seeds = if args.is_empty() {
            GenericSolverSeeds::default()
        } else {
            let args = bind_exact_generic_args(tc, generics, args, span)?;
            GenericSolverSeeds::from_args(generics, &args)
        };
        Some(Self::from_seeds(generics, &seeds, span, tc))
    }

    fn without_args(generics: &GenericParams, span: Span, tc: &mut TypeChecker) -> Self {
        Self::from_seeds(generics, &GenericSolverSeeds::default(), span, tc)
    }

    fn from_seeds(
        generics: &GenericParams,
        seeds: &GenericSolverSeeds,
        span: Span,
        tc: &mut TypeChecker,
    ) -> Self {
        Self {
            vars: tc.solver.generic_solver_vars(generics, seeds, span),
        }
    }

    fn bind_expected(
        &self,
        key: &NominalKey,
        generics: &GenericParams,
        expected: Option<&Type>,
        span: Span,
        tc: &mut TypeChecker,
    ) -> bool {
        let Some(expected) = expected else {
            return true;
        };
        if tc.decls.key_for_type(expected).as_ref() != Some(key) {
            return true;
        }
        let template = nominal_literal_type(key, generics, None);
        let template = tc.solver.instantiate_generic_type(&template, &self.vars);
        let expected = tc.type_handle(expected);
        tc.expect_equal(span, template, expected);
        !tc.solve_constraints()
    }

    fn instantiate(&self, ty: &Type, tc: &mut TypeChecker) -> TypeHandle {
        tc.solver.instantiate_generic_type(ty, &self.vars)
    }

    fn finalize(
        &self,
        key: &NominalKey,
        generics: &GenericParams,
        span: Span,
        tc: &mut TypeChecker,
    ) -> Option<Type> {
        let args = match tc.solver.finalize_generic_args(generics, &self.vars) {
            Ok(args) => args,
            Err(unbound) => {
                tc.push_unbound_generic_errors(unbound, span);
                return None;
            }
        };
        Some(nominal_literal_type(key, generics, Some(&args)))
    }
}

fn check_struct_lit_hint(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(key) = resolve_struct_key(lit, tc) else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    if key.kind == NominalKind::Extern {
        return check_extern_lit(expr, lit, &key, expected, tc);
    }

    let valid_literal_target = matches!(key.kind, NominalKind::Struct | NominalKind::DataRef);
    if !valid_literal_target {
        let kind = match key.kind {
            NominalKind::Enum => "enum",
            NominalKind::Extern => "extern",
            NominalKind::Struct | NominalKind::DataRef => unreachable!(),
        };
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: kind.to_string(),
            span: lit.span,
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let agg = tc
        .decls
        .aggregate(&key)
        .expect("aggregate exists for resolved key")
        .clone();
    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    let Some(inf) = NominalLiteralSolver::new(&agg.generics, &lit.node.generic_args, lit.span, tc)
    else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };
    let expected_ok = inf.bind_expected(&key, &agg.generics, expected_ty.as_ref(), lit.span, tc);
    let field_check = check_nominal_fields(
        &lit.node.fields,
        &agg.fields,
        nominal_type(&key),
        lit.span,
        &inf,
        tc,
    );
    if !expected_ok || field_check.failed {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let Some(ty) = inf.finalize(&key, &agg.generics, lit.span, tc) else {
        return checked_from_type(expr, Type::Infer, tc);
    };
    tc.reject_user_any_type(&ty, lit.span);
    let handle = tc.type_handle(&ty);
    let mut checked = solve_and_checked_from_handle(expr, handle, tc);
    checked.contains_extern_any = field_check.contains_extern_any;
    checked
}

fn check_extern_lit(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    key: &NominalKey,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(owner) = tc.externs.type_by_nominal(key) else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: "extern".to_string(),
            span: lit.span,
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    if !lit.node.generic_args.is_empty() {
        tc.push_error(TypeError::GenericArity(ArityError::TypeArgs {
            expected: 0,
            found: lit.node.generic_args.len(),
        }));
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let Some(init) = tc.externs.init(owner).cloned() else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: "extern".to_string(),
            span: lit.span,
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    if let Some(expected_ty) = expected_ty.as_ref()
        && tc.decls.key_for_type(expected_ty).as_ref() == Some(key)
    {
        let expected = tc.type_handle(expected_ty);
        let actual = tc.type_handle(&nominal_type(key));
        tc.expect_equal(lit.span, actual, expected);
    }

    let fields_failed =
        check_extern_literal_fields(&lit.node.fields, owner, &init.field_init, lit.span, tc);
    if fields_failed {
        return checked_from_type(expr, Type::Infer, tc);
    }

    tc.record_extern_use(expr.node.id, ExternUseTarget::Init(owner));
    let ty = nominal_type(key);
    let handle = tc.type_handle(&ty);
    solve_and_checked_from_handle(expr, handle, tc)
}

fn check_extern_literal_fields(
    fields: &[(Ident, ExprNode)],
    owner: ExternTypeId,
    explicit_init: &[Ident],
    span: Span,
    tc: &mut TypeChecker,
) -> bool {
    let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
    let mut seen = HashMap::new();
    let mut failed = false;
    for (name, value) in fields {
        if seen.insert(*name, value.span).is_some() {
            tc.push_error(TypeError::DuplicateField {
                name: *name,
                span: value.span,
            });
            failed = true;
        }

        let Some((_, field)) = tc.extern_field(owner, *name) else {
            tc.push_error(TypeError::UnknownMember {
                ty: owner_ty.clone(),
                member: *name,
                kind: MemberAccessKind::Field,
                span: value.span,
            });
            check_expr_checked(value, tc);
            failed = true;
            continue;
        };

        let access = field.access;
        let field_ty = field.ty.clone();
        let allowed = if explicit_init.is_empty() {
            !matches!(
                access,
                anvyx_externs::FieldAccess::ReadOnly { computed: true }
                    | anvyx_externs::FieldAccess::ReadWrite { computed: true }
            )
        } else {
            explicit_init.contains(name)
        };
        if !allowed {
            tc.push_error(TypeError::ImmutableAssignment {
                name: *name,
                span: value.span,
            });
            failed = true;
        }
        let hint = tc.type_handle(&field_ty.ty);
        let checked = check_expr_checked_with_hint(value, Some(hint), tc);
        failed |= !extern_boundary::check_checked_value(value, &checked, &field_ty, tc);
    }

    for name in required_extern_literal_fields(owner, explicit_init, tc) {
        if !seen.contains_key(&name) {
            tc.push_error(TypeError::MissingField { name, span });
            failed = true;
        }
    }
    failed
}

fn required_extern_literal_fields(
    owner: ExternTypeId,
    explicit_init: &[Ident],
    tc: &TypeChecker,
) -> Vec<Ident> {
    if !explicit_init.is_empty() {
        return explicit_init.to_vec();
    }
    tc.extern_type(owner)
        .fields
        .iter()
        .filter(|field| {
            !matches!(
                field.access,
                anvyx_externs::FieldAccess::ReadOnly { computed: true }
                    | anvyx_externs::FieldAccess::ReadWrite { computed: true }
            )
        })
        .map(|field| field.name)
        .collect()
}

fn check_inferred_enum_hint(
    expr: &ExprNode,
    node: &InferredEnumNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(expected) = expected else {
        return cannot_infer_inferred_enum(expr, node, tc);
    };
    let expected_ty = tc.handle_type(&expected);
    let expected_key = tc
        .decls
        .key_for_type(&expected_ty)
        .filter(|key| key.kind == NominalKind::Enum);
    let Some(key) = expected_key else {
        return cannot_infer_inferred_enum(expr, node, tc);
    };

    let Some(schema) = tc.decls.enum_schema(&key) else {
        return checked_from_type(expr, Type::Infer, tc);
    };
    let generics = schema.generics.clone();
    let Some(variant) = schema.variants.get(&node.node.variant).cloned() else {
        tc.push_error(TypeError::UnknownEnumVariant {
            enum_name: key.name,
            variant: node.node.variant,
            span: node.span,
        });
        check_inferred_enum_args(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    let inf = NominalLiteralSolver::without_args(&generics, node.span, tc);
    if !inf.bind_expected(&key, &generics, Some(&expected_ty), node.span, tc) {
        check_inferred_enum_args(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let mut contains_extern_any = false;
    match (&variant, &node.node.args) {
        (VariantSchema::Unit, InferredEnumArgs::Unit) => {}
        (VariantSchema::Unit, args) => {
            return wrong_inferred_enum_args(expr, node, 0, args, tc);
        }
        (VariantSchema::Tuple(params), InferredEnumArgs::Tuple(args)) => {
            if params.len() != args.len() {
                tc.push_error(TypeError::WrongArgCount {
                    expected: params.len(),
                    found: args.len(),
                    span: node.span,
                });
                check_exprs_without_hint(args, tc);
                return checked_from_type(expr, Type::Infer, tc);
            }
            let mut failed = false;
            for (arg, param) in args.iter().zip(params) {
                let hint = inf.instantiate(param, tc);
                let checked = check_expected(arg, hint, tc);
                contains_extern_any |= checked.contains_extern_any;
                failed |= tc.solve_constraints();
            }
            if failed || inf.finalize(&key, &generics, node.span, tc).is_none() {
                return checked_from_type(expr, Type::Infer, tc);
            }
        }
        (VariantSchema::Tuple(params), args) => {
            return wrong_inferred_enum_args(expr, node, params.len(), args, tc);
        }
        (VariantSchema::Struct(fields), InferredEnumArgs::Struct(args)) => {
            let field_check =
                check_nominal_fields(args, fields, expected_ty.clone(), node.span, &inf, tc);
            contains_extern_any |= field_check.contains_extern_any;
            if field_check.failed || inf.finalize(&key, &generics, node.span, tc).is_none() {
                return checked_from_type(expr, Type::Infer, tc);
            }
        }
        (VariantSchema::Struct(fields), args) => {
            return wrong_inferred_enum_args(expr, node, fields.len(), args, tc);
        }
    }

    let mut checked = solve_and_checked_from_handle(expr, expected, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn cannot_infer_inferred_enum(
    expr: &ExprNode,
    node: &InferredEnumNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_error(TypeError::CannotInferType { span: node.span });
    check_inferred_enum_args(&node.node.args, tc);
    checked_from_type(expr, Type::Infer, tc)
}

fn wrong_inferred_enum_args(
    expr: &ExprNode,
    node: &InferredEnumNode,
    expected: usize,
    args: &InferredEnumArgs,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_error(TypeError::WrongArgCount {
        expected,
        found: inferred_enum_arg_count(args),
        span: node.span,
    });
    check_inferred_enum_args(args, tc);
    checked_from_type(expr, Type::Infer, tc)
}

#[derive(Default)]
struct NominalFieldCheck {
    failed: bool,
    contains_extern_any: bool,
}

fn check_nominal_fields(
    fields: &[(Ident, ExprNode)],
    schema: &HashMap<Ident, FieldSchema>,
    owner_ty: Type,
    span: Span,
    inf: &NominalLiteralSolver,
    tc: &mut TypeChecker,
) -> NominalFieldCheck {
    let mut seen = HashMap::new();
    let mut check = NominalFieldCheck::default();
    for (name, value) in fields {
        if seen.insert(*name, value.span).is_some() {
            tc.push_error(TypeError::DuplicateField {
                name: *name,
                span: value.span,
            });
        }
        match schema.get(name) {
            Some(field) => {
                let hint = inf.instantiate(&field.ty, tc);
                let checked = check_expr_checked_with_hint(value, Some(hint.clone()), tc);
                check.contains_extern_any |= checked.contains_extern_any;
                tc.expect_assignable(value.span, checked.handle, hint);
                check.failed |= tc.solve_constraints();
            }
            None => {
                tc.push_error(TypeError::UnknownMember {
                    ty: owner_ty.clone(),
                    member: *name,
                    kind: MemberAccessKind::Field,
                    span: value.span,
                });
                check_expr_checked(value, tc);
            }
        }
    }

    for (name, field) in schema {
        let missing_required_field = !seen.contains_key(name) && !field.has_default;
        if missing_required_field {
            tc.push_error(TypeError::MissingField { name: *name, span });
        }
    }
    check
}

fn check_unknown_nominal_fields(fields: &[(Ident, ExprNode)], tc: &mut TypeChecker) {
    for (_, value) in fields {
        check_expr_checked(value, tc);
    }
}

fn check_inferred_enum_args(args: &InferredEnumArgs, tc: &mut TypeChecker) {
    match args {
        InferredEnumArgs::Unit => {}
        InferredEnumArgs::Tuple(args) => check_exprs_without_hint(args, tc),
        InferredEnumArgs::Struct(fields) => check_unknown_nominal_fields(fields, tc),
    }
}

fn check_exprs_without_hint(args: &[ExprNode], tc: &mut TypeChecker) {
    for arg in args {
        check_expr_checked(arg, tc);
    }
}

fn inferred_enum_arg_count(args: &InferredEnumArgs) -> usize {
    match args {
        InferredEnumArgs::Unit => 0,
        InferredEnumArgs::Tuple(args) => args.len(),
        InferredEnumArgs::Struct(fields) => fields.len(),
    }
}

fn nominal_literal_type(
    key: &NominalKey,
    generics: &GenericParams,
    args: Option<&GenericArgs>,
) -> Type {
    if let Some(args) = args {
        let const_args = ConstTerm::to_args_no_infer(&args.const_args)
            .expect("nominal literal finalization must not produce inference const terms");
        return nominal_type_with_args(key, &args.type_args, &const_args);
    }

    let type_args = generics
        .type_params
        .iter()
        .map(|param| Type::Var(param.id))
        .collect::<Vec<_>>();
    let const_args = generics
        .const_params
        .iter()
        .map(|param| ConstArg::Param(param.id))
        .collect::<Vec<_>>();
    nominal_type_with_args(key, &type_args, &const_args)
}

fn resolve_struct_key(lit: &StructLiteralNode, tc: &mut TypeChecker) -> Option<NominalKey> {
    let key = tc.resolve_visible_type_key(lit.node.qualifier, lit.node.name);
    if key.is_none() {
        tc.push_error(TypeError::UnknownStructLiteral {
            qualifier: lit.node.qualifier,
            name: lit.node.name,
            span: lit.span,
        });
    }
    key
}

fn check_assign(expr_id: ExprId, assign: &AssignNode, tc: &mut TypeChecker) {
    let target = check_place(&assign.node.target, tc);
    if !target.value.access.can_assign() {
        let name = assignment_target_name(&assign.node.target);
        tc.push_error(TypeError::ImmutableAssignment {
            name,
            span: assign.node.target.span,
        });
    }

    match assign_op_to_binary_op(assign.node.op) {
        None => {
            let value = check_expr_checked_with_hint(
                &assign.node.value,
                Some(target.checked().handle.clone()),
                tc,
            );
            if !target.accepts_extern_any() {
                tc.reject_extern_any_escape(&value, assign.node.value.span);
            }
            if !target.checked().ty.is_void() && !value.ty.is_void() {
                tc.expect_assignable(
                    assign.node.value.span,
                    value.handle,
                    target.checked().handle.clone(),
                );
            }
            if target.value.access.can_assign() {
                place::record_write(assign.node.target.node.id, &target, tc);
            }
        }
        Some(op) => {
            let value = check_expr_checked(&assign.node.value, tc);
            let target_value = CheckedType {
                ty: target.checked().ty.clone(),
                handle: target.checked().handle.clone(),
                contains_extern_any: target.checked().contains_extern_any,
            };
            let result = check_binary_checked(
                expr_id,
                op,
                &assign.node.target,
                target_value,
                &assign.node.value,
                value,
                assign.span,
                tc,
            );
            if !target.checked().ty.is_void() && !result.ty.is_void() {
                tc.expect_assignable(assign.span, result.handle, target.checked().handle.clone());
            }
            if target.value.access.can_assign() {
                place::record_compound_write(assign.node.target.node.id, &target, tc);
            }
        }
    }
}

fn assignment_target_name(expr: &ExprNode) -> Ident {
    match &expr.node.kind {
        ExprKind::Ident(name) => *name,
        ExprKind::Field(field) => field.node.field,
        _ => Ident::new("<target>"),
    }
}

fn assign_op_to_binary_op(op: AssignOp) -> Option<BinaryOp> {
    match op {
        AssignOp::Assign => None,
        AssignOp::AddAssign => Some(BinaryOp::Add),
        AssignOp::SubAssign => Some(BinaryOp::Sub),
        AssignOp::MulAssign => Some(BinaryOp::Mul),
        AssignOp::DivAssign => Some(BinaryOp::Div),
        AssignOp::XorAssign => Some(BinaryOp::Xor),
        AssignOp::BitAndAssign => Some(BinaryOp::BitAnd),
        AssignOp::BitOrAssign => Some(BinaryOp::BitOr),
        AssignOp::ShlAssign => Some(BinaryOp::Shl),
        AssignOp::ShrAssign => Some(BinaryOp::Shr),
    }
}

fn check_while(while_node: &WhileNode, tc: &mut TypeChecker) {
    let cond = check_expr_checked(&while_node.node.cond, tc);
    check_bool_condition(
        cond,
        while_node.node.cond.span,
        |found, span| TypeError::WhileConditionNotBool { found, span },
        tc,
    );
    tc.enter_loop();
    check_block_checked(&while_node.node.body, tc);
    tc.exit_loop();
}

fn check_for(for_node: &ForNode, tc: &mut TypeChecker) {
    let node = &for_node.node;
    let iterable_ty = check_expr_checked(&node.iterable, tc).ty;

    if let Some(step) = &node.step {
        check_expr_checked(step, tc);
    }

    let item_ty = iterable_item_type(&iterable_ty).unwrap_or_else(|| {
        tc.push_error(TypeError::ForIterableNotSupported {
            found: iterable_ty,
            span: node.iterable.span,
        });
        Type::Infer
    });

    tc.push_scope();
    check_pattern(&node.pattern, &item_ty, false, tc);
    tc.enter_loop();
    check_block_checked(&node.body, tc);
    tc.exit_loop();
    tc.pop_scope();
}

fn check_break(span: Span, tc: &mut TypeChecker) {
    if !tc.in_loop() {
        tc.push_error(TypeError::BreakOutsideLoop { span });
    }
}

fn check_continue(span: Span, tc: &mut TypeChecker) {
    if !tc.in_loop() {
        tc.push_error(TypeError::ContinueOutsideLoop { span });
    }
}

fn iterable_item_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            Some((**elem).clone())
        }
        Type::Map { key, value } => Some(Type::Tuple(vec![(**key).clone(), (**value).clone()])),
        Type::Infer => Some(Type::Infer),
        _ => None,
    }
}

fn check_pattern_from_handle(
    pattern: &PatternNode,
    expected_handle: TypeHandle,
    expected_ty: Type,
    mutable: bool,
    tc: &mut TypeChecker,
) {
    match &pattern.node {
        Pattern::Ident(name) => {
            tc.define(*name, expected_ty, mutable);
            if let Some(type_id) = tc.lookup(*name).map(|info| info.type_id) {
                tc.set_local_type_from_handle(type_id, expected_handle.clone());
                tc.expect_equal(pattern.span, tc.local_handle(type_id), expected_handle);
            }
        }
        _ => check_pattern(pattern, &expected_ty, mutable, tc),
    }
}

fn check_pattern(pattern: &PatternNode, expected: &Type, mutable: bool, tc: &mut TypeChecker) {
    match &pattern.node {
        Pattern::Ident(name) => {
            tc.define(*name, expected.clone(), mutable);
        }
        Pattern::Wildcard => {}
        Pattern::VarIdent(_) => {
            tc.push_error(TypeError::UnsupportedPattern {
                pattern: pattern.node.variant_name(),
                span: pattern.span,
            });
        }
        Pattern::Tuple(elems) => {
            let elem_tys = match expected {
                Type::Tuple(tys) => tys.clone(),
                _ => {
                    tc.push_error(TypeError::TypeMismatch {
                        expected: expected.clone(),
                        found: Type::Tuple(elems.iter().map(|_| Type::Infer).collect()),
                        span: pattern.span,
                    });
                    return;
                }
            };
            let same_arity = elems.len() == elem_tys.len();
            if !same_arity {
                tc.push_error(TypeError::TuplePatternArityMismatch {
                    expected: elem_tys.len(),
                    found: elems.len(),
                    span: pattern.span,
                });
                return;
            }
            for (elem, elem_ty) in elems.iter().zip(elem_tys.iter()) {
                check_pattern(elem, elem_ty, mutable, tc);
            }
        }
        Pattern::NamedTuple(fields) => {
            let (elem_tys, labels) = match expected {
                Type::NamedTuple(fs) => (
                    fs.iter().map(|(_, t)| t.clone()).collect::<Vec<_>>(),
                    fs.iter().map(|(n, _)| *n).collect::<Vec<_>>(),
                ),
                Type::Tuple(_) => {
                    tc.push_error(TypeError::NamedPatternOnPositional { span: pattern.span });
                    return;
                }
                _ => {
                    tc.push_error(TypeError::TypeMismatch {
                        expected: expected.clone(),
                        found: Type::Tuple(fields.iter().map(|_| Type::Infer).collect()),
                        span: pattern.span,
                    });
                    return;
                }
            };
            let same_arity = fields.len() == elem_tys.len();
            if !same_arity {
                tc.push_error(TypeError::TuplePatternArityMismatch {
                    expected: elem_tys.len(),
                    found: fields.len(),
                    span: pattern.span,
                });
                return;
            }
            for ((field_name, subpat), (exp_ty, label)) in
                fields.iter().zip(elem_tys.iter().zip(labels.iter()))
            {
                if *field_name != *label {
                    tc.push_error(TypeError::TuplePatternLabelMismatch {
                        expected: *label,
                        found: *field_name,
                        span: pattern.span,
                    });
                }
                check_pattern(subpat, exp_ty, mutable, tc);
            }
        }
        Pattern::Lit(lit) => {
            let lit_ty = type_from_lit(lit);
            if lit_ty != *expected && !matches!(expected, Type::Infer) {
                tc.push_error(TypeError::TypeMismatch {
                    expected: expected.clone(),
                    found: lit_ty,
                    span: pattern.span,
                });
            } else if matches!(lit, Lit::Bool(false)) {
                tc.push_error(TypeError::UnreachableFalsePattern { span: pattern.span });
            }
        }
        Pattern::Nil => {
            if !expected.is_option() && !matches!(expected, Type::Infer) {
                tc.push_error(TypeError::TypeMismatch {
                    expected: expected.clone(),
                    found: Type::Infer,
                    span: pattern.span,
                });
            }
        }
        Pattern::Optional(inner) => {
            let inner_ty = expected.option_inner().unwrap_or(&Type::Infer);
            check_pattern(inner, inner_ty, mutable, tc);
        }
        Pattern::Range { .. } => {
            tc.push_error(TypeError::UnsupportedPattern {
                pattern: "range",
                span: pattern.span,
            });
        }
        Pattern::Or(_) => {
            tc.push_error(TypeError::OrPatternUnsupported { span: pattern.span });
        }
        Pattern::Rest => {
            tc.push_error(TypeError::UnsupportedPattern {
                pattern: "..",
                span: pattern.span,
            });
        }
        Pattern::Struct { name, fields } => {
            check_struct_pattern(*name, fields, pattern.span, expected, mutable, tc);
        }
        Pattern::EnumUnit { .. }
        | Pattern::EnumTuple { .. }
        | Pattern::EnumStruct { .. }
        | Pattern::InferredEnumUnit { .. }
        | Pattern::InferredEnumTuple { .. }
        | Pattern::InferredEnumStruct { .. } => {
            tc.push_error(TypeError::UnsupportedPattern {
                pattern: pattern.node.variant_name(),
                span: pattern.span,
            });
        }
    }
}

fn check_struct_pattern(
    name: Ident,
    fields: &[(Ident, PatternNode)],
    span: Span,
    expected: &Type,
    mutable: bool,
    tc: &mut TypeChecker,
) {
    let Some(key) = tc.resolve_visible_type_key(None, name) else {
        tc.push_error(TypeError::UnknownType {
            qualifier: None,
            name,
            span,
        });
        for (_, field) in fields {
            check_pattern(field, &Type::Infer, mutable, tc);
        }
        return;
    };

    let expected_key = tc.decls.key_for_type(expected);
    if expected_key.as_ref() != Some(&key) && !matches!(expected, Type::Infer) {
        tc.push_error(TypeError::TypeMismatch {
            expected: nominal_type(&key),
            found: expected.clone(),
            span,
        });
        return;
    }

    match key.kind {
        NominalKind::Struct | NominalKind::DataRef => {
            let Some(agg) = tc.decls.aggregate(&key).cloned() else {
                return;
            };
            let field_tys = agg
                .fields
                .iter()
                .map(|(name, field)| (*name, field.ty.clone()))
                .collect();
            check_struct_field_patterns(fields, nominal_type(&key), &field_tys, mutable, tc);
        }
        NominalKind::Extern => {
            let Some(owner) = tc.externs.type_by_nominal(&key) else {
                return;
            };
            let field_tys = tc
                .extern_type(owner)
                .fields
                .iter()
                .map(|field| (field.name, field.ty.ty.clone()))
                .collect();
            check_struct_field_patterns(fields, nominal_type(&key), &field_tys, mutable, tc);
        }
        NominalKind::Enum => {
            tc.push_error(TypeError::UnsupportedPattern {
                pattern: "Struct",
                span,
            });
        }
    }
}

fn check_struct_field_patterns(
    fields: &[(Ident, PatternNode)],
    owner_ty: Type,
    field_tys: &HashMap<Ident, Type>,
    mutable: bool,
    tc: &mut TypeChecker,
) {
    let mut seen = HashSet::new();
    for (name, pattern) in fields {
        if !seen.insert(*name) {
            tc.push_error(TypeError::DuplicateField {
                name: *name,
                span: pattern.span,
            });
            continue;
        }
        match field_tys.get(name) {
            Some(ty) => check_pattern(pattern, ty, mutable, tc),
            None => tc.push_error(TypeError::UnknownMember {
                ty: owner_ty.clone(),
                member: *name,
                kind: MemberAccessKind::Field,
                span: pattern.span,
            }),
        }
    }
}

fn record_extern_pattern_reads(
    pattern: &PatternNode,
    expected: &Type,
    site: ExprId,
    tc: &mut TypeChecker,
) {
    match &pattern.node {
        Pattern::Struct { fields, .. } => {
            let Some(owner) = tc.extern_type_id(expected) else {
                return;
            };
            for (name, subpattern) in fields {
                let Some((field, decl)) = tc
                    .extern_field(owner, *name)
                    .map(|(id, decl)| (id, decl.clone()))
                else {
                    continue;
                };
                tc.record_extern_use(site, ExternUseTarget::FieldRead(field));
                tc.reject_extern_any_escape_fact(decl.ty.contains_any(), subpattern.span);
                record_extern_pattern_reads(subpattern, &decl.ty.ty, site, tc);
            }
        }
        Pattern::Tuple(elems) => {
            if let Type::Tuple(tys) = expected {
                for (elem, ty) in elems.iter().zip(tys) {
                    record_extern_pattern_reads(elem, ty, site, tc);
                }
            }
        }
        Pattern::NamedTuple(fields) => {
            if let Type::NamedTuple(tys) = expected {
                for ((_, pattern), (_, ty)) in fields.iter().zip(tys) {
                    record_extern_pattern_reads(pattern, ty, site, tc);
                }
            }
        }
        Pattern::Optional(inner) => {
            let inner_ty = expected.option_inner().unwrap_or(&Type::Infer);
            record_extern_pattern_reads(inner, inner_ty, site, tc);
        }
        Pattern::Ident(_)
        | Pattern::Wildcard
        | Pattern::VarIdent(_)
        | Pattern::Lit(_)
        | Pattern::Nil
        | Pattern::Range { .. }
        | Pattern::Or(_)
        | Pattern::Rest
        | Pattern::EnumUnit { .. }
        | Pattern::EnumTuple { .. }
        | Pattern::EnumStruct { .. }
        | Pattern::InferredEnumUnit { .. }
        | Pattern::InferredEnumTuple { .. }
        | Pattern::InferredEnumStruct { .. } => {}
    }
}

fn check_while_let(while_let_node: &WhileLetNode, tc: &mut TypeChecker) {
    let node = &while_let_node.node;
    let value_ty = check_expr_checked(&node.value, tc).ty;
    record_extern_pattern_reads(&node.pattern, &value_ty, node.value.node.id, tc);
    tc.push_scope();
    check_pattern(&node.pattern, &value_ty, false, tc);
    tc.enter_loop();
    check_block_checked(&node.body, tc);
    tc.exit_loop();
    tc.pop_scope();
}

fn check_if_let_checked_with_hint(
    if_let_node: &IfLetNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &if_let_node.node;
    let value = check_expr_checked(&node.value, tc);
    record_extern_pattern_reads(&node.pattern, &value.ty, node.value.node.id, tc);
    tc.push_scope();
    check_pattern_from_handle(&node.pattern, value.handle, value.ty, false, tc);
    let then = check_block_checked_with_hint(&node.then_block, expected.clone(), tc);
    tc.pop_scope();
    let Some(else_block) = &node.else_block else {
        return checked_void(tc);
    };
    let else_checked = check_block_checked_with_hint(else_block, expected, tc);
    join_checked(
        then,
        node.then_block.span,
        else_checked,
        else_block.span,
        tc,
    )
}

fn check_match_checked_with_hint(
    match_node: &MatchNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &match_node.node;
    let scrutinee = check_expr_checked(&node.scrutinee, tc);
    if node.arms.is_empty() {
        tc.push_error(TypeError::EmptyMatch {
            span: match_node.span,
        });
        return checked_void(tc);
    }
    let mut arms = Vec::with_capacity(node.arms.len());
    for arm in &node.arms {
        tc.push_scope();
        record_extern_pattern_reads(&arm.node.pattern, &scrutinee.ty, node.scrutinee.node.id, tc);
        check_pattern_from_handle(
            &arm.node.pattern,
            scrutinee.handle.clone(),
            scrutinee.ty.clone(),
            false,
            tc,
        );
        let body = check_expr_checked_with_hint(&arm.node.body, expected.clone(), tc);
        tc.pop_scope();
        arms.push((arm.node.body.span, body));
    }
    if arms[0].1.ty.is_void() {
        return checked_void(tc);
    }
    let result = tc.fresh_temp_handle(arms[0].0);
    let contains_extern_any = arms
        .iter()
        .any(|(_, arm)| !arm.ty.is_void() && arm.contains_extern_any);
    for (span, arm) in arms {
        if !arm.ty.is_void() {
            tc.expect_assignable(span, arm.handle, result.clone());
        }
    }
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
        contains_extern_any,
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct CallTargetClosureFacts {
    pub(crate) contains_infer: bool,
    pub(crate) contains_unresolved_ref: bool,
    pub(crate) contains_unresolved_const: bool,
    pub(crate) contains_const_infer: bool,
}

pub(crate) fn call_target_closure_facts(target: &CallTarget) -> CallTargetClosureFacts {
    let mut facts = CallTargetClosureFacts::default();
    for ty in &target.args.type_args {
        let ty_facts = type_closure_facts(ty);
        facts.contains_infer |= ty_facts.contains_infer;
        facts.contains_unresolved_ref |= ty_facts.first_unresolved.is_some();
        facts.contains_unresolved_const |= ty_facts.contains_unresolved_const;
    }
    for arg in &target.args.const_args {
        match arg {
            ConstTerm::Name(_) => facts.contains_unresolved_const = true,
            ConstTerm::ArrayInfer | ConstTerm::Infer(_) => facts.contains_const_infer = true,
            ConstTerm::Value(_) | ConstTerm::Param(_) => {}
        }
    }
    facts
}

fn extern_site_span(site: crate::externs::RawExternSite) -> Span {
    site.span.unwrap_or_else(|| Span::new(0, 0))
}

fn push_extern_ty_closure_error(
    errors: &mut Vec<TypeError>,
    ty: &crate::externs::catalog::ResolvedExternTy,
    span: Span,
) {
    push_type_closure_error(errors, &ty.ty, span);
}

fn push_call_target_closure_error(errors: &mut Vec<TypeError>, target: &CallTarget, span: Span) {
    let facts = call_target_closure_facts(target);
    if !facts.contains_infer
        && !facts.contains_unresolved_ref
        && !facts.contains_unresolved_const
        && !facts.contains_const_infer
    {
        return;
    }
    for ty in &target.args.type_args {
        push_type_closure_error(errors, ty, span);
    }
    if facts.contains_unresolved_const || facts.contains_const_infer {
        errors.push(TypeError::CannotInferConst { span });
    }
}

fn push_type_closure_error(errors: &mut Vec<TypeError>, ty: &Type, span: Span) {
    let facts = type_closure_facts(ty);
    if let Some(unresolved) = facts.first_unresolved {
        errors.push(TypeError::UnknownType {
            qualifier: unresolved.qualifier,
            name: unresolved.name,
            span,
        });
    } else if facts.contains_infer {
        errors.push(TypeError::CannotInferType { span });
    } else if facts.contains_unresolved_const {
        errors.push(TypeError::CannotInferConst { span });
    }
}
