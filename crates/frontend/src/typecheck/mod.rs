use std::{collections::HashMap, rc::Rc};

pub(crate) use self::{call_map::*, decls::*, generic::*, result::*};
use self::{
    const_term::ConstTerm,
    infer::{LocalTypeId, Solver, SolverFinalizeError, SolverRelationError, TypeHandle},
    postfix::{check_postfix_chain, collect_postfix_chain},
    type_ops::{TypeFolder, TypeVisitor},
};
use crate::{
    ast::*,
    resolve::{ModuleKey, ResolveResult},
    span::Span,
};

mod call_map;
mod const_eval;
mod const_term;
mod decls;
mod generic;
mod infer;
mod postfix;
mod result;
mod type_ops;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ConstDiagnostic {
    Value(ConstValue),
    Name(Ident),
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeError {
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
    FieldAccessOnNonAggregate {
        ty: Type,
        field: Ident,
        span: Span,
    },
    UnknownField {
        ty: Type,
        field: Ident,
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
pub(crate) struct FuncTemplate {
    pub span: Span,
    pub params: Vec<Param>,
    pub body: BlockNode,
}

#[derive(Debug, Clone)]
struct MethodTemplate {
    span: Span,
    receiver: Option<MethodReceiver>,
    params: Vec<Param>,
    body: BlockNode,
}

struct TypeChecker {
    solver: Solver,
    calls: HashMap<ExprId, CallTarget>,
    decls: DeclarationIndex,
    scopes: Vec<HashMap<Ident, VarInfo>>,
    return_types: Vec<Type>,
    return_seen: Vec<bool>,
    loop_depth: usize,
    errors: Vec<TypeError>,
    current_module: ModuleScope,
    module_programs: HashMap<ModuleScope, Rc<Program>>,
    type_substs: Vec<TypeSubst>,
    const_substs: Vec<ConstSubst>,
    func_templates: HashMap<(ModuleScope, Ident), FuncTemplate>,
    method_templates: HashMap<MethodId, MethodTemplate>,
    extend_templates: HashMap<(ExtendId, Ident), MethodTemplate>,
    specializations: HashMap<SpecializationKey, SpecializationState>,
    consts: HashMap<(ModuleScope, Ident), const_eval::ConstEntry>,
}

struct TypeRefResolver<'tc> {
    tc: &'tc mut TypeChecker,
}

impl TypeFolder for TypeRefResolver<'_> {
    fn fold_unresolved_nominal(
        &mut self,
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: &[GenericArg],
    ) -> Type {
        let key = match qualifier {
            Some(module_name) => self
                .tc
                .lookup_module_alias(module_name)
                .and_then(|scope| self.tc.exported_type_in_module(&scope, name)),
            None => self.tc.lookup_type_name(name),
        };

        match key {
            Some(key) => self
                .tc
                .nominal_type_with_args(&key, generic_args, Span::new(0, 0)),
            None => self.fold_unresolved_nominal_default(qualifier, name, generic_args),
        }
    }
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
    fn new(decls: DeclarationIndex) -> Self {
        Self {
            solver: Solver::default(),
            calls: HashMap::new(),
            decls,
            scopes: vec![],
            return_types: vec![],
            return_seen: vec![],
            loop_depth: 0,
            errors: vec![],
            current_module: ModuleScope::Root,
            module_programs: HashMap::new(),
            type_substs: vec![],
            const_substs: vec![],
            func_templates: HashMap::new(),
            method_templates: HashMap::new(),
            extend_templates: HashMap::new(),
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

    #[cfg(test)]
    fn set_local_type(&mut self, id: LocalTypeId, ty: Type) {
        self.solver.set_local_type_from_type(id, &ty);
    }

    fn type_handle(&self, ty: &Type) -> TypeHandle {
        self.solver.concrete_type(ty)
    }

    fn expr_handle(&self, id: ExprId) -> TypeHandle {
        self.solver.expr_handle(id)
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

    fn store_func_template(&mut self, module: ModuleScope, name: Ident, template: FuncTemplate) {
        self.func_templates.insert((module, name), template);
    }

    fn func_template(&self, module: &ModuleScope, name: Ident) -> Option<&FuncTemplate> {
        self.func_templates.get(&(module.clone(), name))
    }

    fn store_method_template(&mut self, id: MethodId, template: MethodTemplate) {
        self.method_templates.insert(id, template);
    }

    fn method_template(&self, id: &MethodId) -> Option<&MethodTemplate> {
        self.method_templates.get(id)
    }

    fn store_extend_template(&mut self, id: ExtendId, name: Ident, template: MethodTemplate) {
        self.extend_templates.insert((id, name), template);
    }

    fn extend_template(&self, id: &ExtendId, name: Ident) -> Option<&MethodTemplate> {
        self.extend_templates.get(&(id.clone(), name))
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
        origin == &self.current_module || self.imports_module(origin)
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

    fn resolve_type_ref(&mut self, ty: &Type) -> Type {
        TypeRefResolver { tc: self }.fold_type(ty)
    }

    fn resolve_type_for_tc(&mut self, ty: &Type) -> Type {
        let resolved = self.resolve_type_ref(ty);
        let substituted = match self.type_substs.last().cloned() {
            Some(ts) => {
                let cs = self.const_substs.last().cloned().unwrap_or_default();
                self.substitute_checked(&resolved, &ts, &cs, Span::new(0, 0))
            }
            None => resolved,
        };
        self.normalize_type_consts(&substituted, Span::new(0, 0))
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

    fn normalize_const_term(&mut self, term: ConstTerm, span: Span) -> ConstTerm {
        match term {
            ConstTerm::Name(name) => match self.eval_visible_const(name, span) {
                Some(Ok(value)) => ConstTerm::Value(value),
                Some(Err(err)) => {
                    self.push_error(err);
                    ConstTerm::Name(name)
                }
                None => ConstTerm::Name(name),
            },
            ConstTerm::Value(_)
            | ConstTerm::Param(_)
            | ConstTerm::ArrayInfer
            | ConstTerm::Infer(_) => term,
        }
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
                    self.push_error(TypeError::UnknownConst { name, span });
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

    fn eval_generic_const_term(&mut self, arg: &GenericArg, span: Span) -> Option<ConstTerm> {
        let term = match arg {
            GenericArg::Const(arg) => ConstTerm::from_arg(arg),
            GenericArg::Type(ty) => match bare_type_name(ty) {
                Some(name) => ConstTerm::Name(name),
                None => {
                    self.push_error(TypeError::GenericArgKindMismatch {
                        expected: "const",
                        span,
                    });
                    return None;
                }
            },
        };
        self.eval_const_term(term, span)
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
        let term = self.normalize_const_term(ConstTerm::from_arg(arg), span);
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

    fn lookup_type_name(&self, name: Ident) -> Option<NominalKey> {
        self.decls.visible_type(&self.current_module, name)
    }

    fn func_type_from_sig(&mut self, params: &[Param], ret: &Type) -> Type {
        let resolved_params: Vec<FuncParam> = params
            .iter()
            .map(|p| {
                FuncParam::new(
                    self.resolve_type_for_tc(&p.ty),
                    matches!(p.mutability, Mutability::Mutable),
                )
            })
            .collect();
        let resolved_ret = Box::new(self.resolve_type_for_tc(ret));
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
            self.push_result_infer_leaks(&types);
        }
        if self.errors.is_empty() {
            Ok(TypecheckResult {
                types,
                calls: self.calls,
                decls: self.decls,
                consts: const_eval::evaluated_consts(self.consts),
            })
        } else {
            Err(self.errors)
        }
    }

    fn push_result_infer_leaks(&mut self, types: &HashMap<ExprId, (Span, Type)>) {
        for (span, ty) in types.values() {
            if type_contains_infer(ty) {
                self.push_error_once(TypeError::CannotInferType { span: *span });
            }
        }
    }

    fn nominal_generics(&self, key: &NominalKey) -> Option<GenericParams> {
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => self
                .decls
                .aggregate(key)
                .map(|schema| schema.generics.clone()),
            NominalKind::Enum => self
                .decls
                .enum_schema(key)
                .map(|schema| schema.generics.clone()),
            NominalKind::Extern => Some(GenericParams::default()),
        }
    }

    fn nominal_type_with_args(
        &mut self,
        key: &NominalKey,
        generic_args: &[GenericArg],
        span: Span,
    ) -> Type {
        let Some(generics) = self.nominal_generics(key) else {
            return nominal_type(key);
        };
        let Some((type_args, const_args)) = self.bind_nominal_args(&generics, generic_args, span)
        else {
            return Type::Infer;
        };
        self.validate_nominal_args(key, &generics, &type_args, &const_args, span);
        nominal_type_with_args(key, &type_args, &const_args)
    }

    fn validate_nominal_args(
        &mut self,
        key: &NominalKey,
        generics: &GenericParams,
        type_args: &[Type],
        const_args: &[ConstArg],
        span: Span,
    ) {
        let args = GenericArgs {
            type_args: type_args.to_vec(),
            const_args: ConstTerm::from_args(const_args),
        };
        let (type_subst, const_subst) = generics.substitutions(&args);
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                if let Some(schema) = self.decls.aggregate(key).cloned() {
                    for field in schema.fields.values() {
                        self.substitute_checked(&field.ty, &type_subst, &const_subst, span);
                    }
                }
            }
            NominalKind::Enum => {
                if let Some(variants) = self
                    .decls
                    .enum_schema(key)
                    .map(|schema| schema.variants.clone())
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

    fn bind_nominal_args(
        &mut self,
        generics: &GenericParams,
        args: &[GenericArg],
        span: Span,
    ) -> Option<(Vec<Type>, Vec<ConstArg>)> {
        let type_len = generics.type_params.len();
        let expected = type_len + generics.const_params.len();
        if args.len() != expected {
            self.push_error(TypeError::GenericArity(ArityError::TypeArgs {
                expected,
                found: args.len(),
            }));
            return None;
        }

        let mut type_args = Vec::with_capacity(generics.type_params.len());
        let mut const_args = Vec::with_capacity(generics.const_params.len());
        for (index, arg) in args.iter().enumerate() {
            if index < type_len {
                let GenericArg::Type(ty) = arg else {
                    self.push_error(TypeError::GenericArgKindMismatch {
                        expected: "type",
                        span,
                    });
                    return None;
                };
                type_args.push(self.resolve_type_for_tc(ty));
            } else {
                const_args.push(self.bind_nominal_const_arg(arg, span)?);
            }
        }
        Some((type_args, const_args))
    }

    fn bind_nominal_const_arg(&mut self, arg: &GenericArg, span: Span) -> Option<ConstArg> {
        self.eval_generic_const_term(arg, span)?.to_arg_no_infer()
    }
}

pub(crate) fn check_with_modules(
    program: &Program,
    resolved: &ResolveResult,
) -> Result<TypecheckResult, Vec<TypeError>> {
    let decls = DeclarationIndex::from_root_and_modules(program, resolved);
    let mut tc = TypeChecker::new(decls);
    tc.collect_const_decls(ModuleScope::Root, program);
    collect_func_templates(ModuleScope::Root, program, &mut tc);
    collect_method_templates(ModuleScope::Root, program, &mut tc);
    collect_extend_templates(ModuleScope::Root, program, &mut tc);

    for group in &resolved.module_groups {
        for module in group {
            let ModuleKey::Named(path) = &module.key else {
                continue;
            };
            let scope = ModuleScope::Named(path.clone());
            tc.module_programs
                .insert(scope.clone(), Rc::new(module.program.clone()));
            tc.collect_const_decls(scope.clone(), &module.program);
            collect_func_templates(scope.clone(), &module.program, &mut tc);
            collect_method_templates(scope.clone(), &module.program, &mut tc);
            collect_extend_templates(scope, &module.program, &mut tc);
        }
    }

    tc.push_scope();
    register_declarations(program, &mut tc);
    tc.eval_module_consts(&ModuleScope::Root);
    check_stmts(&program.stmts, &mut tc);
    tc.pop_scope();
    tc.into_result()
}

pub(crate) fn check(program: &Program) -> Result<TypecheckResult, Vec<TypeError>> {
    let decls = DeclarationIndex::from_root(program);
    let mut tc = TypeChecker::new(decls);
    tc.collect_const_decls(ModuleScope::Root, program);
    collect_func_templates(ModuleScope::Root, program, &mut tc);
    collect_method_templates(ModuleScope::Root, program, &mut tc);
    collect_extend_templates(ModuleScope::Root, program, &mut tc);
    tc.push_scope();
    register_declarations(program, &mut tc);
    tc.eval_module_consts(&ModuleScope::Root);
    check_stmts(&program.stmts, &mut tc);
    tc.pop_scope();
    tc.into_result()
}

fn is_generic(func: &Func) -> bool {
    !func.type_params.is_empty() || !func.const_params.is_empty()
}

fn collect_func_templates(module: ModuleScope, program: &Program, tc: &mut TypeChecker) {
    for stmt in &program.stmts {
        if let Stmt::Func(func_node) = &stmt.node {
            let func = &func_node.node;
            if is_generic(func) {
                tc.store_func_template(
                    module.clone(),
                    func.name,
                    FuncTemplate {
                        span: func_node.span,
                        params: func.params.clone(),
                        body: func.body.clone(),
                    },
                );
            }
        }
    }
}

fn collect_method_templates(module: ModuleScope, program: &Program, tc: &mut TypeChecker) {
    for stmt in &program.stmts {
        let Stmt::Aggregate(agg_node) = &stmt.node else {
            continue;
        };
        let agg = &agg_node.node;
        let kind = agg.kind.into();
        let owner = NominalKey {
            module: module.clone(),
            kind,
            name: agg.name,
        };
        let aggregate_is_generic = !agg.type_params.is_empty() || !agg.const_params.is_empty();
        for method in &agg.methods {
            let method_is_generic =
                !method.type_params.is_empty() || !method.const_params.is_empty();
            let needs_template = aggregate_is_generic || method_is_generic;
            if !needs_template {
                continue;
            }
            tc.store_method_template(
                MethodId {
                    owner: owner.clone(),
                    name: method.name,
                },
                MethodTemplate {
                    span: agg_node.span,
                    receiver: method.receiver,
                    params: method.params.clone(),
                    body: method.body.clone(),
                },
            );
        }
    }
}

fn collect_extend_templates(module: ModuleScope, program: &Program, tc: &mut TypeChecker) {
    let mut extend_index = 0;

    for stmt in &program.stmts {
        let Stmt::Extend(extend_node) = &stmt.node else {
            continue;
        };
        let extend = &extend_node.node;
        let extend_id = ExtendId {
            module: module.clone(),
            index: extend_index,
        };
        extend_index += 1;
        let extend_is_generic = !extend.type_params.is_empty() || !extend.const_params.is_empty();

        for method_node in &extend.methods {
            if !extend_is_generic {
                continue;
            }
            let method = &method_node.node;
            let Some((self_param, params)) = method.params.split_first() else {
                continue;
            };
            let receiver = match self_param.mutability {
                Mutability::Mutable => MethodReceiver::Var,
                Mutability::Immutable => MethodReceiver::Value,
            };
            tc.store_extend_template(
                extend_id.clone(),
                method.name,
                MethodTemplate {
                    span: extend_node.span,
                    receiver: Some(receiver),
                    params: params.to_vec(),
                    body: method.body.clone(),
                },
            );
        }
    }
}

fn register_declarations(program: &Program, tc: &mut TypeChecker) {
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
            Stmt::ExternFunc(ext_node) => {
                let ext = &ext_node.node;
                let func_ty = tc.func_type_from_sig(&ext.params, &ext.ret);
                tc.define(ext.name, func_ty, false);
            }
            Stmt::ExternType(_) => {}
            Stmt::Const(const_node) => {
                let c = &const_node.node;
                let ty = match &c.ty {
                    Some(t) => tc.resolve_type_for_tc(t),
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
            check_expr(expr_node, tc);
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
        Stmt::Const(const_node) => {
            if tc.scopes.len() > 1 {
                check_const(const_node, tc);
            }
        }
        Stmt::Import(_)
        | Stmt::Aggregate(_)
        | Stmt::Enum(_)
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

    let self_ty = tc.resolve_type_for_tc(&extend.ty);
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
                    tc.resolve_type_for_tc(&param.ty),
                    matches!(param.mutability, Mutability::Mutable),
                )
            })
            .collect();
        let ret_ty = tc.resolve_type_for_tc(&method.ret);
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
            let ret_handle = tc.type_handle(&ret_ty);
            tc.expect_assignable(span, body_checked.handle, ret_handle);
        }
    } else if !body_is_void {
        if let Some(id) = last_expr_id(body) {
            let span = tc
                .get_type(id)
                .map_or_else(|| Span::new(0, 0), |(span, _)| span);
            tc.push_error(TypeError::UnusedValue { span });
        }
    }
    tc.pop_return_type();
    tc.pop_scope();
}

fn check_specialized_func_body(
    module: &ModuleScope,
    name: Ident,
    sig: &FuncSig,
    args: &GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    tc: &mut TypeChecker,
) {
    let key = function_specialization_key(module, name, args);
    if specialization_is_cached(&key, tc) {
        return;
    }

    let Some(template) = tc.func_template(module, name).cloned() else {
        return;
    };
    let Type::Func {
        params: template_params,
        ret: template_ret,
    } = &sig.ty
    else {
        return;
    };
    let param_types: Vec<FuncParam> = template_params
        .iter()
        .map(|param| {
            FuncParam::new(
                tc.substitute_checked(&param.ty, &type_subst, &const_subst, template.span),
                param.mutable,
            )
        })
        .collect();
    let ret_ty = tc.substitute_checked(template_ret, &type_subst, &const_subst, template.span);

    let const_bindings = const_param_bindings(&sig.generics, args);
    check_with_specialization(key, type_subst, const_subst, tc, |tc| {
        with_source_module_scope(module, tc, |tc| {
            check_func_body(
                None,
                &template.params,
                &param_types,
                ret_ty.clone(),
                &template.body,
                template.span,
                &const_bindings,
                tc,
            );
        });
    });
}

fn check_specialized_method_body(
    owner: &NominalKey,
    name: Ident,
    self_ty: Option<Type>,
    param_types: &[FuncParam],
    ret_ty: Type,
    args: &GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    const_bindings: Vec<(Ident, ConstValue)>,
    tc: &mut TypeChecker,
) {
    let key = method_specialization_key(owner, name, self_ty.is_some(), args);
    if specialization_is_cached(&key, tc) {
        return;
    }

    let id = MethodId {
        owner: owner.clone(),
        name,
    };
    let Some(template) = tc.method_template(&id).cloned() else {
        return;
    };

    check_with_specialization(key, type_subst, const_subst, tc, |tc| {
        with_source_module_scope(&owner.module, tc, |tc| {
            check_func_body(
                template.receiver.zip(self_ty),
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

fn check_specialized_extend_body(
    extend: &ExtendId,
    name: Ident,
    receiver_ty: Type,
    param_types: &[FuncParam],
    ret_ty: Type,
    args: &GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    const_bindings: Vec<(Ident, ConstValue)>,
    tc: &mut TypeChecker,
) {
    let key = extend_specialization_key(extend, name, args);
    if specialization_is_cached(&key, tc) {
        return;
    }

    let Some(template) = tc.extend_template(extend, name).cloned() else {
        return;
    };

    check_with_specialization(key, type_subst, const_subst, tc, |tc| {
        with_source_module_scope(&extend.module, tc, |tc| {
            check_func_body(
                template
                    .receiver
                    .map(|receiver| (receiver, receiver_ty.clone())),
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
    tc: &mut TypeChecker,
    check_body: impl FnOnce(&mut TypeChecker),
) {
    let old_types = tc.expr_types();
    tc.store_specialization(key.clone(), SpecializationState::InProgress);
    tc.push_type_subst(type_subst);
    tc.push_const_subst(const_subst);
    check_body(tc);
    tc.solve_constraints();
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

fn combined_const_param_bindings(
    owner_params: &GenericParams,
    owner_args: &GenericArgs,
    method_params: &GenericParams,
    method_args: &GenericArgs,
) -> Vec<(Ident, ConstValue)> {
    let mut bindings = const_param_bindings(owner_params, owner_args);
    bindings.extend(const_param_bindings(method_params, method_args));
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

fn function_specialization_key(
    module: &ModuleScope,
    name: Ident,
    args: &GenericArgs,
) -> SpecializationKey {
    SpecializationKey {
        target: CallableId {
            module: module.clone(),
            parent: None,
            kind: CallableKind::Function,
            name,
        },
        args: args.clone(),
    }
}

fn method_specialization_key(
    owner: &NominalKey,
    name: Ident,
    is_instance: bool,
    args: &GenericArgs,
) -> SpecializationKey {
    SpecializationKey {
        target: CallableId {
            module: owner.module.clone(),
            parent: Some(CallableParent::Nominal(owner.clone())),
            kind: if is_instance {
                CallableKind::InstanceMethod
            } else {
                CallableKind::StaticMethod
            },
            name,
        },
        args: args.clone(),
    }
}

fn extend_callable_id(extend: &ExtendId, name: Ident) -> CallableId {
    CallableId {
        module: extend.module.clone(),
        parent: Some(CallableParent::Extend(extend.clone())),
        kind: CallableKind::ExtendMethod,
        name,
    }
}

fn extend_specialization_key(
    extend: &ExtendId,
    name: Ident,
    args: &GenericArgs,
) -> SpecializationKey {
    SpecializationKey {
        target: extend_callable_id(extend, name),
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
            tc.scopes = vec![HashMap::new()];
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
}

fn checked_type(ty: Type, tc: &TypeChecker) -> CheckedType {
    CheckedType {
        handle: tc.type_handle(&ty),
        ty,
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
    tc.expect_assignable(left_span, left.handle, result.clone());
    tc.expect_assignable(right_span, right.handle, result.clone());
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
    }
}

fn check_block(block: &BlockNode, tc: &mut TypeChecker) -> Type {
    check_block_checked(block, tc).ty
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
            let annot_ty = tc.resolve_type_for_tc(annot);
            let annot_handle = tc.type_handle(&annot_ty);
            let value =
                check_expr_checked_with_hint(&binding.value, Some(annot_handle.clone()), tc);
            tc.expect_assignable(binding_node.span, value.handle, annot_handle);
            check_pattern(&binding.pattern, &annot_ty, mutable, tc);
        }
        None => {
            let value = check_expr_checked(&binding.value, tc);
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
            let annot_ty = tc.resolve_type_for_tc(annot);
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
            tc.expect_assignable(ret_node.span, actual.handle, expected);
        } else {
            check_expr(expr, tc);
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

fn check_expr(expr: &ExprNode, tc: &mut TypeChecker) -> Type {
    check_expr_checked(expr, tc).ty
}

fn check_expr_checked(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedType {
    check_expr_checked_with_hint(expr, None, tc)
}

fn checked_from_type(expr: &ExprNode, ty: Type, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_type(expr.node.id, ty.clone(), expr.span);
    CheckedType { ty, handle }
}

fn checked_from_handle(expr: &ExprNode, handle: TypeHandle, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_type_from_handle(expr.node.id, expr.span, handle);
    let ty = tc.handle_type(&handle);
    CheckedType { ty, handle }
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
        ExprKind::Binary(bin_node) => checked_from_type(expr, check_binary(bin_node, tc), tc),
        ExprKind::Unary(unary_node) => checked_from_type(expr, check_unary(unary_node, tc), tc),
        ExprKind::Block(block_node) => checked_from_handle(
            expr,
            check_block_checked_with_hint(block_node, expected, tc).handle,
            tc,
        ),
        ExprKind::If(if_node) => checked_from_handle(
            expr,
            check_if_checked_with_hint(if_node, expected, tc).handle,
            tc,
        ),
        ExprKind::Assign(assign_node) => {
            check_assign(assign_node, tc);
            checked_from_type(expr, Type::Void, tc)
        }
        ExprKind::StructLiteral(lit) => {
            check_struct_literal_checked_with_hint(expr, lit, expected, tc)
        }
        ExprKind::InferredEnum(node) => {
            check_inferred_enum_checked_with_hint(expr, node, expected, tc)
        }
        ExprKind::Field(_) | ExprKind::Call(_) => {
            let chain = collect_postfix_chain(expr).expect("postfix chain");
            let ty = check_postfix_chain(&chain, expr, expected.as_ref(), tc);
            CheckedType {
                ty,
                handle: tc.expr_handle(expr.node.id),
            }
        }
        ExprKind::Tuple(elems) => check_tuple_checked_with_hint(expr, elems, expected, tc),
        ExprKind::NamedTuple(fields) => {
            check_named_tuple_checked_with_hint(expr, fields, expected, tc)
        }
        ExprKind::ArrayLiteral(lit) => {
            check_array_literal_checked_with_hint(expr, lit, expected, tc)
        }
        ExprKind::ArrayFill(fill) => check_array_fill_checked_with_hint(expr, fill, expected, tc),
        ExprKind::IfLet(if_let_node) => checked_from_handle(
            expr,
            check_if_let_checked_with_hint(if_let_node, expected, tc).handle,
            tc,
        ),
        ExprKind::Match(match_node) => checked_from_handle(
            expr,
            check_match_checked_with_hint(match_node, expected, tc).handle,
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

fn check_binary(bin: &BinaryNode, tc: &mut TypeChecker) -> Type {
    let left_ty = check_expr(&bin.node.left, tc);
    let right_ty = check_expr(&bin.node.right, tc);
    let same = left_ty == right_ty;
    match bin.node.op {
        BinaryOp::Add => {
            let l_str = left_ty.is_str();
            let r_str = right_ty.is_str();
            let l_stringable = left_ty.is_stringable();
            let r_stringable = right_ty.is_stringable();
            let is_string_concat =
                (l_str && r_str) || (l_str && r_stringable) || (r_str && l_stringable);
            if is_string_concat {
                return Type::String;
            }
            if left_ty.is_num() && same {
                return left_ty;
            }
            tc.push_error(TypeError::InvalidOperand {
                op: "+".to_string(),
                operand_type: right_ty,
                span: bin.span,
            });
            Type::Infer
        }
        BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
            if left_ty.is_num() && same {
                return left_ty;
            }
            tc.push_error(TypeError::InvalidOperand {
                op: format!("{}", bin.node.op),
                operand_type: right_ty,
                span: bin.span,
            });
            Type::Infer
        }
        BinaryOp::Eq | BinaryOp::NotEq => {
            if same {
                Type::Bool
            } else {
                tc.push_error(TypeError::TypeMismatch {
                    expected: left_ty,
                    found: right_ty,
                    span: bin.span,
                });
                Type::Bool
            }
        }
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEq
        | BinaryOp::GreaterThanEq => {
            if left_ty.is_num() && same {
                Type::Bool
            } else {
                tc.push_error(TypeError::TypeMismatch {
                    expected: left_ty,
                    found: right_ty,
                    span: bin.span,
                });
                Type::Bool
            }
        }
        BinaryOp::And | BinaryOp::Or => {
            if left_ty.is_bool() && same {
                Type::Bool
            } else {
                tc.push_error(TypeError::TypeMismatch {
                    expected: Type::Bool,
                    found: left_ty,
                    span: bin.span,
                });
                Type::Bool
            }
        }
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor | BinaryOp::Shl | BinaryOp::Shr => {
            if left_ty.is_int() && same {
                Type::Int
            } else {
                tc.push_error(TypeError::TypeMismatch {
                    expected: Type::Int,
                    found: left_ty,
                    span: bin.span,
                });
                Type::Int
            }
        }
        BinaryOp::Coalesce => {
            tc.push_error(TypeError::InvalidOperand {
                op: "??".to_string(),
                operand_type: left_ty,
                span: bin.span,
            });
            Type::Infer
        }
    }
}

fn check_unary(unary: &UnaryNode, tc: &mut TypeChecker) -> Type {
    let operand_ty = check_expr(&unary.node.expr, tc);
    match unary.node.op {
        UnaryOp::Neg => {
            if operand_ty.is_num() {
                operand_ty
            } else {
                tc.push_error(TypeError::InvalidOperand {
                    op: "-".to_string(),
                    operand_type: operand_ty,
                    span: unary.span,
                });
                Type::Infer
            }
        }
        UnaryOp::Not => {
            if operand_ty.is_bool() {
                Type::Bool
            } else {
                tc.push_error(TypeError::InvalidOperand {
                    op: "not".to_string(),
                    operand_type: operand_ty,
                    span: unary.span,
                });
                Type::Infer
            }
        }
        UnaryOp::BitNot => {
            if operand_ty.is_int() {
                Type::Int
            } else {
                tc.push_error(TypeError::InvalidOperand {
                    op: "~".to_string(),
                    operand_type: operand_ty,
                    span: unary.span,
                });
                Type::Infer
            }
        }
    }
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
        check_block(&if_node.node.then_block, tc);
        return checked_void(tc);
    };
    let then = check_block_checked_with_hint(&if_node.node.then_block, expected.clone(), tc);
    let else_checked = check_block_checked_with_hint(else_block, expected, tc);
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

fn check_array_literal_checked_with_hint(
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
    for value in &lit.node.elements {
        check_expected(value, elem.clone(), tc);
    }
    solve_and_checked_from_handle(expr, array, tc)
}

fn check_array_fill_checked_with_hint(
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
    check_expected(&fill.node.value, elem.clone(), tc);
    let array = collection_literal_handle(kind, elem, len, tc);
    solve_and_checked_from_handle(expr, array, tc)
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
    for (elem, hint) in elems.iter().zip(&hints) {
        check_expected(elem, hint.clone(), tc);
    }
    let tuple = tc.tuple_handle(hints);
    solve_and_checked_from_handle(expr, tuple, tc)
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
    for ((_, value), hint) in fields.iter().zip(&hints) {
        check_expected(value, hint.clone(), tc);
    }
    let fields = fields
        .iter()
        .zip(hints)
        .map(|((name, _), handle)| (*name, handle))
        .collect();
    let tuple = tc.named_tuple_handle(fields);
    solve_and_checked_from_handle(expr, tuple, tc)
}

struct ProvidedNominalField {
    span: Span,
    handle: TypeHandle,
    template_ty: Type,
}

struct FinalNominalLiteral {
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    ty: Type,
}

struct NominalLiteralInference {
    inf: Inference,
}

impl NominalLiteralInference {
    fn new() -> Self {
        Self {
            inf: Inference::new(),
        }
    }

    fn bind_explicit_args(
        &mut self,
        generics: &GenericParams,
        args: &[GenericArg],
        span: Span,
        tc: &mut TypeChecker,
    ) -> bool {
        if args.is_empty() {
            return true;
        }
        let Some((type_args, const_args)) = tc.bind_nominal_args(generics, args, span) else {
            return false;
        };
        for (param, ty) in generics.type_params.iter().zip(type_args) {
            if !self.inf.bind_type(param.id, ty) {
                return false;
            }
        }
        for (param, arg) in generics.const_params.iter().zip(const_args) {
            if !self.inf.bind_const(param.id, ConstTerm::from_arg(&arg)) {
                return false;
            }
        }
        true
    }

    fn bind_expected(
        &mut self,
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
        if infer(&template, expected, &mut self.inf) {
            return true;
        }
        tc.push_error(TypeError::TypeMismatch {
            expected: expected.clone(),
            found: self.current_nominal_type(key, generics),
            span,
        });
        false
    }

    fn current_hint(&self, ty: &Type, span: Span, tc: &mut TypeChecker) -> Type {
        tc.substitute_checked(ty, self.inf.type_subst(), self.inf.const_subst(), span)
    }

    fn infer_field(&mut self, template: &Type, checked: &CheckedType) {
        if checked.ty != Type::Infer {
            infer(template, &checked.ty, &mut self.inf);
        }
    }

    fn finalize(
        self,
        key: &NominalKey,
        generics: &GenericParams,
        span: Span,
        tc: &mut TypeChecker,
    ) -> Option<FinalNominalLiteral> {
        if generics.is_empty() {
            return Some(FinalNominalLiteral {
                type_subst: TypeSubst::new(),
                const_subst: ConstSubst::new(),
                ty: nominal_type(key),
            });
        }
        let args = match self.inf.into_args(generics) {
            Ok(args) => args,
            Err(unbound) => {
                tc.push_unbound_generic_errors(unbound, span);
                return None;
            }
        };
        let (type_subst, const_subst) = generics.substitutions(&args);
        let ty = nominal_literal_type(key, generics, Some(&args));
        Some(FinalNominalLiteral {
            type_subst,
            const_subst,
            ty,
        })
    }

    fn current_nominal_type(&self, key: &NominalKey, generics: &GenericParams) -> Type {
        let type_args = generics
            .type_params
            .iter()
            .map(|param| {
                self.inf
                    .type_subst()
                    .get(&param.id)
                    .cloned()
                    .unwrap_or(Type::Var(param.id))
            })
            .collect::<Vec<_>>();
        let const_args = generics
            .const_params
            .iter()
            .map(|param| {
                self.inf
                    .const_subst()
                    .get(&param.id)
                    .and_then(ConstTerm::to_arg_no_infer)
                    .unwrap_or(ConstArg::Param(param.id))
            })
            .collect::<Vec<_>>();
        nominal_type_with_args(key, &type_args, &const_args)
    }
}

fn check_struct_literal_checked_with_hint(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(key) = resolve_struct_key(lit, tc) else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

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
    let mut inf = NominalLiteralInference::new();
    if !inf.bind_explicit_args(&agg.generics, &lit.node.generic_args, lit.span, tc) {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }
    let expected_ok = inf.bind_expected(&key, &agg.generics, expected_ty.as_ref(), lit.span, tc);

    let provided = check_nominal_fields(
        &lit.node.fields,
        &agg.fields,
        nominal_type(&key),
        lit.span,
        &agg.generics,
        &mut inf,
        tc,
    );
    if !expected_ok {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let Some(result) = inf.finalize(&key, &agg.generics, lit.span, tc) else {
        return checked_from_type(expr, Type::Infer, tc);
    };
    constrain_provided_fields(provided, &result.type_subst, &result.const_subst, tc);
    let handle = tc.type_handle(&result.ty);
    solve_and_checked_from_handle(expr, handle, tc)
}

fn check_inferred_enum_checked_with_hint(
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
        check_inferred_enum_args_without_hint(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    let mut inf = NominalLiteralInference::new();
    if !inf.bind_expected(&key, &generics, Some(&expected_ty), node.span, tc) {
        check_inferred_enum_args_without_hint(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

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
            let Some(result) = inf.finalize(&key, &generics, node.span, tc) else {
                check_exprs_without_hint(args, tc);
                return checked_from_type(expr, Type::Infer, tc);
            };
            for (arg, param) in args.iter().zip(params) {
                let expected_ty =
                    tc.substitute_checked(param, &result.type_subst, &result.const_subst, arg.span);
                let hint = tc.type_handle(&expected_ty);
                check_expected(arg, hint, tc);
            }
        }
        (VariantSchema::Tuple(params), args) => {
            return wrong_inferred_enum_args(expr, node, params.len(), args, tc);
        }
        (VariantSchema::Struct(fields), InferredEnumArgs::Struct(args)) => {
            let provided = check_nominal_fields(
                args,
                fields,
                expected_ty.clone(),
                node.span,
                &generics,
                &mut inf,
                tc,
            );
            let Some(result) = inf.finalize(&key, &generics, node.span, tc) else {
                return checked_from_type(expr, Type::Infer, tc);
            };
            constrain_provided_fields(provided, &result.type_subst, &result.const_subst, tc);
        }
        (VariantSchema::Struct(fields), args) => {
            return wrong_inferred_enum_args(expr, node, fields.len(), args, tc);
        }
    }

    solve_and_checked_from_handle(expr, expected, tc)
}

fn cannot_infer_inferred_enum(
    expr: &ExprNode,
    node: &InferredEnumNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_error(TypeError::CannotInferType { span: node.span });
    check_inferred_enum_args_without_hint(&node.node.args, tc);
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
    check_inferred_enum_args_without_hint(args, tc);
    checked_from_type(expr, Type::Infer, tc)
}

fn check_nominal_fields(
    fields: &[(Ident, ExprNode)],
    schema: &HashMap<Ident, FieldSchema>,
    owner_ty: Type,
    span: Span,
    generics: &GenericParams,
    inf: &mut NominalLiteralInference,
    tc: &mut TypeChecker,
) -> Vec<ProvidedNominalField> {
    let mut seen = HashMap::new();
    let mut provided = vec![];
    for (name, value) in fields {
        if seen.insert(*name, value.span).is_some() {
            tc.push_error(TypeError::DuplicateField {
                name: *name,
                span: value.span,
            });
        }
        match schema.get(name) {
            Some(field) => {
                let field_hint = inf.current_hint(&field.ty, value.span, tc);
                let checked = if generics.contains_param(&field_hint) {
                    check_expr_checked(value, tc)
                } else {
                    let hint = tc.type_handle(&field_hint);
                    check_expr_checked_with_hint(value, Some(hint), tc)
                };
                inf.infer_field(&field.ty, &checked);
                provided.push(ProvidedNominalField {
                    span: value.span,
                    handle: checked.handle,
                    template_ty: field.ty.clone(),
                });
            }
            None => {
                tc.push_error(TypeError::UnknownField {
                    ty: owner_ty.clone(),
                    field: *name,
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
    provided
}

fn constrain_provided_fields(
    fields: Vec<ProvidedNominalField>,
    type_subst: &TypeSubst,
    const_subst: &ConstSubst,
    tc: &mut TypeChecker,
) {
    for field in fields {
        let expected_ty =
            tc.substitute_checked(&field.template_ty, type_subst, const_subst, field.span);
        let expected = tc.type_handle(&expected_ty);
        tc.expect_assignable(field.span, field.handle, expected);
    }
}

fn check_unknown_nominal_fields(fields: &[(Ident, ExprNode)], tc: &mut TypeChecker) {
    for (_, value) in fields {
        check_expr_checked(value, tc);
    }
}

fn check_inferred_enum_args_without_hint(args: &InferredEnumArgs, tc: &mut TypeChecker) {
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
        let const_args = args
            .const_args
            .iter()
            .filter_map(ConstTerm::to_arg_no_infer)
            .collect::<Vec<_>>();
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

fn resolve_struct_key(lit: &StructLiteralNode, tc: &TypeChecker) -> Option<NominalKey> {
    match lit.node.qualifier {
        Some(qualifier) => {
            let scope = tc.lookup_module_alias(qualifier)?;
            tc.exported_type_in_module(&scope, lit.node.name)
        }
        None => tc.lookup_type_name(lit.node.name),
    }
}

fn check_assign(assign: &AssignNode, tc: &mut TypeChecker) {
    let target = check_expr_checked(&assign.node.target, tc);
    let value = check_expr_checked_with_hint(&assign.node.value, Some(target.handle.clone()), tc);
    if let ExprKind::Ident(name) = &assign.node.target.node.kind {
        if let Some(info) = tc.lookup(*name) {
            if !info.mutable {
                tc.push_error(TypeError::ImmutableAssignment {
                    name: *name,
                    span: assign.node.target.span,
                });
            }
        }
    }
    if !target.ty.is_void() && !value.ty.is_void() {
        tc.expect_assignable(assign.node.value.span, value.handle, target.handle);
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
    check_block(&while_node.node.body, tc);
    tc.exit_loop();
}

fn check_for(for_node: &ForNode, tc: &mut TypeChecker) {
    let node = &for_node.node;
    let iterable_ty = check_expr(&node.iterable, tc);

    if let Some(step) = &node.step {
        check_expr(step, tc);
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
    check_block(&node.body, tc);
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
        Pattern::Struct { .. }
        | Pattern::EnumUnit { .. }
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

fn check_while_let(while_let_node: &WhileLetNode, tc: &mut TypeChecker) {
    let node = &while_let_node.node;
    let value_ty = check_expr(&node.value, tc);
    tc.push_scope();
    check_pattern(&node.pattern, &value_ty, false, tc);
    tc.enter_loop();
    check_block(&node.body, tc);
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
    for (span, arm) in arms {
        if !arm.ty.is_void() {
            tc.expect_assignable(span, arm.handle, result.clone());
        }
    }
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
    }
}

pub(super) fn bare_type_name(ty: &Type) -> Option<Ident> {
    match ty {
        Type::UnresolvedName(name) => Some(*name),
        Type::UnresolvedNominal {
            qualifier: None,
            name,
            generic_args,
        } if generic_args.is_empty() => Some(*name),
        _ => None,
    }
}

struct ContainsInfer;

impl TypeVisitor for ContainsInfer {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        matches!(ty, Type::Infer)
    }

    fn visit_array_len(&mut self, len: ArrayLen) -> bool {
        matches!(len, ArrayLen::Infer)
    }
}

pub(crate) fn type_contains_infer(ty: &Type) -> bool {
    ContainsInfer.visit_type(ty)
}

pub(crate) fn call_target_contains_infer(target: &CallTarget) -> bool {
    match target {
        CallTarget::GenericDirect { type_args, .. } | CallTarget::Method { type_args, .. } => {
            type_args.iter().any(type_contains_infer)
        }
        CallTarget::Extend { receiver, args, .. } => {
            let receiver_contains_infer = type_contains_infer(receiver);
            let args_contain_infer = args.type_args.iter().any(type_contains_infer);
            receiver_contains_infer || args_contain_infer
        }
        CallTarget::Direct { .. }
        | CallTarget::ModuleFunction { .. }
        | CallTarget::EnumVariant { .. } => false,
    }
}

fn last_expr_id(block: &BlockNode) -> Option<ExprId> {
    block.node.tail.as_ref().map(|e| e.node.id)
}
