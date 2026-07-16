use std::collections::HashSet;

use anvyx_semantics::{display_float, float_to_int, int_to_float};

use super::{
    CallableTemplateEnv, CheckedType, DeprecatedUseKind, LocalConstId, LocalConstInfo, LocalSymbol,
    ModuleScope, RawEnumValue, TypeChecker, TypeError, ValueDecl, VarInfo,
    body::with_callable_body_env, checked_from_type, const_term::ConstTerm,
};
use crate::{
    ast::{
        ArrayLen, BinaryOp, CastNode, ConstArg, ConstDeclNode, ConstExpr, ConstValue, ExprId,
        ExprKind, ExprNode, FieldAccessNode, Ident, Lit, Program, Stmt, StringPart, Type,
        TypeFolder, UnaryOp,
    },
    span::{SourceSpan, Span},
};

#[derive(Debug, Clone)]
pub(super) struct ConstEntry {
    ty: Option<Type>,
    value: ExprNode,
    span: Span,
    state: ConstState,
}

#[derive(Clone)]
pub(super) struct LocalConstEntry {
    info: LocalConstInfo,
    name: Ident,
    ty: Option<Type>,
    value: ExprNode,
    span: Span,
    module: ModuleScope,
    env: CallableTemplateEnv,
    state: ConstState,
}

#[derive(Debug, Clone)]
enum ConstState {
    Unevaluated,
    Evaluating,
    Evaluated(ConstValue),
    Failed,
}

pub(super) enum ConstNameLookup {
    Value(ConstValue),
    RuntimeGlobal(super::GlobalKey),
    NotConstLocal,
    Error(Box<TypeError>),
    Missing,
}

type ConstEvalResult<T> = Result<T, Box<TypeError>>;

fn const_error<T>(error: TypeError) -> ConstEvalResult<T> {
    Err(Box::new(error))
}

fn const_lookup(result: ConstEvalResult<ConstValue>) -> ConstNameLookup {
    match result {
        Ok(value) => ConstNameLookup::Value(value),
        Err(error) => ConstNameLookup::Error(error),
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

pub(super) fn check_const(const_node: &ConstDeclNode, tc: &mut TypeChecker) {
    let c = &const_node.node;
    let value = match tc.eval_const_expr(&c.value, true) {
        Ok(value) => value,
        Err(err) => {
            tc.push_error(err);
            return;
        }
    };
    let value_ty = const_type(&value);
    let ty = match &c.ty {
        Some(annot) => {
            let annot_ty = tc.resolve_type_for_tc_at(annot, const_node.span);
            tc.reject_user_any_type(&annot_ty, const_node.span);
            if annot_ty != value_ty {
                tc.push_error(TypeError::ConstTypeMismatch {
                    expected: annot_ty.clone(),
                    found: value_ty,
                    span: tc.error_span(const_node.span),
                });
            }
            annot_ty
        }
        None => value_ty,
    };
    tc.define_const(c.name, &ty, value);
}

pub(super) fn const_type(value: &ConstValue) -> Type {
    value.ty()
}

fn const_expr_has_param(expr: &ConstExpr) -> bool {
    match expr {
        ConstExpr::Param(_) => true,
        ConstExpr::Unary(_, expr) => const_expr_has_param(expr),
        ConstExpr::Binary(_, left, right) => {
            const_expr_has_param(left) || const_expr_has_param(right)
        }
        ConstExpr::Value(_) => false,
    }
}

pub(super) fn const_usize(value: &ConstValue, span: Option<SourceSpan>) -> ConstEvalResult<usize> {
    match value {
        ConstValue::Int(value) => usize::try_from(*value).map_err(|_| {
            Box::new(TypeError::NegativeArrayLength {
                value: *value,
                span,
            })
        }),
        value => const_error(TypeError::ExpectedIntConst {
            found: const_type(value),
            span,
        }),
    }
}

impl TypeChecker {
    pub(super) fn normalize_type_consts(&mut self, ty: &Type, span: Span) -> Type {
        ConstNormalizer { tc: self, span }.fold_type(ty)
    }

    pub(super) fn eval_const_term(
        &mut self,
        term: ConstTerm,
        span: Span,
        warn_deprecated: bool,
    ) -> Option<ConstTerm> {
        match term {
            ConstTerm::Value(_) => Some(term),
            ConstTerm::Name(name) => {
                if warn_deprecated {
                    self.warn_named_const_deprecated(name, span);
                }
                match self.lookup_visible_const_name(name, span) {
                    ConstNameLookup::Value(value) => Some(ConstTerm::Value(value)),
                    ConstNameLookup::RuntimeGlobal(global) => {
                        self.push_error_once(TypeError::RuntimeGlobalInConstPosition {
                            global,
                            span: self.error_span(span),
                        });
                        None
                    }
                    ConstNameLookup::Error(error) => {
                        self.push_error(*error);
                        None
                    }
                    ConstNameLookup::NotConstLocal => {
                        self.push_error_once(TypeError::NonConstExpression {
                            span: self.error_span(span),
                        });
                        None
                    }
                    ConstNameLookup::Missing => {
                        self.push_error_once(TypeError::UnknownConst {
                            name,
                            span: self.error_span(span),
                        });
                        None
                    }
                }
            }
            ConstTerm::Param(id) => match self
                .const_substs
                .last()
                .and_then(|subst| subst.get(&id).cloned())
            {
                Some(term) => self.eval_const_term(term, span, warn_deprecated),
                None => Some(ConstTerm::Param(id)),
            },
            ConstTerm::Expr(expr) => {
                self.normalize_symbolic_const_expr(expr, span)
                    .map(|expr| match expr {
                        ConstExpr::Value(value) => ConstTerm::Value(value),
                        expr => ConstTerm::Expr(expr),
                    })
            }
            ConstTerm::ArrayInfer | ConstTerm::Infer(_) => None,
        }
    }

    pub(super) fn require_usize_const(
        &mut self,
        term: ConstTerm,
        span: Span,
        warn_deprecated: bool,
    ) -> Option<usize> {
        match self.eval_const_term(term, span, warn_deprecated)? {
            ConstTerm::Value(value) => match const_usize(&value, self.error_span(span)) {
                Ok(value) => Some(value),
                Err(err) => {
                    self.push_error(err);
                    None
                }
            },
            ConstTerm::Name(name) => {
                self.push_error(TypeError::UnknownConst {
                    name,
                    span: self.error_span(span),
                });
                None
            }
            ConstTerm::Expr(expr) => match self.normalize_symbolic_const_expr(expr, span)? {
                ConstExpr::Value(value) => match const_usize(&value, self.error_span(span)) {
                    Ok(value) => Some(value),
                    Err(err) => {
                        self.push_error(err);
                        None
                    }
                },
                _ => None,
            },
            ConstTerm::Param(_) | ConstTerm::ArrayInfer | ConstTerm::Infer(_) => None,
        }
    }

    pub(super) fn array_len_from_term(&mut self, term: ConstTerm, span: Span) -> Option<ArrayLen> {
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
            ConstTerm::Value(_) | ConstTerm::Name(_) => self
                .require_usize_const(term, span, true)
                .map(ArrayLen::fixed),
            ConstTerm::Expr(expr) => match self.normalize_symbolic_const_expr(expr, span)? {
                ConstExpr::Value(value) => const_usize(&value, self.error_span(span))
                    .map(ArrayLen::fixed)
                    .map_err(|error| self.push_error(error))
                    .ok(),
                expr => Some(ArrayLen::Expr(expr)),
            },
            ConstTerm::Infer(_) => None,
        }
    }

    fn normalize_symbolic_const_expr(&mut self, expr: ConstExpr, span: Span) -> Option<ConstExpr> {
        match expr {
            ConstExpr::Value(_) => Some(expr),
            ConstExpr::Param(id) => {
                let term = self
                    .const_substs
                    .last()
                    .and_then(|subst| subst.get(&id).cloned());
                match term {
                    Some(ConstTerm::Value(value)) => Some(ConstExpr::Value(value)),
                    Some(ConstTerm::Param(id)) => Some(ConstExpr::Param(id)),
                    Some(ConstTerm::Expr(expr)) => self.normalize_symbolic_const_expr(expr, span),
                    Some(ConstTerm::Name(name)) => self
                        .eval_const_term(ConstTerm::Name(name), span, true)
                        .and_then(|term| match term {
                            ConstTerm::Value(value) => Some(ConstExpr::Value(value)),
                            _ => None,
                        }),
                    Some(ConstTerm::ArrayInfer | ConstTerm::Infer(_)) => None,
                    None => Some(ConstExpr::Param(id)),
                }
            }
            ConstExpr::Unary(op, expr) => {
                let expr = self.normalize_symbolic_const_expr(*expr, span)?;
                if let ConstExpr::Value(value) = expr {
                    match eval_unary(op, value, self.error_span(span)) {
                        Ok(value) => Some(ConstExpr::Value(value)),
                        Err(error) => {
                            self.push_error(error);
                            None
                        }
                    }
                } else {
                    Some(ConstExpr::Unary(op, Box::new(expr)))
                }
            }
            ConstExpr::Binary(op, left, right) => {
                let left = self.normalize_symbolic_const_expr(*left, span)?;
                let right = self.normalize_symbolic_const_expr(*right, span)?;
                match (left, right) {
                    (ConstExpr::Value(left), ConstExpr::Value(right)) => {
                        match eval_binary(op, left, right, self.error_span(span)) {
                            Ok(value) => Some(ConstExpr::Value(value)),
                            Err(error) => {
                                self.push_error(error);
                                None
                            }
                        }
                    }
                    (left, right) => Some(ConstExpr::Binary(op, Box::new(left), Box::new(right))),
                }
            }
        }
    }

    pub(super) fn normalize_const_arg(&mut self, arg: &ConstArg, span: Span) -> ConstArg {
        if let ConstArg::Name(name) = arg
            && let Some(param) = self.local_const_param(*name)
        {
            return ConstArg::Param(param);
        }
        let Some(term) = self.eval_const_term(ConstTerm::from_arg(arg), span, true) else {
            return arg.clone();
        };
        term.to_arg_no_infer().unwrap_or_else(|| arg.clone())
    }

    pub(super) fn normalize_array_len(&mut self, len: ArrayLen, span: Span) -> ArrayLen {
        if let ArrayLen::Named(name) = &len
            && let Some(expr) = self.symbolic_local_const(*name)
        {
            return match expr {
                ConstExpr::Param(id) => ArrayLen::Param(id),
                expr => ArrayLen::Expr(expr),
            };
        }
        self.array_len_from_term(ConstTerm::from_array_len(len), span)
            .unwrap_or(ArrayLen::Infer)
    }

    fn symbolic_local_const(&self, name: Ident) -> Option<ConstExpr> {
        let id = self.local_const_id_in_env(name, None)?;
        let mut visiting = HashSet::new();
        let expr = self.symbolic_local_const_id(id, &mut visiting)?;
        const_expr_has_param(&expr).then_some(expr)
    }

    fn symbolic_local_const_id(
        &self,
        id: LocalConstId,
        visiting: &mut HashSet<LocalConstId>,
    ) -> Option<ConstExpr> {
        if !visiting.insert(id) {
            return None;
        }
        let entry = self.local_consts.get(id.0 as usize)?;
        let expr = self.symbolic_const_expr(&entry.value, Some(&entry.env), visiting);
        visiting.remove(&id);
        expr
    }

    fn symbolic_const_expr(
        &self,
        expr: &ExprNode,
        env: Option<&CallableTemplateEnv>,
        visiting: &mut HashSet<LocalConstId>,
    ) -> Option<ConstExpr> {
        match &expr.node.kind {
            ExprKind::Lit(lit) => lit.const_value().map(ConstExpr::Value),
            ExprKind::Ident(name) => {
                if let Some(id) = self
                    .generic_contexts
                    .iter()
                    .rev()
                    .find_map(|context| context.const_param(*name))
                {
                    return Some(ConstExpr::Param(id));
                }
                let id = self.local_const_id_in_env(*name, env)?;
                self.symbolic_local_const_id(id, visiting)
            }
            ExprKind::Unary(node) => Some(ConstExpr::Unary(
                node.node.op,
                Box::new(self.symbolic_const_expr(&node.node.expr, env, visiting)?),
            )),
            ExprKind::Binary(node) => Some(ConstExpr::Binary(
                node.node.op,
                Box::new(self.symbolic_const_expr(&node.node.left, env, visiting)?),
                Box::new(self.symbolic_const_expr(&node.node.right, env, visiting)?),
            )),
            _ => None,
        }
    }

    fn local_const_param(&self, name: Ident) -> Option<crate::ast::ConstParamId> {
        let (LocalSymbol::Value(symbol), _) = self.lookup_local_symbol(name)? else {
            return None;
        };
        let id = symbol.local_const?;
        let entry = self.local_consts.get(id.0 as usize)?;
        let ExprKind::Ident(param) = entry.value.node.kind else {
            return None;
        };
        self.generic_contexts
            .iter()
            .rev()
            .find_map(|context| context.const_param(param))
    }

    pub(super) fn collect_const_decls(&mut self, module: &ModuleScope, program: &Program) {
        for stmt in &program.stmts {
            let Stmt::Const(node) = &stmt.node else {
                continue;
            };
            self.consts.insert(
                (module.clone(), node.node.name),
                ConstEntry {
                    ty: node.node.ty.clone(),
                    value: node.node.value.clone(),
                    span: node.span,
                    state: ConstState::Unevaluated,
                },
            );
        }
    }

    pub(super) fn declare_local_const(
        &mut self,
        node: &ConstDeclNode,
        env: CallableTemplateEnv,
    ) -> LocalConstInfo {
        let info = LocalConstInfo {
            binding_id: self.fresh_binding_id(),
            type_id: self.solver.alloc_local_type(&Type::Infer),
            id: LocalConstId(self.local_consts.len() as u32),
        };
        self.local_consts.push(LocalConstEntry {
            info,
            name: node.node.name,
            ty: node.node.ty.clone(),
            value: node.node.value.clone(),
            span: node.span,
            module: self.current_module.clone(),
            env,
            state: ConstState::Unevaluated,
        });
        info
    }

    pub(super) fn set_local_const_env(&mut self, id: LocalConstId, env: CallableTemplateEnv) {
        if let Some(entry) = self.local_consts.get_mut(id.0 as usize)
            && matches!(entry.state, ConstState::Unevaluated)
        {
            entry.env = env;
        }
    }

    pub(super) fn eval_module_consts(&mut self, module: &ModuleScope) {
        let names = self
            .consts
            .keys()
            .filter_map(|(scope, name)| (scope == module).then_some(*name))
            .collect::<Vec<_>>();
        for name in names {
            if let Err(err) = self.eval_top_const(module, name, None) {
                self.push_error(err);
            }
        }
    }

    pub(super) fn expr_depends_on_const_params(
        &self,
        expr: &ExprNode,
        params: &HashSet<Ident>,
    ) -> bool {
        self.expr_depends_on_const_params_with_shadowed(expr, params, &HashSet::new())
    }

    pub(super) fn expr_depends_on_const_params_with_shadowed(
        &self,
        expr: &ExprNode,
        params: &HashSet<Ident>,
        shadowed: &HashSet<Ident>,
    ) -> bool {
        self.expr_depends_on_const_params_inner(expr, params, shadowed, None, &mut HashSet::new())
    }

    fn expr_depends_on_const_params_inner(
        &self,
        expr: &ExprNode,
        params: &HashSet<Ident>,
        shadowed: &HashSet<Ident>,
        env: Option<&CallableTemplateEnv>,
        visiting: &mut HashSet<LocalConstId>,
    ) -> bool {
        match &expr.node.kind {
            ExprKind::Ident(name) => {
                if shadowed.contains(name) {
                    return false;
                }
                if params.contains(name) {
                    return true;
                }
                let Some(id) = self.local_const_id_in_env(*name, env) else {
                    return false;
                };
                if !visiting.insert(id) {
                    return false;
                }
                let depends = self.local_consts.get(id.0 as usize).is_some_and(|entry| {
                    self.expr_depends_on_const_params_inner(
                        &entry.value,
                        params,
                        &HashSet::new(),
                        Some(&entry.env),
                        visiting,
                    )
                });
                visiting.remove(&id);
                depends
            }
            ExprKind::Unary(node) => self.expr_depends_on_const_params_inner(
                &node.node.expr,
                params,
                shadowed,
                env,
                visiting,
            ),
            ExprKind::Binary(node) => {
                self.expr_depends_on_const_params_inner(
                    &node.node.left,
                    params,
                    shadowed,
                    env,
                    visiting,
                ) || self.expr_depends_on_const_params_inner(
                    &node.node.right,
                    params,
                    shadowed,
                    env,
                    visiting,
                )
            }
            ExprKind::Cast(node) | ExprKind::FailableCast(node) => self
                .expr_depends_on_const_params_inner(
                    &node.node.expr,
                    params,
                    shadowed,
                    env,
                    visiting,
                ),
            ExprKind::Ternary(node) => {
                self.expr_depends_on_const_params_inner(
                    &node.node.cond,
                    params,
                    shadowed,
                    env,
                    visiting,
                ) || self.expr_depends_on_const_params_inner(
                    &node.node.then_expr,
                    params,
                    shadowed,
                    env,
                    visiting,
                ) || self.expr_depends_on_const_params_inner(
                    &node.node.else_expr,
                    params,
                    shadowed,
                    env,
                    visiting,
                )
            }
            ExprKind::StringInterp(parts) => parts.iter().any(|part| match part {
                StringPart::Expr(expr, _) => {
                    self.expr_depends_on_const_params_inner(expr, params, shadowed, env, visiting)
                }
                StringPart::Text(_) => false,
            }),
            _ => false,
        }
    }

    fn local_const_id_in_env(
        &self,
        name: Ident,
        env: Option<&CallableTemplateEnv>,
    ) -> Option<LocalConstId> {
        let symbol = match env {
            Some(CallableTemplateEnv::Local(state)) => {
                state.scopes.iter().rev().find_map(|scope| scope.get(&name))
            }
            Some(CallableTemplateEnv::SourceModule) => None,
            None => self.lookup_local_symbol(name).map(|(symbol, _)| symbol),
        };
        let LocalSymbol::Value(symbol) = symbol? else {
            return None;
        };
        symbol.local_const
    }

    pub(super) fn eval_const_expr(
        &mut self,
        expr: &ExprNode,
        warn_deprecated: bool,
    ) -> ConstEvalResult<ConstValue> {
        if let Some(value) = self.raw_projection_const(expr) {
            return Ok(value);
        }
        match &expr.node.kind {
            ExprKind::Lit(lit) => self.eval_const_lit(lit, expr.span),
            ExprKind::Ident(name) => {
                if let Some(value) = self
                    .flag_initializer_values
                    .last()
                    .and_then(|values| values.get(name))
                {
                    return Ok(ConstValue::Int(*value));
                }
                let flag_values = self.flag_initializer_values.pop();
                if warn_deprecated {
                    self.warn_named_const_deprecated(*name, expr.span);
                }
                let lookup = self.lookup_visible_const_name(*name, expr.span);
                self.flag_initializer_values.extend(flag_values);
                match lookup {
                    ConstNameLookup::Value(value) => Ok(value),
                    ConstNameLookup::RuntimeGlobal(global) => {
                        const_error(TypeError::RuntimeGlobalInConstPosition {
                            global,
                            span: self.error_span(expr.span),
                        })
                    }
                    ConstNameLookup::NotConstLocal => const_error(TypeError::NonConstExpression {
                        span: self.error_span(expr.span),
                    }),
                    ConstNameLookup::Error(error) => Err(error),
                    ConstNameLookup::Missing => const_error(TypeError::UnknownConst {
                        name: *name,
                        span: self.error_span(expr.span),
                    }),
                }
            }
            ExprKind::Unary(node) => {
                let value = self.eval_const_expr(&node.node.expr, warn_deprecated)?;
                eval_unary(node.node.op, value, self.error_span(node.span))
            }
            ExprKind::Binary(node) => match node.node.op {
                BinaryOp::And => {
                    let left = bool_operand(
                        node.node.op,
                        self.eval_const_expr(&node.node.left, warn_deprecated)?,
                        self.error_span(node.span),
                    )?;
                    if !left {
                        return Ok(ConstValue::Bool(false));
                    }
                    eval_binary(
                        node.node.op,
                        ConstValue::Bool(left),
                        self.eval_const_expr(&node.node.right, warn_deprecated)?,
                        self.error_span(node.span),
                    )
                }
                BinaryOp::Or => {
                    let left = bool_operand(
                        node.node.op,
                        self.eval_const_expr(&node.node.left, warn_deprecated)?,
                        self.error_span(node.span),
                    )?;
                    if left {
                        return Ok(ConstValue::Bool(true));
                    }
                    eval_binary(
                        node.node.op,
                        ConstValue::Bool(left),
                        self.eval_const_expr(&node.node.right, warn_deprecated)?,
                        self.error_span(node.span),
                    )
                }
                _ => {
                    let left = self.eval_const_expr(&node.node.left, warn_deprecated)?;
                    let right = self.eval_const_expr(&node.node.right, warn_deprecated)?;
                    eval_binary(node.node.op, left, right, self.error_span(node.span))
                }
            },
            ExprKind::Cast(node) => self.eval_const_cast(node, warn_deprecated),
            ExprKind::Ternary(node) => {
                let cond = self.eval_const_expr(&node.node.cond, warn_deprecated)?;
                match cond {
                    ConstValue::Bool(true) => {
                        self.eval_const_expr(&node.node.then_expr, warn_deprecated)
                    }
                    ConstValue::Bool(false) => {
                        self.eval_const_expr(&node.node.else_expr, warn_deprecated)
                    }
                    other => const_error(TypeError::InvalidOperand {
                        op: "?:".to_string(),
                        operand_type: const_type(&other),
                        span: self.error_span(node.node.cond.span),
                    }),
                }
            }
            ExprKind::StringInterp(parts) => {
                self.eval_const_string_interp(parts, expr.span, warn_deprecated)
            }
            ExprKind::Field(node) => self.eval_const_field(node, expr.span, warn_deprecated),
            _ => const_error(TypeError::NonConstExpression {
                span: self.error_span(expr.span),
            }),
        }
    }

    fn raw_projection_const(&self, expr: &ExprNode) -> Option<ConstValue> {
        let fact = self
            .semantic_facts
            .body(&self.current_body())?
            .raw_projections
            .get(&expr.node.id)?;
        let source = if fact.source_expr == expr.node.id {
            expr
        } else {
            let ExprKind::Cast(cast) = &expr.node.kind else {
                return None;
            };
            if cast.node.expr.node.id != fact.source_expr {
                return None;
            }
            &cast.node.expr
        };
        let kind = &self.decls.enum_schema_for_type(&fact.source_ty)?.body.kind;
        if let Some(flag) = kind.flag() {
            return self
                .flag_const_bits(source, flag.known_bits)
                .map(ConstValue::Int);
        }
        let raw = kind.raw()?;
        let ExprKind::Field(field) = &source.node.kind else {
            return None;
        };
        match raw.value(field.node.field)? {
            RawEnumValue::Int(value) => Some(ConstValue::Int(*value)),
            RawEnumValue::String(value) => Some(ConstValue::String(value.clone())),
        }
    }

    fn flag_const_bits(&self, expr: &ExprNode, known_bits: i64) -> Option<i64> {
        let facts = self.semantic_facts.body(&self.current_body())?;
        if let Some(member) = facts.flag_members.get(&expr.node.id) {
            return Some(member.value);
        }
        if let Some(static_) = facts.flag_statics.get(&expr.node.id) {
            return Some(match static_.op {
                super::FlagStaticOp::Empty => 0,
                super::FlagStaticOp::All => known_bits,
            });
        }
        match &expr.node.kind {
            ExprKind::Binary(binary) => {
                let left = self.flag_const_bits(&binary.node.left, known_bits)?;
                let right = self.flag_const_bits(&binary.node.right, known_bits)?;
                match binary.node.op {
                    BinaryOp::BitAnd => Some(left & right),
                    BinaryOp::BitOr => Some(left | right),
                    BinaryOp::Xor => Some(left ^ right),
                    _ => None,
                }
            }
            ExprKind::Unary(unary) if unary.node.op == UnaryOp::BitNot => {
                Some(known_bits ^ self.flag_const_bits(&unary.node.expr, known_bits)?)
            }
            _ => None,
        }
    }

    fn eval_const_field(
        &mut self,
        node: &FieldAccessNode,
        span: Span,
        warn_deprecated: bool,
    ) -> ConstEvalResult<ConstValue> {
        let err_span = self.error_span(span);
        if node.node.safe {
            return const_error(TypeError::NonConstExpression { span: err_span });
        }
        let ExprKind::Ident(module) = &node.node.target.node.kind else {
            return const_error(TypeError::NonConstExpression { span: err_span });
        };
        if let Some(owner) = self.visible_type_subject(*module, node.node.target.span) {
            self.ensure_pending_enum_values_for_type(&owner, node.node.target.span);
            if let Some(schema) = self.decls.enum_schema_for_type(&owner) {
                if let Some(value) = schema
                    .body
                    .kind
                    .raw()
                    .and_then(|raw| raw.value(node.node.field))
                {
                    return Ok(match value {
                        RawEnumValue::Int(value) => ConstValue::Int(*value),
                        RawEnumValue::String(value) => ConstValue::String(value.clone()),
                    });
                }
                if let Some(member) = schema
                    .body
                    .kind
                    .flag()
                    .and_then(|flag| flag.member(node.node.field))
                {
                    return Ok(ConstValue::Int(member.value));
                }
            }
        }
        let Some(scope) = self.lookup_module_alias(*module) else {
            return const_error(TypeError::NonConstExpression { span: err_span });
        };
        match self.exported_value_in_module(&scope, node.node.field) {
            Some((module, name, ValueDecl::Const(sig))) => {
                if warn_deprecated {
                    self.warn_deprecated(&sig.policy, DeprecatedUseKind::Const, name, span);
                }
                self.eval_top_const(&module, name, err_span)
            }
            Some((_, _, ValueDecl::Global(sig))) => {
                const_error(TypeError::RuntimeGlobalInConstPosition {
                    global: sig.key,
                    span: err_span,
                })
            }
            Some((_, _, ValueDecl::Func(_))) => {
                const_error(TypeError::NonConstExpression { span: err_span })
            }
            None => const_error(TypeError::UndefinedModuleMember {
                module: scope,
                name: node.node.field,
                span: err_span,
            }),
        }
    }

    pub(super) fn lookup_visible_const_name(&mut self, name: Ident, span: Span) -> ConstNameLookup {
        match self.resolve_ident_subject(name, span, super::NameSubjectMode::Const) {
            super::ResolvedIdentSubject::Local(LocalSymbol::Value(info), _) => {
                return self
                    .eval_const_symbol_value(&info, name, span)
                    .map_or(ConstNameLookup::NotConstLocal, const_lookup);
            }
            super::ResolvedIdentSubject::Local(LocalSymbol::Callable(_), _) => {
                return ConstNameLookup::NotConstLocal;
            }
            super::ResolvedIdentSubject::Blocked(error) => return ConstNameLookup::Error(error),
            super::ResolvedIdentSubject::Missing => {}
            super::ResolvedIdentSubject::Named(..)
            | super::ResolvedIdentSubject::Module(_)
            | super::ResolvedIdentSubject::Type(_) => unreachable!(),
        }

        if self.has_top_const(&self.current_module, name) {
            let module = self.current_module.clone();
            return const_lookup(self.eval_top_const(&module, name, self.error_span(span)));
        }

        if let Some((_, _, ValueDecl::Global(sig))) = self.current_module_value(name) {
            return ConstNameLookup::RuntimeGlobal(sig.key);
        }

        match self.imported_value(name) {
            Some((module, imported_name, ValueDecl::Const(_))) => {
                const_lookup(self.eval_top_const(&module, imported_name, self.error_span(span)))
            }
            Some((_, _, ValueDecl::Global(sig))) => ConstNameLookup::RuntimeGlobal(sig.key),
            Some((_, _, ValueDecl::Func(_))) | None => ConstNameLookup::Missing,
        }
    }

    fn eval_const_symbol_value(
        &mut self,
        info: &VarInfo,
        name: Ident,
        span: Span,
    ) -> Option<ConstEvalResult<ConstValue>> {
        if let Some(value) = info.const_value.clone() {
            return Some(Ok(value));
        }
        if let Some(id) = info.local_const {
            return Some(self.eval_local_const(id, span));
        }
        if info.kind.is_const() && self.has_top_const(&self.current_module, name) {
            let module = self.current_module.clone();
            return Some(self.eval_top_const(&module, name, self.error_span(span)));
        }
        None
    }

    pub(super) fn check_local_const_value_expr(
        &mut self,
        expr: &ExprNode,
        name: Ident,
        info: &VarInfo,
    ) -> Option<CheckedType> {
        let value = self.eval_const_symbol_value(info, name, expr.span)?;
        self.warn_local_const_deprecated(info, name, expr.span);
        Some(self.check_const_value_expr(expr, value))
    }

    pub(super) fn check_const_value_expr(
        &mut self,
        expr: &ExprNode,
        value: ConstEvalResult<ConstValue>,
    ) -> CheckedType {
        let ty = self.record_const_value_result(expr.node.id, value);
        checked_from_type(expr, ty, self)
    }

    pub(super) fn record_const_value_result(
        &mut self,
        expr_id: ExprId,
        value: ConstEvalResult<ConstValue>,
    ) -> Type {
        match value {
            Ok(value) => {
                let ty = const_type(&value);
                self.record_const_value(expr_id, value);
                ty
            }
            Err(error) => {
                self.push_error(error);
                Type::Infer
            }
        }
    }

    pub(super) fn eval_local_const(
        &mut self,
        id: LocalConstId,
        span: Span,
    ) -> ConstEvalResult<ConstValue> {
        let err_span = self.error_span(span);
        let Some(entry) = self.local_consts.get(id.0 as usize) else {
            return const_error(TypeError::NonConstExpression { span: err_span });
        };
        match entry.state.clone() {
            ConstState::Evaluated(value) => return Ok(value),
            ConstState::Evaluating => {
                return const_error(TypeError::ConstCycle {
                    name: entry.name,
                    span: err_span,
                });
            }
            ConstState::Failed => {
                return const_error(TypeError::NonConstExpression { span: err_span });
            }
            ConstState::Unevaluated => {}
        }

        let (info, ty, value_expr, decl_span, module, env) = {
            let entry = self
                .local_consts
                .get_mut(id.0 as usize)
                .expect("local const entry exists");
            entry.state = ConstState::Evaluating;
            (
                entry.info,
                entry.ty.clone(),
                entry.value.clone(),
                entry.span,
                entry.module.clone(),
                entry.env.clone(),
            )
        };

        let result = with_callable_body_env(&module, &env, self, |tc| {
            eval_const_decl(ty.as_ref(), &value_expr, decl_span, tc)
        });

        match result {
            Ok((value, ty)) => {
                if let Some(entry) = self.local_consts.get_mut(id.0 as usize) {
                    entry.state = ConstState::Evaluated(value.clone());
                }
                self.solver.set_local_type_from_type(info.type_id, &ty);
                Ok(value)
            }
            Err(err) => {
                if let Some(entry) = self.local_consts.get_mut(id.0 as usize) {
                    entry.state = ConstState::Failed;
                }
                Err(err)
            }
        }
    }

    fn has_top_const(&self, module: &ModuleScope, name: Ident) -> bool {
        self.consts.contains_key(&(module.clone(), name))
    }

    pub(super) fn eval_top_const(
        &mut self,
        module: &ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    ) -> ConstEvalResult<ConstValue> {
        let key = (module.clone(), name);
        let Some(state) = self.consts.get(&key).map(|entry| entry.state.clone()) else {
            return const_error(TypeError::UnknownConst { name, span });
        };
        match state {
            ConstState::Evaluated(value) => return Ok(value),
            ConstState::Evaluating => return const_error(TypeError::ConstCycle { name, span }),
            ConstState::Failed => return const_error(TypeError::NonConstExpression { span }),
            ConstState::Unevaluated => {}
        }

        let (ty, value_expr, decl_span) = {
            let entry = self.consts.get_mut(&key).expect("const entry exists");
            entry.state = ConstState::Evaluating;
            (entry.ty.clone(), entry.value.clone(), entry.span)
        };

        let previous_module = std::mem::replace(&mut self.current_module, module.clone());
        let saved_state = (previous_module != *module).then(|| self.take_scope_state());
        let result = eval_const_decl(ty.as_ref(), &value_expr, decl_span, self);
        if let Some(state) = saved_state {
            self.restore_scope_state(state);
        }
        self.current_module = previous_module;

        match result {
            Ok((value, ty)) => {
                if let Some(entry) = self.consts.get_mut(&key) {
                    entry.state = ConstState::Evaluated(value.clone());
                }
                self.decls.set_const_type(module, name, &ty);
                self.set_current_scope_const(module, name, &ty, value.clone());
                Ok(value)
            }
            Err(err) => {
                if let Some(entry) = self.consts.get_mut(&key) {
                    entry.state = ConstState::Failed;
                }
                Err(err)
            }
        }
    }

    fn set_current_scope_const(
        &mut self,
        module: &ModuleScope,
        name: Ident,
        ty: &Type,
        value: ConstValue,
    ) {
        if &self.current_module != module {
            return;
        }
        let Some(scope) = self.scopes.first_mut() else {
            return;
        };
        let Some(LocalSymbol::Value(info)) = scope.get_mut(&name) else {
            return;
        };
        let type_id = info.type_id;
        info.const_value = Some(value);
        self.solver.set_local_type_from_type(type_id, ty);
    }

    fn eval_const_lit(&self, lit: &Lit, span: Span) -> ConstEvalResult<ConstValue> {
        lit.const_value().ok_or_else(|| {
            Box::new(TypeError::NonConstExpression {
                span: self.error_span(span),
            })
        })
    }

    fn eval_const_cast(
        &mut self,
        node: &CastNode,
        warn_deprecated: bool,
    ) -> ConstEvalResult<ConstValue> {
        let value = self.eval_const_expr(&node.node.expr, warn_deprecated)?;
        let from = const_type(&value);
        let to = self.resolve_type_for_tc_at(&node.node.target, node.span);
        match (value, &to) {
            (value, _) if from == to => Ok(value),
            (ConstValue::Int(int), Type::Float) => Ok(ConstValue::Float(int_to_float(int))),
            (ConstValue::Float(float), Type::Int) => match float_to_int(float) {
                Ok(int) => Ok(ConstValue::Int(int)),
                Err(error) => const_error(TypeError::ConstFloatToInt {
                    error,
                    span: self.error_span(node.span),
                }),
            },
            _ => const_error(TypeError::InvalidConstCast {
                from,
                to,
                span: self.error_span(node.span),
            }),
        }
    }

    fn eval_const_string_interp(
        &mut self,
        parts: &[StringPart],
        span: Span,
        warn_deprecated: bool,
    ) -> ConstEvalResult<ConstValue> {
        let mut out = String::new();
        for part in parts {
            match part {
                StringPart::Text(text) => out.push_str(text),
                StringPart::Expr(expr, _) => {
                    let value = self.eval_const_expr(expr, warn_deprecated)?;
                    out.push_str(&const_string(&value));
                }
            }
        }
        let empty_interpolation = out.is_empty() && parts.is_empty();
        if empty_interpolation {
            return const_error(TypeError::NonConstExpression {
                span: self.error_span(span),
            });
        }
        Ok(ConstValue::String(out))
    }
}

fn eval_const_decl(
    ty: Option<&Type>,
    value_expr: &ExprNode,
    decl_span: Span,
    tc: &mut TypeChecker,
) -> ConstEvalResult<(ConstValue, Type)> {
    let expected_ty = ty.map(|annot| tc.resolve_type_for_tc_at(annot, decl_span));
    let expected_handle = expected_ty.as_ref().map(TypeChecker::type_handle);
    super::validate_const_expr_type(value_expr, expected_handle, tc)?;
    let value = tc.eval_const_expr(value_expr, false)?;
    let value_ty = const_type(&value);
    match expected_ty {
        Some(expected) if expected != value_ty => const_error(TypeError::ConstTypeMismatch {
            expected,
            found: value_ty,
            span: tc.error_span(decl_span),
        }),
        Some(expected) => Ok((value, expected)),
        None => Ok((value, value_ty)),
    }
}

fn const_string(value: &ConstValue) -> String {
    match value {
        ConstValue::String(value) => value.clone(),
        ConstValue::Char(value) => value.to_string(),
        ConstValue::Float(value) => display_float(*value),
        ConstValue::Int(_) | ConstValue::Bool(_) => format!("{value}"),
    }
}

fn bool_operand(
    op: BinaryOp,
    value: ConstValue,
    span: Option<SourceSpan>,
) -> ConstEvalResult<bool> {
    match value {
        ConstValue::Bool(value) => Ok(value),
        value => const_error(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: const_type(&value),
            span,
        }),
    }
}

pub(super) fn eval_unary(
    op: UnaryOp,
    value: ConstValue,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    match (op, value) {
        (UnaryOp::Neg, ConstValue::Int(value)) => value
            .checked_neg()
            .map(ConstValue::Int)
            .ok_or_else(|| Box::new(TypeError::ConstOverflow { span })),
        (UnaryOp::Neg, ConstValue::Float(value)) => Ok(ConstValue::Float(-value)),
        (UnaryOp::Not, ConstValue::Bool(value)) => Ok(ConstValue::Bool(!value)),
        (UnaryOp::BitNot, ConstValue::Int(value)) => Ok(ConstValue::Int(!value)),
        (_, value) => const_error(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: const_type(&value),
            span,
        }),
    }
}

pub(super) fn eval_binary(
    op: BinaryOp,
    left: ConstValue,
    right: ConstValue,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    match (left, right) {
        (ConstValue::Int(a), ConstValue::Int(b)) => eval_int_binary(op, a, b, span),
        (ConstValue::Float(a), ConstValue::Float(b)) => eval_float_binary(op, a, b, span),
        (ConstValue::Bool(a), ConstValue::Bool(b)) => eval_bool_binary(op, a, b, span),
        (ConstValue::String(a), ConstValue::String(b)) => eval_string_binary(op, &a, &b, span),
        (ConstValue::Char(a), ConstValue::Char(b)) => eval_char_binary(op, a, b, span),
        (ConstValue::String(_), ConstValue::Char(_))
        | (ConstValue::Char(_), ConstValue::String(_))
            if op == BinaryOp::Add =>
        {
            const_error(TypeError::InvalidOperand {
                op: format!("{op}"),
                operand_type: Type::Char,
                span,
            })
        }
        (ConstValue::String(a), value) if op == BinaryOp::Add => {
            Ok(ConstValue::String(format!("{a}{}", const_string(&value))))
        }
        (value, ConstValue::String(b)) if op == BinaryOp::Add => {
            Ok(ConstValue::String(format!("{}{b}", const_string(&value))))
        }
        (left, right) => const_error(TypeError::TypeMismatch {
            expected: const_type(&left),
            found: const_type(&right),
            span,
        }),
    }
}

fn eval_int_binary(
    op: BinaryOp,
    a: i64,
    b: i64,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    let int = |value| Ok(ConstValue::Int(value));
    let bool = |value| Ok(ConstValue::Bool(value));
    match op {
        BinaryOp::Add => a.checked_add(b).map(ConstValue::Int),
        BinaryOp::Sub => a.checked_sub(b).map(ConstValue::Int),
        BinaryOp::Mul => a.checked_mul(b).map(ConstValue::Int),
        BinaryOp::Div | BinaryOp::Rem if b == 0 => {
            return const_error(TypeError::ConstDivisionByZero { span });
        }
        BinaryOp::Div => a.checked_div(b).map(ConstValue::Int),
        BinaryOp::Rem => a.checked_rem(b).map(ConstValue::Int),
        BinaryOp::Eq => return bool(a == b),
        BinaryOp::NotEq => return bool(a != b),
        BinaryOp::LessThan => return bool(a < b),
        BinaryOp::GreaterThan => return bool(a > b),
        BinaryOp::LessThanEq => return bool(a <= b),
        BinaryOp::GreaterThanEq => return bool(a >= b),
        BinaryOp::BitAnd => return int(a & b),
        BinaryOp::BitOr => return int(a | b),
        BinaryOp::Xor => return int(a ^ b),
        BinaryOp::Shl => u32::try_from(b)
            .ok()
            .and_then(|rhs| a.checked_shl(rhs))
            .map(ConstValue::Int),
        BinaryOp::Shr => u32::try_from(b)
            .ok()
            .and_then(|rhs| a.checked_shr(rhs))
            .map(ConstValue::Int),
        BinaryOp::And | BinaryOp::Or | BinaryOp::Coalesce => {
            return const_error(TypeError::InvalidOperand {
                op: format!("{op}"),
                operand_type: Type::Int,
                span,
            });
        }
    }
    .ok_or_else(|| Box::new(TypeError::ConstOverflow { span }))
}

fn eval_float_binary(
    op: BinaryOp,
    a: f64,
    b: f64,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    match op {
        BinaryOp::Add => Ok(ConstValue::Float(a + b)),
        BinaryOp::Sub => Ok(ConstValue::Float(a - b)),
        BinaryOp::Mul => Ok(ConstValue::Float(a * b)),
        BinaryOp::Div => Ok(ConstValue::Float(a / b)),
        BinaryOp::Rem => Ok(ConstValue::Float(a % b)),
        BinaryOp::Eq => Ok(ConstValue::Bool(a == b)),
        BinaryOp::NotEq => Ok(ConstValue::Bool(a != b)),
        BinaryOp::LessThan => Ok(ConstValue::Bool(a < b)),
        BinaryOp::GreaterThan => Ok(ConstValue::Bool(a > b)),
        BinaryOp::LessThanEq => Ok(ConstValue::Bool(a <= b)),
        BinaryOp::GreaterThanEq => Ok(ConstValue::Bool(a >= b)),
        _ => const_error(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: Type::Float,
            span,
        }),
    }
}

fn eval_bool_binary(
    op: BinaryOp,
    a: bool,
    b: bool,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    match op {
        BinaryOp::And => Ok(ConstValue::Bool(a && b)),
        BinaryOp::Or => Ok(ConstValue::Bool(a || b)),
        BinaryOp::Eq => Ok(ConstValue::Bool(a == b)),
        BinaryOp::NotEq => Ok(ConstValue::Bool(a != b)),
        _ => const_error(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: Type::Bool,
            span,
        }),
    }
}

fn eval_ordered_binary<T: PartialEq + PartialOrd + ?Sized>(
    op: BinaryOp,
    a: &T,
    b: &T,
    operand_type: Type,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    match op {
        BinaryOp::Eq => Ok(ConstValue::Bool(a == b)),
        BinaryOp::NotEq => Ok(ConstValue::Bool(a != b)),
        BinaryOp::LessThan => Ok(ConstValue::Bool(a < b)),
        BinaryOp::GreaterThan => Ok(ConstValue::Bool(a > b)),
        BinaryOp::LessThanEq => Ok(ConstValue::Bool(a <= b)),
        BinaryOp::GreaterThanEq => Ok(ConstValue::Bool(a >= b)),
        _ => const_error(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type,
            span,
        }),
    }
}

fn eval_char_binary(
    op: BinaryOp,
    a: char,
    b: char,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    eval_ordered_binary(op, &a, &b, Type::Char, span)
}

fn eval_string_binary(
    op: BinaryOp,
    a: &str,
    b: &str,
    span: Option<SourceSpan>,
) -> ConstEvalResult<ConstValue> {
    match op {
        BinaryOp::Add => Ok(ConstValue::String(format!("{a}{b}"))),
        _ => eval_ordered_binary(op, a, b, Type::String, span),
    }
}
