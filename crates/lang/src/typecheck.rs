use crate::{
    ast::{
        AssignNode, AssignOp, BinaryNode, BinaryOp, BindingNode, CallNode, ExprId, ExprKind,
        ExprNode, Func, FuncNode, Ident, Lit, Program, ReturnNode, Stmt, StmtNode, Type, UnaryNode,
        UnaryOp,
    },
    span::Span,
};
use std::collections::HashMap;

pub fn check_program(program: &Program) -> Result<TypeChecker, Vec<TypeErr>> {
    let mut type_checker = TypeChecker::default();
    let mut errors = vec![];

    // first pass we collect the types from the ast
    check_block_stmts(&program.stmts, &mut type_checker, &mut errors);

    if !errors.is_empty() {
        return Err(errors);
    }

    // second pass we infer the types from the constraints
    resolve_constraints(&mut type_checker, &mut errors);

    // at this point there should be no remaining unresolved types
    // so if there are any we add an error
    for (_expr_id, (span, ty)) in &type_checker.types {
        if contains_infer(ty) {
            errors.push(TypeErr {
                span: *span,
                kind: TypeErrKind::UnresolvedInfer,
            });
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(type_checker)
}

#[derive(Debug)]
pub struct TypeChecker {
    types: HashMap<ExprId, (Span, Type)>,
    scopes: Vec<HashMap<Ident, Type>>,
    return_types: Vec<Type>,
    constraints: Vec<Constraint>,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self {
            types: HashMap::new(),
            scopes: vec![],
            return_types: vec![],
            constraints: vec![],
        }
    }
}

impl TypeChecker {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn set_type(&mut self, id: ExprId, ty: Type, span: Span) {
        self.types.insert(id, (span, ty));
    }

    pub fn get_type(&self, id: ExprId) -> Option<&(Span, Type)> {
        self.types.get(&id)
    }

    pub fn set_var(&mut self, name: Ident, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    pub fn get_var(&self, name: Ident) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(&name) {
                return Some(ty);
            }
        }
        None
    }

    fn push_return_type(&mut self, ty: Type) {
        self.return_types.push(ty);
    }

    fn pop_return_type(&mut self) {
        self.return_types.pop();
    }

    fn current_return_type(&self) -> Option<&Type> {
        self.return_types.last()
    }

    fn add_constraint(&mut self, span: Span, left: TypeRef, right: TypeRef) {
        self.constraints.push(Constraint { span, left, right });
    }

    fn get_type_ref(&self, r: &TypeRef) -> Option<Type> {
        match r {
            TypeRef::Expr(id) => self.get_type(*id).map(|(_, ty)| ty.clone()),
            TypeRef::Var(ident) => self.get_var(*ident).cloned(),
            TypeRef::Concrete(t) => Some(t.clone()),
        }
    }

    fn set_type_ref(&mut self, r: &TypeRef, ty: Type, span: Span) {
        match r {
            TypeRef::Expr(id) => self.set_type(*id, ty, span),
            TypeRef::Var(ident) => self.set_var(*ident, ty),
            TypeRef::Concrete(_) => {} // Cannot write to concrete types
        }
    }

    /// Constrains two types that must be the same
    /// ie: let x:int = 10; x += 10; (int to int)
    fn constrain_equal(
        &mut self,
        span: Span,
        left: TypeRef,
        right: TypeRef,
        errors: &mut Vec<TypeErr>,
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
    fn constrain_assignable(
        &mut self,
        span: Span,
        from: TypeRef,
        to: TypeRef,
        errors: &mut Vec<TypeErr>,
    ) {
        let from_ty = self.get_type_ref(&from).unwrap_or(Type::Infer);
        let to_ty = self.get_type_ref(&to).unwrap_or(Type::Infer);

        // if both types are resolved, check assignability and set the types
        let both_resolved = !(contains_infer(&from_ty) || contains_infer(&to_ty));
        if both_resolved {
            if !is_assignable(&from_ty, &to_ty) {
                errors.push(TypeErr {
                    span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: to_ty.clone(),
                        found: from_ty.clone(),
                    },
                });
            }
            self.set_type_ref(&to, to_ty, span);
            self.set_type_ref(&from, from_ty, span);
            return;
        }

        // at this point at least one type is unresolved
        // if both are optionals, constrain the inner types
        if let (Type::Optional(inner_from), Type::Optional(inner_to)) = (&from_ty, &to_ty) {
            let inner_from_ref = TypeRef::Concrete(*inner_from.clone());
            let inner_to_ref = TypeRef::Concrete(*inner_to.clone());
            self.constrain_equal(span, inner_from_ref, inner_to_ref, errors);
            self.set_type_ref(&from, Type::Optional(inner_to.clone()), span);
            return;
        }

        // if to is an optional and from has inference, constrain from to the inner type of to
        if let Type::Optional(inner_to) = &to_ty
            && contains_infer(&from_ty)
        {
            self.constrain_equal(span, from, TypeRef::Concrete(*inner_to.clone()), errors);
            return;
        }

        // otherwise just constrain them to be the same as fallback
        self.constrain_equal(span, from, to, errors);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeErr {
    pub span: Span,
    pub kind: TypeErrKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrKind {
    UnknownVariable { name: Ident },
    MismatchedTypes { expected: Type, found: Type },
    InvalidOperand { op: String, operand_type: Type },
    UnknownFunction { name: Ident },
    UnresolvedInfer,
    NotAFunction { expr_type: Type },
}

#[derive(Debug, Clone)]
enum TypeRef {
    Expr(ExprId),
    Var(Ident),
    Concrete(Type),
}

#[derive(Debug, Clone)]
struct Constraint {
    span: Span,
    left: TypeRef,
    right: TypeRef,
}

fn contains_infer(ty: &Type) -> bool {
    match ty {
        Type::Infer => true,
        Type::Optional(inner) => contains_infer(inner),
        Type::Func { params, ret } => params.iter().any(contains_infer) || contains_infer(ret),
        _ => false,
    }
}

fn is_assignable(from: &Type, to: &Type) -> bool {
    use Type::*;

    // same type is always assignable
    let is_same_type = from == to;
    if is_same_type {
        return true;
    }

    // if either side is Infer, we need to unify them
    let needs_inference = matches!(from, Infer) || matches!(to, Infer);
    if needs_inference {
        return true;
    }

    match (from, to) {
        // optional types needs to check the inner types
        (Optional(inner_from), Optional(inner_to)) => is_assignable(inner_from, inner_to),

        // T to T? is assignable
        (from_ty, Optional(inner_to)) if !matches!(from_ty, Optional(_)) => {
            is_assignable(from_ty, inner_to)
        }

        // function types needs to check the signature (params + return type)
        (
            Func {
                params: params_from,
                ret: ret_from,
            },
            Func {
                params: params_to,
                ret: ret_to,
            },
        ) => {
            params_from.len() == params_to.len()
                && params_from
                    .iter()
                    .zip(params_to.iter())
                    .all(|(pf, pt)| is_assignable(pf, pt))
                && is_assignable(ret_from, ret_to)
        }

        // T? to T is not assignable, the value must be unwrapped first
        (Optional(_), non_opt) if !matches!(non_opt, Optional(_)) => false,

        // anything else is just not assignable
        _ => false,
    }
}

fn unify_types(left: &Type, right: &Type, span: Span, errors: &mut Vec<TypeErr>) -> Option<Type> {
    use Type::*;

    // same type, no need to unify
    if left == right {
        return Some(left.clone());
    }

    match (left, right) {
        // if either side is Infer we use the concrete side
        (Infer, t) | (t, Infer) => Some(t.clone()),

        // optional types needs to unify the inner types
        (Optional(l), Optional(r)) => {
            unify_types(l, r, span, errors).map(|inner| Optional(Box::new(inner)))
        }

        // function types needs to unify the params and return type
        (
            Func {
                params: lp,
                ret: lr,
            },
            Func {
                params: rp,
                ret: rr,
            },
        ) => {
            if lp.len() != rp.len() {
                errors.push(TypeErr {
                    span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left.clone(),
                        found: right.clone(),
                    },
                });
                return None;
            }

            let mut new_params = Vec::with_capacity(lp.len());
            for (lpi, rpi) in lp.iter().zip(rp.iter()) {
                unify_types(lpi, rpi, span, errors).map(|p| new_params.push(p))?;
            }

            unify_types(lr, rr, span, errors).map(|new_ret| Func {
                params: new_params,
                ret: Box::new(new_ret),
            })
        }

        // mismatched types report an error
        (l, r) => {
            errors.push(TypeErr {
                span,
                kind: TypeErrKind::MismatchedTypes {
                    expected: l.clone(),
                    found: r.clone(),
                },
            });
            None
        }
    }
}

fn unify_equal(
    tcx: &mut TypeChecker,
    span: Span,
    left: &TypeRef,
    right: &TypeRef,
    errors: &mut Vec<TypeErr>,
) -> bool {
    let (Some(lt), Some(rt)) = (tcx.get_type_ref(left), tcx.get_type_ref(right)) else {
        return false;
    };

    match unify_types(&lt, &rt, span, errors) {
        Some(new_ty) => {
            tcx.set_type_ref(left, new_ty.clone(), span);
            tcx.set_type_ref(right, new_ty, span);
            true
        }
        None => false,
    }
}

fn resolve_constraints(type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    // keep going until we make no progress infering types
    loop {
        if !resolve_constraints_pass(type_checker, errors) {
            break;
        }
    }
}

fn resolve_constraints_pass(type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) -> bool {
    let mut made_progress = false;

    let constraints = std::mem::take(&mut type_checker.constraints);
    for c in constraints {
        let unified = unify_equal(type_checker, c.span, &c.left, &c.right, errors);
        if !unified {
            type_checker.constraints.push(c);
        }

        // if unified just set made_progress to true otherwise keep it false
        made_progress |= unified;
    }

    made_progress
}

fn check_block_stmts(
    stmts: &[StmtNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) {
    type_checker.push_scope();
    collect_scope_types(stmts, type_checker);

    for stmt in stmts {
        check_stmt(&stmt.node, type_checker, errors);
    }
    type_checker.pop_scope();
}

fn collect_scope_types(stmts: &[StmtNode], type_checker: &mut TypeChecker) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Func(node) => {
                let func = &node.node;
                type_checker.set_var(func.name, type_from_fn(func));
            }

            // TODO: consts, structs, type alias, anything that we need to collect first goes here
            _ => {}
        }
    }
}

fn type_from_fn(func: &Func) -> Type {
    Type::Func {
        params: func.params.iter().map(|param| param.ty.clone()).collect(),
        ret: Box::new(func.ret.clone()),
    }
}

fn type_from_lit(lit: &Lit) -> Type {
    match lit {
        Lit::Int(_) => Type::Int,
        Lit::Float(_) => Type::Float,
        Lit::Bool(_) => Type::Bool,
        Lit::String(_) => Type::String,
        Lit::Nil => Type::Optional(Box::new(Type::Infer)),
    }
}

fn check_stmt(stmt: &Stmt, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    match stmt {
        Stmt::Func(node) => check_func(node, type_checker, errors),
        Stmt::Expr(node) => {
            let _ = check_expr(node, type_checker, errors);
        }
        Stmt::Binding(node) => check_binding(node, type_checker, errors),
        Stmt::Return(node) => check_ret(node, type_checker, errors),
    }
}

fn check_func(fn_node: &FuncNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    let func = &fn_node.node;

    let Some(ty) = type_checker.get_var(func.name) else {
        errors.push(TypeErr {
            span: fn_node.span,
            kind: TypeErrKind::UnknownFunction { name: func.name },
        });

        return;
    };

    if !matches!(ty, Type::Func { .. }) {
        errors.push(TypeErr {
            span: fn_node.span,
            kind: TypeErrKind::MismatchedTypes {
                expected: type_from_fn(func),
                found: ty.clone(),
            },
        });

        return;
    }

    // new scope so we can add the parameters to the scope
    // internally for the body a new scope will be pushed but I guess it's fine
    type_checker.push_scope();
    type_checker.push_return_type(func.ret.clone());

    for param in &func.params {
        type_checker.set_var(param.name, param.ty.clone());
    }
    check_block_stmts(&func.body.node.stmts, type_checker, errors);

    type_checker.pop_return_type();
    type_checker.pop_scope();
}

fn check_expr(
    expr_node: &ExprNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let expr = &expr_node.node;
    let ty = match &expr.kind {
        ExprKind::Ident(ident) => match type_checker.get_var(*ident) {
            Some(ty) => ty.clone(),
            None => {
                errors.push(TypeErr {
                    span: expr_node.span,
                    kind: TypeErrKind::UnknownVariable { name: *ident },
                });
                Type::Infer
            }
        },
        ExprKind::Block(spanned) => {
            // FIXME: we need to return type for if, blocks, etc...
            check_block_stmts(&spanned.node.stmts, type_checker, errors);
            Type::Void
        }
        ExprKind::Lit(lit) => type_from_lit(lit),
        ExprKind::Call(call) => check_call(call, type_checker, errors),
        ExprKind::Binary(bin) => check_binary(bin, type_checker, errors),
        ExprKind::Unary(unary) => check_unary(unary, type_checker, errors),
        ExprKind::Assign(assign) => check_assign(assign, type_checker, errors),
    };

    type_checker.set_type(expr_node.node.id, ty.clone(), expr_node.span);
    ty
}

fn check_call(call: &CallNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) -> Type {
    let node = &call.node;
    let func_ty = check_expr(&node.func, type_checker, errors);
    match func_ty.clone() {
        Type::Func { params, ret } => {
            let same_params_len = params.len() == node.args.len();
            if !same_params_len {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: Type::Func {
                            params: params.clone(),
                            ret: ret.clone(),
                        },
                        found: func_ty,
                    },
                });
                return Type::Infer;
            }

            for (arg_expr, param_ty) in node.args.iter().zip(params.iter()) {
                check_expr(arg_expr, type_checker, errors);
                let arg_ref = TypeRef::Expr(arg_expr.node.id);
                let param_ref = TypeRef::Concrete(param_ty.clone());
                type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
            }

            *ret
        }
        _ => {
            errors.push(TypeErr {
                span: call.span,
                kind: TypeErrKind::NotAFunction { expr_type: func_ty },
            });
            Type::Infer
        }
    }
}

fn check_binary(
    bin: &BinaryNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    use BinaryOp::*;

    let node = &bin.node;
    let left_ty = check_expr(&node.left, type_checker, errors);
    let right_ty = check_expr(&node.right, type_checker, errors);
    let same_ty = left_ty == right_ty;

    match node.op {
        // numeric ops
        Add | Sub | Mul | Div | Rem => {
            if left_ty.is_num() && same_ty {
                left_ty
            } else {
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left_ty.clone(),
                        found: right_ty.clone(),
                    },
                });
                Type::Infer
            }
        }

        // equal ops must be the same type
        Eq | NotEq => {
            if same_ty {
                Type::Bool
            } else {
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left_ty.clone(),
                        found: right_ty.clone(),
                    },
                });
                Type::Bool
            }
        }

        // comparison ops must be numeric
        LessThan | GreaterThan | LessThanEq | GreaterThanEq => {
            if left_ty.is_num() && same_ty {
                Type::Bool
            } else {
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left_ty.clone(),
                        found: right_ty.clone(),
                    },
                });
                Type::Infer
            }
        }

        // logical ops must be bool
        And | Or | Xor => {
            if left_ty.is_bool() && same_ty {
                Type::Bool
            } else {
                let wrong_ty = if !left_ty.is_bool() {
                    left_ty
                } else {
                    right_ty
                };
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::InvalidOperand {
                        op: node.op.to_string(),
                        operand_type: wrong_ty,
                    },
                });
                Type::Infer
            }
        }

        Coalesce => check_coalesce(bin, left_ty, right_ty, type_checker, errors),
    }
}

fn check_coalesce(
    bin: &BinaryNode,
    left_ty: Type,
    right_ty: Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &bin.node;

    // left must be optional
    let Type::Optional(left_inner) = left_ty.clone() else {
        errors.push(TypeErr {
            span: bin.span,
            kind: TypeErrKind::InvalidOperand {
                op: node.op.to_string(),
                operand_type: left_ty,
            },
        });
        return Type::Infer;
    };

    let right_ref = TypeRef::Expr(node.right.node.id);
    let left_inner_ty = *left_inner;

    // if right is optional too then we're chaining optionals
    if let Type::Optional(right_inner) = right_ty.clone() {
        // constrain the inner types if both are optional
        let left_inner_ref = TypeRef::Concrete(left_inner_ty.clone());
        let right_inner_ref = TypeRef::Concrete(*right_inner.clone());
        type_checker.constrain_equal(bin.span, left_inner_ref, right_inner_ref, errors);

        // get the unified inner type
        let unified_inner = type_checker
            .get_type_ref(&right_ref)
            .and_then(|t| {
                if let Type::Optional(inner) = t {
                    Some(*inner)
                } else {
                    None
                }
            })
            .unwrap_or(left_inner_ty.clone());

        // set the left expression's type to the unified inner type
        let ty = Type::Optional(Box::new(unified_inner));
        type_checker.set_type(node.left.node.id, ty.clone(), bin.span);

        return ty;
    }

    // if right side is not optional then we're unwrapping or returning the right side
    let left_inner_ref = TypeRef::Concrete(left_inner_ty.clone());
    type_checker.constrain_equal(bin.span, left_inner_ref, right_ref.clone(), errors);

    // get the unified inner type
    let unified_inner = type_checker
        .get_type_ref(&right_ref)
        .unwrap_or(left_inner_ty);

    // set the left expression's type to the unified inner type
    type_checker.set_type(
        node.left.node.id,
        Type::Optional(Box::new(unified_inner.clone())),
        bin.span,
    );

    unified_inner
}

fn check_unary(
    unary: &UnaryNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &unary.node;
    let expr_ty = check_expr(&node.expr, type_checker, errors);

    match node.op {
        UnaryOp::Neg if expr_ty.is_num() => expr_ty,
        UnaryOp::Not if expr_ty.is_bool() => Type::Bool,
        _ => {
            errors.push(TypeErr {
                span: unary.span,
                kind: TypeErrKind::InvalidOperand {
                    op: node.op.to_string(),
                    operand_type: expr_ty.clone(),
                },
            });
            Type::Infer
        }
    }
}

fn check_assign(
    assign: &AssignNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &assign.node;
    check_expr(&node.target, type_checker, errors);
    check_expr(&node.value, type_checker, errors);

    let target_ref = TypeRef::Expr(node.target.node.id);
    let value_ref = TypeRef::Expr(node.value.node.id);

    match node.op {
        AssignOp::Assign => check_assign_op(assign, target_ref, value_ref, type_checker, errors),
        AssignOp::AddAssign | AssignOp::SubAssign | AssignOp::MulAssign | AssignOp::DivAssign => {
            check_compound_assign_op(assign, target_ref, value_ref, type_checker, errors)
        }
    }
}

fn check_assign_op(
    assign: &AssignNode,
    target_ref: TypeRef,
    value_ref: TypeRef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    type_checker.constrain_assignable(assign.span, value_ref, target_ref.clone(), errors);
    type_checker
        .get_type_ref(&target_ref)
        .unwrap_or(Type::Infer)
}

fn check_compound_assign_op(
    assign: &AssignNode,
    target_ref: TypeRef,
    value_ref: TypeRef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    type_checker.constrain_equal(assign.span, target_ref.clone(), value_ref, errors);

    let target_ty = type_checker
        .get_type_ref(&target_ref)
        .unwrap_or(Type::Infer);

    let is_numeric = target_ty.is_num() || target_ty.is_unresolved();
    if !is_numeric {
        errors.push(TypeErr {
            span: assign.span,
            kind: TypeErrKind::InvalidOperand {
                op: assign.node.op.to_string(),
                operand_type: target_ty.clone(),
            },
        });
    }

    target_ty
}

fn check_binding(binding: &BindingNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    let node = &binding.node;
    check_expr(&node.value, type_checker, errors);

    let val_ref = TypeRef::Expr(node.value.node.id);

    // if there is no type annotation then we just constrain the variable to the value
    let Some(annot_ty) = &node.ty else {
        let var_ref = TypeRef::Var(node.name);
        type_checker.constrain_equal(binding.span, val_ref, var_ref, errors);
        return;
    };

    // if there is a type annotation then we use constrain_assignable to check
    // if the value is assignable to the annotation (handles T -> T?, inference, etc.)
    let annot_ref = TypeRef::Concrete(annot_ty.clone());
    type_checker.constrain_assignable(binding.span, val_ref, annot_ref, errors);
    type_checker.set_var(node.name, annot_ty.clone());
}

fn check_ret(ret: &ReturnNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    let node = &ret.node;

    // if return is outside a function then we just return (although this shouldn't happen)
    let Some(expected_ret) = type_checker.current_return_type().cloned() else {
        return;
    };

    match (&node.value, &expected_ret) {
        // returning a value in a non-void fn needs constraining
        (Some(value_expr), expected_ty) => {
            check_expr(value_expr, type_checker, errors);
            let expr_ref = TypeRef::Expr(value_expr.node.id);
            let ret_ref = TypeRef::Concrete(expected_ty.clone());
            type_checker.constrain_assignable(ret.span, expr_ref, ret_ref, errors);
        }

        // returning nothing in a void fn is fine
        (None, Type::Void) => {}

        // returning nothing in a non-void fn is invalid
        (None, expected_ty) => {
            errors.push(TypeErr {
                span: ret.span,
                kind: TypeErrKind::MismatchedTypes {
                    expected: expected_ty.clone(),
                    found: Type::Void,
                },
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Assign, Binary, Binding, Block, BlockNode, Call, Expr, Func, Mutability, Param, Return,
        Unary, Visibility,
    };
    use crate::span::Span;
    use internment::Intern;
    use std::cell::Cell;

    thread_local! {
        static EXPR_ID_COUNTER: Cell<u64> = Cell::new(0);
    }

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    fn dummy_ident(s: &str) -> Ident {
        Ident(Intern::new(s.to_string()))
    }

    // reset the expression id counter for deterministic test ids
    fn reset_expr_ids() {
        EXPR_ID_COUNTER.with(|counter| counter.set(0));
    }

    fn next_expr_id() -> ExprId {
        EXPR_ID_COUNTER.with(|counter| {
            let id = counter.get();
            counter.set(id + 1);
            ExprId(id)
        })
    }

    // ---- ast builder helpers ----
    fn lit_int(val: i64) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Int(val)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_float(val: f64) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Float(val)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_bool(val: bool) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Bool(val)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_string(val: &str) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::String(val.to_string())), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_nil() -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Nil), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn ident_expr(name: &str) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Ident(dummy_ident(name)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn binary_expr(left: ExprNode, op: BinaryOp, right: ExprNode) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Binary(BinaryNode {
                    node: Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn unary_expr(op: UnaryOp, expr: ExprNode) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Unary(UnaryNode {
                    node: Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn call_expr(func: ExprNode, args: Vec<ExprNode>) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Call(CallNode {
                    node: Call {
                        func: Box::new(func),
                        args,
                        type_args: vec![],
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn assign_expr(target: ExprNode, op: AssignOp, value: ExprNode) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Assign(AssignNode {
                    node: Assign {
                        target: Box::new(target),
                        op,
                        value: Box::new(value),
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn let_binding(name: &str, ty: Option<Type>, value: ExprNode) -> StmtNode {
        StmtNode {
            node: Stmt::Binding(BindingNode {
                node: Binding {
                    name: dummy_ident(name),
                    ty,
                    mutability: Mutability::Immutable,
                    value,
                },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn var_binding(name: &str, ty: Option<Type>, value: ExprNode) -> StmtNode {
        StmtNode {
            node: Stmt::Binding(BindingNode {
                node: Binding {
                    name: dummy_ident(name),
                    ty,
                    mutability: Mutability::Mutable,
                    value,
                },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn fn_decl(name: &str, params: Vec<(&str, Type)>, ret: Type, body: Vec<StmtNode>) -> StmtNode {
        StmtNode {
            node: Stmt::Func(FuncNode {
                node: Func {
                    name: dummy_ident(name),
                    visibility: Visibility::Private,
                    params: params
                        .into_iter()
                        .map(|(n, t)| Param {
                            name: dummy_ident(n),
                            ty: t,
                        })
                        .collect(),
                    ret,
                    body: BlockNode {
                        node: Block { stmts: body },
                        span: dummy_span(),
                    },
                },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn return_stmt(value: Option<ExprNode>) -> StmtNode {
        StmtNode {
            node: Stmt::Return(ReturnNode {
                node: Return { value },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn expr_stmt(expr: ExprNode) -> StmtNode {
        StmtNode {
            node: Stmt::Expr(expr),
            span: dummy_span(),
        }
    }

    fn program(stmts: Vec<StmtNode>) -> Program {
        Program { stmts }
    }

    // ---- runner helpers ----
    fn run_ok(prog: Program) -> TypeChecker {
        match check_program(&prog) {
            Ok(tcx) => tcx,
            Err(errors) => {
                panic!("Expected Ok, got errors: {:?}", errors);
            }
        }
    }

    fn run_err(prog: Program) -> Vec<TypeErr> {
        match check_program(&prog) {
            Ok(_) => panic!("Expected Err, got Ok"),
            Err(errors) => errors,
        }
    }

    // ---- assertion helpers ----
    fn assert_expr_type(tcx: &TypeChecker, id: ExprId, expected: Type) {
        match tcx.get_type(id) {
            Some((_, ty)) => assert_eq!(
                *ty, expected,
                "Expression {:?} has wrong type. Expected {:?}, got {:?}",
                id, expected, ty
            ),
            None => panic!("Expression {:?} not found in type map", id),
        }
    }

    fn get_expr_id(expr: &ExprNode) -> ExprId {
        expr.node.id
    }

    #[test]
    fn test_unify_primitives() {
        let span = dummy_span();
        let mut errors = vec![];

        // int unifies with int
        let result = unify_types(&Type::Int, &Type::Int, span, &mut errors);
        assert_eq!(result, Some(Type::Int));
        assert_eq!(errors.len(), 0);

        // float unifies with float
        let result = unify_types(&Type::Float, &Type::Float, span, &mut errors);
        assert_eq!(result, Some(Type::Float));
        assert_eq!(errors.len(), 0);

        // bool unifies with bool
        let result = unify_types(&Type::Bool, &Type::Bool, span, &mut errors);
        assert_eq!(result, Some(Type::Bool));
        assert_eq!(errors.len(), 0);

        // string unifies with string
        let result = unify_types(&Type::String, &Type::String, span, &mut errors);
        assert_eq!(result, Some(Type::String));
        assert_eq!(errors.len(), 0);

        // void unifies with void
        let result = unify_types(&Type::Void, &Type::Void, span, &mut errors);
        assert_eq!(result, Some(Type::Void));
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_unify_infer_with_concrete() {
        let span = dummy_span();
        let mut errors = vec![];

        // infer unifies with int (both directions)
        let result = unify_types(&Type::Infer, &Type::Int, span, &mut errors);
        assert_eq!(result, Some(Type::Int));
        assert_eq!(errors.len(), 0);

        let result = unify_types(&Type::Int, &Type::Infer, span, &mut errors);
        assert_eq!(result, Some(Type::Int));
        assert_eq!(errors.len(), 0);

        // infer unifies with optional(int)
        let result = unify_types(
            &Type::Infer,
            &Type::Optional(Box::new(Type::Int)),
            span,
            &mut errors,
        );
        assert_eq!(result, Some(Type::Optional(Box::new(Type::Int))));
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_unify_optional() {
        let span = dummy_span();
        let mut errors = vec![];

        // int? unifies with int?
        let result = unify_types(
            &Type::Optional(Box::new(Type::Int)),
            &Type::Optional(Box::new(Type::Int)),
            span,
            &mut errors,
        );
        assert_eq!(result, Some(Type::Optional(Box::new(Type::Int))));
        assert_eq!(errors.len(), 0);

        // infer? unifies with string?
        let result = unify_types(
            &Type::Optional(Box::new(Type::Infer)),
            &Type::Optional(Box::new(Type::String)),
            span,
            &mut errors,
        );
        assert_eq!(result, Some(Type::Optional(Box::new(Type::String))));
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_unify_function_types() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn(int, bool) -> float unifies with identical signature
        let func_type = Type::Func {
            params: vec![Type::Int, Type::Bool],
            ret: Box::new(Type::Float),
        };
        let result = unify_types(&func_type, &func_type, span, &mut errors);
        assert_eq!(result, Some(func_type.clone()));
        assert_eq!(errors.len(), 0);

        // parameter length mismatch produces error
        let func1 = Type::Func {
            params: vec![Type::Int],
            ret: Box::new(Type::Void),
        };
        let func2 = Type::Func {
            params: vec![Type::Int, Type::Bool],
            ret: Box::new(Type::Void),
        };
        let result = unify_types(&func1, &func2, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::MismatchedTypes { .. }
        ));
    }

    #[test]
    fn test_unify_mismatched_types() {
        let span = dummy_span();
        let mut errors = vec![];

        // int vs bool produces error
        let result = unify_types(&Type::Int, &Type::Bool, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Bool
        ));

        // optional vs non-optional produces error
        errors.clear();
        let result = unify_types(
            &Type::Optional(Box::new(Type::Int)),
            &Type::Int,
            span,
            &mut errors,
        );
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_binding_annotated_success() {
        reset_expr_ids();

        // let x: int = 1;
        let value_expr = lit_int(1);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding("x", Some(Type::Int), value_expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_binding_annotated_mismatch() {
        reset_expr_ids();

        // let x: int = true;
        let prog = program(vec![let_binding("x", Some(Type::Int), lit_bool(true))]);

        let errors = run_err(prog);
        // should have at least one mismatched types error between int and bool
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Int && *found == Type::Bool) ||
                   (*expected == Type::Bool && *found == Type::Int)
            )),
            "Expected MismatchedTypes error (int/bool mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_binding_unannotated_simple_inference() {
        reset_expr_ids();

        // let x = 1;
        let value_expr = lit_int(1);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding("x", None, value_expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_binding_unannotated_unresolved_infer() {
        reset_expr_ids();

        // let x = nil; (no other uses)
        let prog = program(vec![let_binding("x", None, lit_nil())]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::UnresolvedInfer))
        );
    }

    #[test]
    fn test_binding_chained_inference() {
        reset_expr_ids();

        // let x: int = 1; let y = x;
        // first binding needs type annotation so x is in scope for second binding
        let x_val_expr = lit_int(1);
        let x_val_id = get_expr_id(&x_val_expr);
        let y_val_expr = ident_expr("x");
        let y_val_id = get_expr_id(&y_val_expr);
        let prog = program(vec![
            let_binding("x", Some(Type::Int), x_val_expr),
            let_binding("y", None, y_val_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, x_val_id, Type::Int);
        assert_expr_type(&tcx, y_val_id, Type::Int);
    }

    #[test]
    fn test_call_happy_path() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f(1, true);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let call_expr_node = call_expr(ident_expr("f"), vec![lit_int(1), lit_bool(true)]);
        let call_id = get_expr_id(&call_expr_node);
        let prog = program(vec![fn_def, expr_stmt(call_expr_node)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::String);
    }

    #[test]
    fn test_call_arity_mismatch_too_few() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f(1);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let prog = program(vec![
            fn_def,
            expr_stmt(call_expr(ident_expr("f"), vec![lit_int(1)])),
        ]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_call_arity_mismatch_too_many() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f(1, true, 3);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let prog = program(vec![
            fn_def,
            expr_stmt(call_expr(
                ident_expr("f"),
                vec![lit_int(1), lit_bool(true), lit_int(3)],
            )),
        ]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_call_argument_type_mismatch() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f("nope", true);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let prog = program(vec![
            fn_def,
            expr_stmt(call_expr(
                ident_expr("f"),
                vec![lit_string("nope"), lit_bool(true)],
            )),
        ]);

        let errors = run_err(prog);
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Int && *found == Type::String) ||
                   (*expected == Type::String && *found == Type::Int)
            )),
            "Expected MismatchedTypes error (int/string mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_return_void_function_ok() {
        reset_expr_ids();
        // fn main() { return; }
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![return_stmt(None)],
        )]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_return_void_function_returning_value() {
        reset_expr_ids();
        // fn main() { return 1; }
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![return_stmt(Some(lit_int(1)))],
        )]);

        let errors = run_err(prog);
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Void && *found == Type::Int) ||
                   (*expected == Type::Int && *found == Type::Void)
            )),
            "Expected MismatchedTypes error (void/int mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_return_non_void_function_correct() {
        reset_expr_ids();
        // fn f() -> int { return 1; }
        let value_expr = lit_int(1);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![return_stmt(Some(value_expr))],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_return_non_void_wrong_type() {
        reset_expr_ids();
        // fn f() -> int { return true; }
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![return_stmt(Some(lit_bool(true)))],
        )]);

        let errors = run_err(prog);
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Int && *found == Type::Bool) ||
                   (*expected == Type::Bool && *found == Type::Int)
            )),
            "Expected MismatchedTypes error (int/bool mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_return_non_void_without_value() {
        reset_expr_ids();
        // fn f() -> int { return; }
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![return_stmt(None)],
        )]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Void
        )));
    }

    #[test]
    fn test_coalesce_optional_with_concrete_fallback() {
        reset_expr_ids();
        // let a: int? = nil;
        // let x: int = a ?? 10;
        let a_expr = lit_nil();
        let a_binding = let_binding("a", Some(Type::Optional(Box::new(Type::Int))), a_expr);
        let coalesce_expr = binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_int(10));
        let coalesce_id = get_expr_id(&coalesce_expr);
        let x_binding = let_binding("x", Some(Type::Int), coalesce_expr);
        let prog = program(vec![a_binding, x_binding]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, coalesce_id, Type::Int);
    }

    #[test]
    fn test_coalesce_non_optional_left_error() {
        reset_expr_ids();
        // let x = 10 ?? 20;
        let coalesce_expr = binary_expr(lit_int(10), BinaryOp::Coalesce, lit_int(20));
        let prog = program(vec![let_binding("x", None, coalesce_expr)]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { op, operand_type }
            if op == "??" && *operand_type == Type::Int
        )));
    }

    #[test]
    fn test_coalesce_mismatched_types() {
        reset_expr_ids();
        // let x: int? = nil;
        // let y = x ?? "s"; // int? ?? string should error
        let x_binding = let_binding("x", Some(Type::Optional(Box::new(Type::Int))), lit_nil());
        let coalesce_expr = binary_expr(ident_expr("x"), BinaryOp::Coalesce, lit_string("s"));
        let y_binding = let_binding("y", None, coalesce_expr);
        let prog = program(vec![x_binding, y_binding]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::String
        )));
    }

    #[test]
    fn test_assignment_plain_ok() {
        reset_expr_ids();
        // var x: int = 1; x = 2;
        let assign_expr = assign_expr(ident_expr("x"), AssignOp::Assign, lit_int(2));
        let assign_id = get_expr_id(&assign_expr);
        let prog = program(vec![
            var_binding("x", Some(Type::Int), lit_int(1)),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, assign_id, Type::Int);
    }

    #[test]
    fn test_assignment_plain_mismatch() {
        reset_expr_ids();
        // var x: int = 1; x = true;
        let prog = program(vec![
            var_binding("x", Some(Type::Int), lit_int(1)),
            expr_stmt(assign_expr(
                ident_expr("x"),
                AssignOp::Assign,
                lit_bool(true),
            )),
        ]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Bool
        )));
    }

    #[test]
    fn test_assignment_compound_ok() {
        reset_expr_ids();
        // var x: int = 1; x += 2;
        let assign_expr = assign_expr(ident_expr("x"), AssignOp::AddAssign, lit_int(2));
        let assign_id = get_expr_id(&assign_expr);
        let prog = program(vec![
            var_binding("x", Some(Type::Int), lit_int(1)),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, assign_id, Type::Int);
    }

    #[test]
    fn test_assignment_compound_non_numeric() {
        reset_expr_ids();
        // var x: string = "a"; x += 1;
        let prog = program(vec![
            var_binding("x", Some(Type::String), lit_string("a")),
            expr_stmt(assign_expr(
                ident_expr("x"),
                AssignOp::AddAssign,
                lit_int(1),
            )),
        ]);

        let errors = run_err(prog);
        // should get either InvalidOperand or MismatchedTypes
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { .. } | TypeErrKind::MismatchedTypes { .. }
        )));
    }

    #[test]
    fn test_binary_arithmetic_int() {
        reset_expr_ids();
        // 1 + 2
        let expr = binary_expr(lit_int(1), BinaryOp::Add, lit_int(2));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Int);
    }

    #[test]
    fn test_binary_arithmetic_float() {
        reset_expr_ids();
        // 1.0 + 2.0
        let expr = binary_expr(lit_float(1.0), BinaryOp::Add, lit_float(2.0));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Float);
    }

    #[test]
    fn test_binary_arithmetic_mismatch() {
        reset_expr_ids();
        // 1 + true
        let prog = program(vec![expr_stmt(binary_expr(
            lit_int(1),
            BinaryOp::Add,
            lit_bool(true),
        ))]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_binary_logical_ok() {
        reset_expr_ids();
        // true && false
        let expr = binary_expr(lit_bool(true), BinaryOp::And, lit_bool(false));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Bool);
    }

    #[test]
    fn test_binary_logical_invalid_operand() {
        reset_expr_ids();
        // 1 && 2
        let prog = program(vec![expr_stmt(binary_expr(
            lit_int(1),
            BinaryOp::And,
            lit_int(2),
        ))]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { operand_type, .. }
            if *operand_type == Type::Int
        )));
    }

    #[test]
    fn test_binary_comparison_ok() {
        reset_expr_ids();
        // 1 < 2
        let expr = binary_expr(lit_int(1), BinaryOp::LessThan, lit_int(2));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Bool);
    }

    #[test]
    fn test_binary_comparison_mismatch() {
        reset_expr_ids();
        // 1 < true
        let prog = program(vec![expr_stmt(binary_expr(
            lit_int(1),
            BinaryOp::LessThan,
            lit_bool(true),
        ))]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_unary_neg_int() {
        reset_expr_ids();
        // -1
        let expr = unary_expr(UnaryOp::Neg, lit_int(1));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Int);
    }

    #[test]
    fn test_unary_neg_float() {
        reset_expr_ids();
        // -1.0
        let expr = unary_expr(UnaryOp::Neg, lit_float(1.0));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Float);
    }

    #[test]
    fn test_unary_not_bool() {
        reset_expr_ids();
        // !true
        let expr = unary_expr(UnaryOp::Not, lit_bool(true));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Bool);
    }

    #[test]
    fn test_unary_neg_invalid() {
        reset_expr_ids();
        // -true
        let prog = program(vec![expr_stmt(unary_expr(UnaryOp::Neg, lit_bool(true)))]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { operand_type, .. }
            if *operand_type == Type::Bool
        )));
    }

    #[test]
    fn test_unary_not_invalid() {
        reset_expr_ids();
        // !1
        let prog = program(vec![expr_stmt(unary_expr(UnaryOp::Not, lit_int(1)))]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { operand_type, .. }
            if *operand_type == Type::Int
        )));
    }

    #[test]
    fn test_constraint_chain_resolves() {
        reset_expr_ids();
        // let a: int? = nil; let b: int? = a;
        let a_expr = lit_nil();
        let a_id = get_expr_id(&a_expr);
        let b_expr = ident_expr("a");
        let b_id = get_expr_id(&b_expr);
        let prog = program(vec![
            let_binding("a", Some(Type::Optional(Box::new(Type::Int))), a_expr),
            let_binding("b", Some(Type::Optional(Box::new(Type::Int))), b_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, a_id, Type::Optional(Box::new(Type::Int)));
        assert_expr_type(&tcx, b_id, Type::Optional(Box::new(Type::Int)));
    }

    #[test]
    fn test_leftover_infer() {
        reset_expr_ids();
        // let a = nil;
        let prog = program(vec![let_binding("a", None, lit_nil())]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::UnresolvedInfer))
        );
    }

    #[test]
    fn test_constraint_through_function_call() {
        reset_expr_ids();
        // fn id(x: int) -> int { return x; }
        // let a: int = id(1);
        let fn_def = fn_decl(
            "id",
            vec![("x", Type::Int)],
            Type::Int,
            vec![return_stmt(Some(ident_expr("x")))],
        );
        let a_val = call_expr(ident_expr("id"), vec![lit_int(1)]);
        let a_val_id = get_expr_id(&a_val);
        let prog = program(vec![fn_def, let_binding("a", Some(Type::Int), a_val)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, a_val_id, Type::Int);
    }

    #[test]
    fn test_function_as_value() {
        reset_expr_ids();
        // fn f(a: int) -> int { return a; }
        // let g: fn(int) -> int = f;
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int)],
            Type::Int,
            vec![return_stmt(Some(ident_expr("a")))],
        );
        let g_val = ident_expr("f");
        let g_val_id = get_expr_id(&g_val);
        let expected_fn_type = Type::Func {
            params: vec![Type::Int],
            ret: Box::new(Type::Int),
        };
        let prog = program(vec![
            fn_def,
            let_binding("g", Some(expected_fn_type.clone()), g_val),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, g_val_id, expected_fn_type);
    }

    #[test]
    fn test_function_call_through_variable() {
        reset_expr_ids();
        // fn f(a: int) -> int { return a; }
        // let g: fn(int) -> int = f;
        // g(42);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int)],
            Type::Int,
            vec![return_stmt(Some(ident_expr("a")))],
        );
        let fn_type = Type::Func {
            params: vec![Type::Int],
            ret: Box::new(Type::Int),
        };
        let g_binding = let_binding("g", Some(fn_type), ident_expr("f"));
        let call_expr_node = call_expr(ident_expr("g"), vec![lit_int(42)]);
        let call_id = get_expr_id(&call_expr_node);
        let prog = program(vec![fn_def, g_binding, expr_stmt(call_expr_node)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::Int);
    }

    #[test]
    fn test_nested_function_scope() {
        reset_expr_ids();
        // fn outer() -> int {
        //   fn inner() -> int { return 10; }
        //   return inner();
        // }
        let inner_fn = fn_decl(
            "inner",
            vec![],
            Type::Int,
            vec![return_stmt(Some(lit_int(10)))],
        );
        let call_inner = call_expr(ident_expr("inner"), vec![]);
        let outer_fn = fn_decl(
            "outer",
            vec![],
            Type::Int,
            vec![inner_fn, return_stmt(Some(call_inner))],
        );
        let prog = program(vec![outer_fn]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_function_forward_reference() {
        reset_expr_ids();
        // fn a() -> int { return b(); }
        // fn b() -> int { return 1; }
        let a_fn = fn_decl(
            "a",
            vec![],
            Type::Int,
            vec![return_stmt(Some(call_expr(ident_expr("b"), vec![])))],
        );
        let b_fn = fn_decl("b", vec![], Type::Int, vec![return_stmt(Some(lit_int(1)))]);
        let prog = program(vec![a_fn, b_fn]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_function_mutual_recursion() {
        reset_expr_ids();
        // fn even(n: int) -> bool {
        //   return odd(n);
        // }
        // fn odd(n: int) -> bool {
        //   return even(n);
        // }
        let even_fn = fn_decl(
            "even",
            vec![("n", Type::Int)],
            Type::Bool,
            vec![return_stmt(Some(call_expr(
                ident_expr("odd"),
                vec![ident_expr("n")],
            )))],
        );
        let odd_fn = fn_decl(
            "odd",
            vec![("n", Type::Int)],
            Type::Bool,
            vec![return_stmt(Some(call_expr(
                ident_expr("even"),
                vec![ident_expr("n")],
            )))],
        );
        let prog = program(vec![even_fn, odd_fn]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_assignability_int_to_optional_int() {
        reset_expr_ids();
        // let x: int? = 10;
        let value_expr = lit_int(10);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding(
            "x",
            Some(Type::Optional(Box::new(Type::Int))),
            value_expr,
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_assignability_nil_to_optional_int() {
        reset_expr_ids();
        // let x: int? = nil;
        let value_expr = lit_nil();
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding(
            "x",
            Some(Type::Optional(Box::new(Type::Int))),
            value_expr,
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Optional(Box::new(Type::Int)));
    }

    #[test]
    fn test_assignability_optional_to_non_optional_fails() {
        reset_expr_ids();
        // let a: int? = nil; let b: int = a;
        let a_expr = lit_nil();
        let b_expr = ident_expr("a");
        let prog = program(vec![
            let_binding("a", Some(Type::Optional(Box::new(Type::Int))), a_expr),
            let_binding("b", Some(Type::Int), b_expr),
        ]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Optional(Box::new(Type::Int))
        )));
    }

    #[test]
    fn test_assignment_int_to_optional_var() {
        reset_expr_ids();
        // var x: int? = nil; x = 10;
        let nil_expr = lit_nil();
        let ten_expr = lit_int(10);
        let ten_id = get_expr_id(&ten_expr);
        let assign_expr = assign_expr(ident_expr("x"), AssignOp::Assign, ten_expr);
        let prog = program(vec![
            var_binding("x", Some(Type::Optional(Box::new(Type::Int))), nil_expr),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, ten_id, Type::Int);
    }

    #[test]
    fn test_assignment_string_to_optional_string() {
        reset_expr_ids();
        // var c: string? = nil; c = "whatever";
        let nil_expr = lit_nil();
        let str_expr = lit_string("whatever");
        let str_id = get_expr_id(&str_expr);
        let assign_expr = assign_expr(ident_expr("c"), AssignOp::Assign, str_expr);
        let prog = program(vec![
            var_binding("c", Some(Type::Optional(Box::new(Type::String))), nil_expr),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, str_id, Type::String);
    }

    #[test]
    fn test_assignment_float_to_optional_float() {
        reset_expr_ids();
        // var d: float? = 10.0; d = nil;
        let float_expr = lit_float(10.0);
        let nil_expr = lit_nil();
        let assign_expr = assign_expr(ident_expr("d"), AssignOp::Assign, nil_expr);
        let prog = program(vec![
            var_binding("d", Some(Type::Optional(Box::new(Type::Float))), float_expr),
            expr_stmt(assign_expr),
        ]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_coalesce_nil_with_int() {
        reset_expr_ids();
        // let a: int = nil ?? 10;
        let nil_expr = lit_nil();
        let nil_id = get_expr_id(&nil_expr);
        let ten_expr = lit_int(10);
        let coalesce_expr = binary_expr(nil_expr, BinaryOp::Coalesce, ten_expr);
        let coalesce_id = get_expr_id(&coalesce_expr);
        let prog = program(vec![let_binding("a", Some(Type::Int), coalesce_expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, nil_id, Type::Optional(Box::new(Type::Int)));
        assert_expr_type(&tcx, coalesce_id, Type::Int);
    }

    #[test]
    fn test_coalesce_string_with_string_error() {
        reset_expr_ids();
        // let b = "nice" ?? "other";
        let nice_expr = lit_string("nice");
        let other_expr = lit_string("other");
        let coalesce_expr = binary_expr(nice_expr, BinaryOp::Coalesce, other_expr);
        let prog = program(vec![let_binding("b", None, coalesce_expr)]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { op, operand_type }
            if op == "??" && *operand_type == Type::String
        )));
    }

    #[test]
    fn test_coalesce_mismatched_inner_types() {
        reset_expr_ids();
        // let a: int? = nil ?? true;
        let nil_expr = lit_nil();
        let bool_expr = lit_bool(true);
        let coalesce_expr = binary_expr(nil_expr, BinaryOp::Coalesce, bool_expr);
        let prog = program(vec![let_binding(
            "a",
            Some(Type::Optional(Box::new(Type::Int))),
            coalesce_expr,
        )]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_coalesce_optional_string_with_string() {
        reset_expr_ids();
        // let a: string? = nil;
        // let b: string = a ?? "fallback";
        let a_expr = lit_nil();
        let a_binding = let_binding("a", Some(Type::Optional(Box::new(Type::String))), a_expr);
        let coalesce_expr =
            binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_string("fallback"));
        let coalesce_id = get_expr_id(&coalesce_expr);
        let b_binding = let_binding("b", Some(Type::String), coalesce_expr);
        let prog = program(vec![a_binding, b_binding]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, coalesce_id, Type::String);
    }

    #[test]
    fn test_coalesce_optional_int_with_float_error() {
        reset_expr_ids();
        // let a: int? = nil;
        // let b = a ?? 1.5;  // error: int? ?? float mismatch
        let a_binding = let_binding("a", Some(Type::Optional(Box::new(Type::Int))), lit_nil());
        let coalesce_expr = binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_float(1.5));
        let b_binding = let_binding("b", None, coalesce_expr);
        let prog = program(vec![a_binding, b_binding]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Float
        )));
    }

    #[test]
    fn test_multiple_optional_assignments() {
        reset_expr_ids();
        // var e: int? = nil; e = 10;
        let nil_expr = lit_nil();
        let ten_expr = lit_int(10);
        let ten_id = get_expr_id(&ten_expr);
        let assign_expr = assign_expr(ident_expr("e"), AssignOp::Assign, ten_expr);
        let prog = program(vec![
            var_binding("e", Some(Type::Optional(Box::new(Type::Int))), nil_expr),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, ten_id, Type::Int);
    }
}
