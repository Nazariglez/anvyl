use std::collections::HashMap;

use super::{ModuleScope, TypeChecker, TypeError, ValueDecl};
use crate::{
    ast::{
        BinaryOp, CastNode, ConstValue, ExprKind, ExprNode, FieldAccessNode, Ident, Lit, Program,
        Stmt, StringPart, Type, UnaryOp,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub(super) struct ConstEntry {
    ty: Option<Type>,
    value: ExprNode,
    span: Span,
    state: ConstState,
}

#[derive(Debug, Clone)]
enum ConstState {
    Unevaluated,
    Evaluating,
    Evaluated(ConstValue),
    Failed,
}

pub(super) fn const_type(value: &ConstValue) -> Type {
    match value {
        ConstValue::Int(_) => Type::Int,
        ConstValue::Float(_) => Type::Float,
        ConstValue::Bool(_) => Type::Bool,
        ConstValue::String(_) => Type::String,
    }
}

pub(super) fn const_usize(value: &ConstValue, span: Span) -> Result<usize, TypeError> {
    match value {
        ConstValue::Int(value) => {
            usize::try_from(*value).map_err(|_| TypeError::NegativeArrayLength {
                value: *value,
                span,
            })
        }
        value => Err(TypeError::ExpectedIntConst {
            found: const_type(value),
            span,
        }),
    }
}

pub(super) fn evaluated_consts(
    consts: HashMap<(ModuleScope, Ident), ConstEntry>,
) -> HashMap<(ModuleScope, Ident), ConstValue> {
    consts
        .into_iter()
        .filter_map(|(key, entry)| match entry.state {
            ConstState::Evaluated(value) => Some((key, value)),
            ConstState::Unevaluated | ConstState::Evaluating | ConstState::Failed => None,
        })
        .collect()
}

impl TypeChecker {
    pub(super) fn collect_const_decls(&mut self, module: ModuleScope, program: &Program) {
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

    pub(super) fn eval_module_consts(&mut self, module: &ModuleScope) {
        let names = self
            .consts
            .keys()
            .filter_map(|(scope, name)| (scope == module).then_some(*name))
            .collect::<Vec<_>>();
        for name in names {
            if let Err(err) = self.eval_top_const(module.clone(), name, Span::new(0, 0)) {
                self.push_error(err);
            }
        }
    }

    pub(super) fn eval_const_expr(&mut self, expr: &ExprNode) -> Result<ConstValue, TypeError> {
        match &expr.node.kind {
            ExprKind::Lit(lit) => self.eval_const_lit(lit, expr.span),
            ExprKind::Ident(name) => {
                self.eval_visible_const(*name, expr.span)
                    .unwrap_or_else(|| {
                        Err(TypeError::UnknownConst {
                            name: *name,
                            span: expr.span,
                        })
                    })
            }
            ExprKind::Unary(node) => {
                let value = self.eval_const_expr(&node.node.expr)?;
                eval_unary(node.node.op, value, node.span)
            }
            ExprKind::Binary(node) => match node.node.op {
                BinaryOp::And => {
                    let left = bool_operand(
                        node.node.op,
                        self.eval_const_expr(&node.node.left)?,
                        node.span,
                    )?;
                    if !left {
                        return Ok(ConstValue::Bool(false));
                    }
                    eval_binary(
                        node.node.op,
                        ConstValue::Bool(left),
                        self.eval_const_expr(&node.node.right)?,
                        node.span,
                    )
                }
                BinaryOp::Or => {
                    let left = bool_operand(
                        node.node.op,
                        self.eval_const_expr(&node.node.left)?,
                        node.span,
                    )?;
                    if left {
                        return Ok(ConstValue::Bool(true));
                    }
                    eval_binary(
                        node.node.op,
                        ConstValue::Bool(left),
                        self.eval_const_expr(&node.node.right)?,
                        node.span,
                    )
                }
                _ => {
                    let left = self.eval_const_expr(&node.node.left)?;
                    let right = self.eval_const_expr(&node.node.right)?;
                    eval_binary(node.node.op, left, right, node.span)
                }
            },
            ExprKind::Cast(node) => self.eval_const_cast(node),
            ExprKind::StringInterp(parts) => self.eval_const_string_interp(parts, expr.span),
            ExprKind::Field(node) => self.eval_const_field(node, expr.span),
            _ => Err(TypeError::NonConstExpression { span: expr.span }),
        }
    }

    fn eval_const_field(
        &mut self,
        node: &FieldAccessNode,
        span: Span,
    ) -> Result<ConstValue, TypeError> {
        if node.node.safe {
            return Err(TypeError::NonConstExpression { span });
        }
        let ExprKind::Ident(module) = &node.node.target.node.kind else {
            return Err(TypeError::NonConstExpression { span });
        };
        let Some(scope) = self.lookup_module_alias(*module) else {
            return Err(TypeError::NonConstExpression { span });
        };
        match self.exported_value_in_module(&scope, node.node.field) {
            Some((module, name, ValueDecl::Const(_))) => self.eval_top_const(module, name, span),
            Some((_, _, ValueDecl::Func(_))) => Err(TypeError::NonConstExpression { span }),
            None => Err(TypeError::UndefinedModuleMember {
                module: scope,
                name: node.node.field,
                span,
            }),
        }
    }

    pub(super) fn eval_visible_const(
        &mut self,
        name: Ident,
        span: Span,
    ) -> Option<Result<ConstValue, TypeError>> {
        for (index, scope) in self.scopes.iter().enumerate().rev() {
            let Some(info) = scope.get(&name) else {
                continue;
            };
            if let Some(value) = &info.const_value {
                return Some(Ok(value.clone()));
            }
            let scope_binding_blocks_const_lookup =
                index != 0 || !self.has_top_const(&self.current_module, name);
            if scope_binding_blocks_const_lookup {
                return None;
            }
            break;
        }

        if self.has_top_const(&self.current_module, name) {
            return Some(self.eval_top_const(self.current_module.clone(), name, span));
        }

        let imported = self.imported_value(name)?;
        match imported {
            (module, imported_name, ValueDecl::Const(_)) => {
                Some(self.eval_top_const(module, imported_name, span))
            }
            (_, _, ValueDecl::Func(_)) => None,
        }
    }

    fn has_top_const(&self, module: &ModuleScope, name: Ident) -> bool {
        self.consts.contains_key(&(module.clone(), name))
    }

    fn eval_top_const(
        &mut self,
        module: ModuleScope,
        name: Ident,
        span: Span,
    ) -> Result<ConstValue, TypeError> {
        let key = (module.clone(), name);
        let Some(state) = self.consts.get(&key).map(|entry| entry.state.clone()) else {
            return Err(TypeError::UnknownConst { name, span });
        };
        match state {
            ConstState::Evaluated(value) => return Ok(value),
            ConstState::Evaluating => return Err(TypeError::ConstCycle { name, span }),
            ConstState::Failed => return Err(TypeError::NonConstExpression { span }),
            ConstState::Unevaluated => {}
        }

        let (ty, value_expr, decl_span) = {
            let entry = self.consts.get_mut(&key).expect("const entry exists");
            entry.state = ConstState::Evaluating;
            (entry.ty.clone(), entry.value.clone(), entry.span)
        };

        let previous_module = std::mem::replace(&mut self.current_module, module.clone());
        let saved_scopes = (previous_module != module).then(|| std::mem::take(&mut self.scopes));
        let result = self.eval_const_expr(&value_expr).and_then(|value| {
            let value_ty = const_type(&value);
            match ty {
                Some(annot) => {
                    let expected = self.resolve_type_for_tc(&annot);
                    if expected == value_ty {
                        Ok((value, expected))
                    } else {
                        Err(TypeError::ConstTypeMismatch {
                            expected,
                            found: value_ty,
                            span: decl_span,
                        })
                    }
                }
                None => Ok((value, value_ty)),
            }
        });
        if let Some(scopes) = saved_scopes {
            self.scopes = scopes;
        }
        self.current_module = previous_module;

        match result {
            Ok((value, ty)) => {
                if let Some(entry) = self.consts.get_mut(&key) {
                    entry.state = ConstState::Evaluated(value.clone());
                }
                self.decls.set_const_type(&module, name, ty.clone());
                self.set_current_scope_const(&module, name, ty, value.clone());
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
        ty: Type,
        value: ConstValue,
    ) {
        if &self.current_module != module {
            return;
        }
        let Some(scope) = self.scopes.first_mut() else {
            return;
        };
        let Some(info) = scope.get_mut(&name) else {
            return;
        };
        let type_id = info.type_id;
        info.const_value = Some(value);
        self.solver.set_local_type_from_type(type_id, &ty);
    }

    fn eval_const_lit(&self, lit: &Lit, span: Span) -> Result<ConstValue, TypeError> {
        match lit {
            Lit::Int(value) => Ok(ConstValue::Int(*value)),
            Lit::Float(value) => Ok(ConstValue::Float(*value)),
            Lit::Bool(value) => Ok(ConstValue::Bool(*value)),
            Lit::String(value) => Ok(ConstValue::String(value.clone())),
            Lit::Nil => Err(TypeError::NonConstExpression { span }),
        }
    }

    fn eval_const_cast(&mut self, node: &CastNode) -> Result<ConstValue, TypeError> {
        let value = self.eval_const_expr(&node.node.expr)?;
        let from = const_type(&value);
        let to = self.resolve_type_for_tc(&node.node.target);
        match (value, &to) {
            (value, _) if from == to => Ok(value),
            (ConstValue::Int(value), Type::Float) => Ok(ConstValue::Float(value as f64)),
            (ConstValue::Float(value), Type::Int) => Ok(ConstValue::Int(value as i64)),
            _ => Err(TypeError::InvalidConstCast {
                from,
                to,
                span: node.span,
            }),
        }
    }

    fn eval_const_string_interp(
        &mut self,
        parts: &[StringPart],
        span: Span,
    ) -> Result<ConstValue, TypeError> {
        let mut out = String::new();
        for part in parts {
            match part {
                StringPart::Text(text) => out.push_str(text),
                StringPart::Expr(expr, _) => {
                    out.push_str(&const_string(&self.eval_const_expr(expr)?))
                }
            }
        }
        let empty_interpolation = out.is_empty() && parts.is_empty();
        if empty_interpolation {
            return Err(TypeError::NonConstExpression { span });
        }
        Ok(ConstValue::String(out))
    }
}

fn const_string(value: &ConstValue) -> String {
    match value {
        ConstValue::String(value) => value.clone(),
        ConstValue::Int(_) | ConstValue::Float(_) | ConstValue::Bool(_) => format!("{value}"),
    }
}

fn bool_operand(op: BinaryOp, value: ConstValue, span: Span) -> Result<bool, TypeError> {
    match value {
        ConstValue::Bool(value) => Ok(value),
        value => Err(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: const_type(&value),
            span,
        }),
    }
}

fn eval_unary(op: UnaryOp, value: ConstValue, span: Span) -> Result<ConstValue, TypeError> {
    match (op, value) {
        (UnaryOp::Neg, ConstValue::Int(value)) => value
            .checked_neg()
            .map(ConstValue::Int)
            .ok_or(TypeError::ConstOverflow { span }),
        (UnaryOp::Neg, ConstValue::Float(value)) => Ok(ConstValue::Float(-value)),
        (UnaryOp::Not, ConstValue::Bool(value)) => Ok(ConstValue::Bool(!value)),
        (UnaryOp::BitNot, ConstValue::Int(value)) => Ok(ConstValue::Int(!value)),
        (_, value) => Err(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: const_type(&value),
            span,
        }),
    }
}

fn eval_binary(
    op: BinaryOp,
    left: ConstValue,
    right: ConstValue,
    span: Span,
) -> Result<ConstValue, TypeError> {
    match (left, right) {
        (ConstValue::Int(a), ConstValue::Int(b)) => eval_int_binary(op, a, b, span),
        (ConstValue::Float(a), ConstValue::Float(b)) => eval_float_binary(op, a, b, span),
        (ConstValue::Bool(a), ConstValue::Bool(b)) => eval_bool_binary(op, a, b, span),
        (ConstValue::String(a), ConstValue::String(b)) => eval_string_binary(op, a, b, span),
        (ConstValue::String(a), value) if op == BinaryOp::Add => {
            Ok(ConstValue::String(format!("{a}{}", const_string(&value))))
        }
        (value, ConstValue::String(b)) if op == BinaryOp::Add => {
            Ok(ConstValue::String(format!("{}{b}", const_string(&value))))
        }
        (left, right) => Err(TypeError::TypeMismatch {
            expected: const_type(&left),
            found: const_type(&right),
            span,
        }),
    }
}

fn eval_int_binary(op: BinaryOp, a: i64, b: i64, span: Span) -> Result<ConstValue, TypeError> {
    let int = |value| Ok(ConstValue::Int(value));
    let bool = |value| Ok(ConstValue::Bool(value));
    match op {
        BinaryOp::Add => a.checked_add(b).map(ConstValue::Int),
        BinaryOp::Sub => a.checked_sub(b).map(ConstValue::Int),
        BinaryOp::Mul => a.checked_mul(b).map(ConstValue::Int),
        BinaryOp::Div if b == 0 => return Err(TypeError::ConstDivisionByZero { span }),
        BinaryOp::Div => a.checked_div(b).map(ConstValue::Int),
        BinaryOp::Rem if b == 0 => return Err(TypeError::ConstDivisionByZero { span }),
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
            return Err(TypeError::InvalidOperand {
                op: format!("{op}"),
                operand_type: Type::Int,
                span,
            });
        }
    }
    .ok_or(TypeError::ConstOverflow { span })
}

fn eval_float_binary(op: BinaryOp, a: f64, b: f64, span: Span) -> Result<ConstValue, TypeError> {
    match op {
        BinaryOp::Add => Ok(ConstValue::Float(a + b)),
        BinaryOp::Sub => Ok(ConstValue::Float(a - b)),
        BinaryOp::Mul => Ok(ConstValue::Float(a * b)),
        BinaryOp::Div if b == 0.0 => Err(TypeError::ConstDivisionByZero { span }),
        BinaryOp::Div => Ok(ConstValue::Float(a / b)),
        BinaryOp::Eq => Ok(ConstValue::Bool(a.to_bits() == b.to_bits())),
        BinaryOp::NotEq => Ok(ConstValue::Bool(a.to_bits() != b.to_bits())),
        BinaryOp::LessThan => Ok(ConstValue::Bool(a < b)),
        BinaryOp::GreaterThan => Ok(ConstValue::Bool(a > b)),
        BinaryOp::LessThanEq => Ok(ConstValue::Bool(a <= b)),
        BinaryOp::GreaterThanEq => Ok(ConstValue::Bool(a >= b)),
        _ => Err(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: Type::Float,
            span,
        }),
    }
}

fn eval_bool_binary(op: BinaryOp, a: bool, b: bool, span: Span) -> Result<ConstValue, TypeError> {
    match op {
        BinaryOp::And => Ok(ConstValue::Bool(a && b)),
        BinaryOp::Or => Ok(ConstValue::Bool(a || b)),
        BinaryOp::Eq => Ok(ConstValue::Bool(a == b)),
        BinaryOp::NotEq => Ok(ConstValue::Bool(a != b)),
        _ => Err(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: Type::Bool,
            span,
        }),
    }
}

fn eval_string_binary(
    op: BinaryOp,
    a: String,
    b: String,
    span: Span,
) -> Result<ConstValue, TypeError> {
    match op {
        BinaryOp::Add => Ok(ConstValue::String(format!("{a}{b}"))),
        BinaryOp::Eq => Ok(ConstValue::Bool(a == b)),
        BinaryOp::NotEq => Ok(ConstValue::Bool(a != b)),
        _ => Err(TypeError::InvalidOperand {
            op: format!("{op}"),
            operand_type: Type::String,
            span,
        }),
    }
}
