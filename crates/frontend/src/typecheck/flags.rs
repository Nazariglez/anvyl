use std::collections::{HashMap, HashSet};

use super::{
    CheckedType, FlagStaticFact, PendingRawVariant, ResolvedFlagMember, ResolvedFlagValues,
    TypeChecker, TypeError, check_arg_count, check_expr_checked, checked_type,
    const_eval::const_type, decls::CallableParent, semantic_use::BodyInstanceKey,
};
use crate::{
    ast::{
        BinaryOp, CallNode, ConstValue, EnumKindConstraint, ExprId, ExtendTargetConstraint, Ident,
        Type, UnaryOp,
    },
    span::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FlagResolveErrorKind {
    Negative,
    Duplicate,
    UnknownCompositeBits,
    AutomaticOverflow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct FlagResolveError {
    pub(super) kind: FlagResolveErrorKind,
    pub(super) member: Ident,
    pub(super) span: Span,
}

pub(super) fn resolve_values(
    tc: &mut TypeChecker,
    variants: &[PendingRawVariant],
) -> (Option<ResolvedFlagValues>, Vec<FlagResolveError>) {
    tc.flag_initializer_values.push(HashMap::new());
    let mut seen = HashSet::new();
    let mut members = Vec::with_capacity(variants.len());
    let mut known_bits = 0;
    let mut next_bit = 0;
    let mut errors = vec![];
    let mut failed = false;

    for variant in variants {
        let value = if let Some(expr) = &variant.value {
            match tc.eval_enum_value(expr) {
                Some(ConstValue::Int(value)) => value,
                Some(other) => {
                    tc.push_error(TypeError::FlagExpectedIntValue {
                        found: const_type(&other),
                        span: tc.error_span(expr.span),
                    });
                    failed = true;
                    continue;
                }
                None => {
                    failed = true;
                    continue;
                }
            }
        } else if next_bit <= 62 {
            1_i64 << next_bit
        } else {
            errors.push(FlagResolveError {
                kind: FlagResolveErrorKind::AutomaticOverflow,
                member: variant.name,
                span: variant.span,
            });
            failed = true;
            continue;
        };

        let kind = if value < 0 {
            Some(FlagResolveErrorKind::Negative)
        } else {
            let atomic = value > 0 && value & (value - 1) == 0;
            if !atomic && value & !known_bits != 0 {
                Some(FlagResolveErrorKind::UnknownCompositeBits)
            } else if !seen.insert(value) {
                Some(FlagResolveErrorKind::Duplicate)
            } else {
                if atomic {
                    let bit = value.trailing_zeros();
                    next_bit = next_bit.max(bit + 1);
                    known_bits |= value;
                }
                members.push(ResolvedFlagMember {
                    name: variant.name,
                    value,
                    atomic,
                });
                tc.flag_initializer_values
                    .last_mut()
                    .expect("flag initializer scope")
                    .insert(variant.name, value);
                None
            }
        };
        if let Some(kind) = kind {
            errors.push(FlagResolveError {
                kind,
                member: variant.name,
                span: variant.span,
            });
            failed = true;
        }
    }

    tc.flag_initializer_values.pop();
    (
        (!failed).then_some(ResolvedFlagValues {
            members,
            known_bits,
        }),
        errors,
    )
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FlagStaticOp {
    Empty,
    All,
}

impl FlagStaticOp {
    pub(crate) const ALL: [Self; 2] = [Self::Empty, Self::All];

    pub(crate) fn name(self) -> Ident {
        Ident::new(match self {
            Self::Empty => "empty",
            Self::All => "all",
        })
    }

    pub(crate) fn from_name(name: Ident) -> Option<Self> {
        Self::ALL.into_iter().find(|op| op.name() == name)
    }
}

pub(super) fn classify_binary(
    tc: &mut TypeChecker,
    op: BinaryOp,
    left: (&Type, Span),
    right: (&Type, Span),
) -> Option<Type> {
    if left.0 != right.0 || !is_type(tc, left.1, left.0) {
        return None;
    }
    match op {
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor => Some(left.0.clone()),
        BinaryOp::Eq | BinaryOp::NotEq => Some(Type::Bool),
        _ => None,
    }
}

pub(super) fn classify_unary(
    tc: &mut TypeChecker,
    op: UnaryOp,
    operand: &Type,
    span: Span,
) -> Option<Type> {
    (op == UnaryOp::BitNot && is_type(tc, span, operand)).then(|| operand.clone())
}

pub(super) fn is_type(tc: &mut TypeChecker, span: Span, ty: &Type) -> bool {
    tc.ensure_pending_enum_values_for_type(ty, span);
    tc.decls
        .enum_schema_for_type(ty)
        .is_some_and(|schema| schema.body.kind.flag().is_some())
}

pub(super) fn check_static_call(
    op: FlagStaticOp,
    owner_ty: &Type,
    call: &CallNode,
    call_id: ExprId,
    tc: &mut TypeChecker,
) -> CheckedType {
    let valid = check_arg_count(&call.node.args, 0, call.span, tc);
    for arg in &call.node.args {
        check_expr_checked(arg, tc);
    }
    if valid {
        tc.record_flag_static(FlagStaticFact {
            expr_id: call_id,
            owner_ty: owner_ty.clone(),
            op,
        });
    }
    checked_type(owner_ty.clone())
}

pub(super) fn static_op(tc: &mut TypeChecker, target: &Type, name: Ident) -> Option<FlagStaticOp> {
    let op = FlagStaticOp::from_name(name)?;
    if let Some(schema) = tc.decls.enum_schema_for_type(target)
        && schema.body.kind.flag().is_some()
    {
        return Some(op);
    }
    let Type::Var(target_var) = target else {
        return None;
    };
    let BodyInstanceKey::Callable(body) = tc.current_body() else {
        return None;
    };
    let Some(CallableParent::Extend(extend_id)) = &body.target.parent else {
        return None;
    };
    let extend = tc.decls.extend(extend_id)?;
    let Type::Var(extend_var) = &extend.target else {
        return None;
    };
    (target_var == extend_var
        && matches!(
            extend.target_constraint,
            Some(ExtendTargetConstraint::Enum {
                kind: EnumKindConstraint::Flag,
            })
        ))
    .then_some(op)
}
