use super::ConstDiagnostic;
use crate::ast::{ArrayLen, ConstArg, ConstExpr, ConstParamId, ConstValue, Ident};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub(crate) struct ConstInferVarId(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ConstTerm {
    Value(ConstValue),
    Name(Ident),
    Param(ConstParamId),
    Expr(ConstExpr),
    ArrayInfer,
    Infer(ConstInferVarId),
}

impl ConstTerm {
    pub(crate) fn from_arg(arg: &ConstArg) -> Self {
        match arg {
            ConstArg::Value(value) => Self::Value(value.value().clone()),
            ConstArg::Name(name) => Self::Name(*name),
            ConstArg::Param(id) => Self::Param(*id),
        }
    }

    pub(crate) fn from_args(args: &[ConstArg]) -> Vec<Self> {
        args.iter().map(Self::from_arg).collect()
    }

    pub(crate) fn from_array_len(len: ArrayLen) -> Self {
        match len {
            ArrayLen::Fixed(value) => Self::from_usize(*value.value()),
            ArrayLen::Infer => Self::ArrayInfer,
            ArrayLen::Named(name) => Self::Name(name),
            ArrayLen::Param(id) => Self::Param(id),
            ArrayLen::Expr(expr) => Self::Expr(expr),
        }
    }

    pub(crate) fn from_usize(value: usize) -> Self {
        let value = i64::try_from(value).expect("const term exceeds int range");
        Self::Value(ConstValue::Int(value))
    }

    pub(crate) fn to_arg_no_infer(&self) -> Option<ConstArg> {
        match self {
            Self::Value(value) => Some(ConstArg::value(value.clone())),
            Self::Name(name) => Some(ConstArg::Name(*name)),
            Self::Param(id) => Some(ConstArg::Param(*id)),
            Self::Expr(_) | Self::ArrayInfer | Self::Infer(_) => None,
        }
    }

    pub(crate) fn to_args_no_infer(args: &[Self]) -> Option<Vec<ConstArg>> {
        args.iter().map(Self::to_arg_no_infer).collect()
    }

    pub(crate) fn to_array_len_no_infer(&self) -> Option<ArrayLen> {
        match self {
            Self::Value(ConstValue::Int(value)) => {
                usize::try_from(*value).ok().map(ArrayLen::fixed)
            }
            Self::Value(_) | Self::Infer(_) => None,
            Self::Name(name) => Some(ArrayLen::Named(*name)),
            Self::Param(id) => Some(ArrayLen::Param(*id)),
            Self::Expr(expr) => Some(ArrayLen::Expr(expr.clone())),
            Self::ArrayInfer => Some(ArrayLen::Infer),
        }
    }

    pub(crate) fn diagnostic(&self) -> ConstDiagnostic {
        match self {
            Self::Value(value) => ConstDiagnostic::Value(value.clone()),
            Self::Name(name) => ConstDiagnostic::Name(*name),
            Self::Param(_) | Self::Expr(_) | Self::ArrayInfer | Self::Infer(_) => {
                ConstDiagnostic::Unknown
            }
        }
    }

    pub(crate) fn is_self_binding(&self, var: ConstInferVarId) -> bool {
        matches!(self, Self::Infer(id) if *id == var)
    }
}
