use super::ConstDiagnostic;
use crate::ast::{ArrayLen, ConstArg, ConstParamId, ConstValue, Ident};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub(crate) struct ConstInferVarId(pub(crate) u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum ConstTerm {
    Value(ConstValue),
    Name(Ident),
    Param(ConstParamId),
    ArrayInfer,
    Infer(ConstInferVarId),
}

impl ConstTerm {
    pub(crate) fn from_arg(arg: &ConstArg) -> Self {
        match arg {
            ConstArg::Value(value) => Self::Value(value.clone()),
            ConstArg::Name(name) => Self::Name(*name),
            ConstArg::Param(id) => Self::Param(*id),
        }
    }

    pub(crate) fn from_args(args: &[ConstArg]) -> Vec<Self> {
        args.iter().map(Self::from_arg).collect()
    }

    pub(crate) fn from_array_len(len: ArrayLen) -> Self {
        match len {
            ArrayLen::Fixed(value) => Self::from_usize(value),
            ArrayLen::Infer => Self::ArrayInfer,
            ArrayLen::Named(name) => Self::Name(name),
            ArrayLen::Param(id) => Self::Param(id),
        }
    }

    pub(crate) fn from_usize(value: usize) -> Self {
        let value = i64::try_from(value).expect("const term exceeds int range");
        Self::Value(ConstValue::Int(value))
    }

    pub(crate) fn to_arg_no_infer(&self) -> Option<ConstArg> {
        match self {
            Self::Value(value) => Some(ConstArg::Value(value.clone())),
            Self::Name(name) => Some(ConstArg::Name(*name)),
            Self::Param(id) => Some(ConstArg::Param(*id)),
            Self::ArrayInfer | Self::Infer(_) => None,
        }
    }

    pub(crate) fn to_args_no_infer(args: &[Self]) -> Option<Vec<ConstArg>> {
        args.iter().map(Self::to_arg_no_infer).collect()
    }

    pub(crate) fn to_array_len_no_infer(&self) -> Option<ArrayLen> {
        match self {
            Self::Value(ConstValue::Int(value)) => {
                usize::try_from(*value).ok().map(ArrayLen::Fixed)
            }
            Self::Value(_) | Self::Infer(_) => None,
            Self::Name(name) => Some(ArrayLen::Named(*name)),
            Self::Param(id) => Some(ArrayLen::Param(*id)),
            Self::ArrayInfer => Some(ArrayLen::Infer),
        }
    }

    pub(crate) fn diagnostic(&self) -> ConstDiagnostic {
        match self {
            Self::Value(value) => ConstDiagnostic::Value(value.clone()),
            Self::Name(name) => ConstDiagnostic::Name(*name),
            Self::Param(_) | Self::ArrayInfer | Self::Infer(_) => ConstDiagnostic::Unknown,
        }
    }

    pub(crate) fn is_self_binding(&self, var: ConstInferVarId) -> bool {
        matches!(self, Self::Infer(id) if *id == var)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn cp(id: u32) -> ConstParamId {
        ConstParamId(id)
    }

    #[test]
    fn const_args_roundtrip() {
        let args = [
            ConstArg::Value(ConstValue::Int(3)),
            ConstArg::Value(ConstValue::Bool(true)),
            ConstArg::Name(ident("N")),
            ConstArg::Param(cp(1)),
        ];
        for arg in args {
            assert_eq!(ConstTerm::from_arg(&arg).to_arg_no_infer(), Some(arg));
        }
    }

    #[test]
    fn array_lens_roundtrip() {
        let lens = [
            ArrayLen::Fixed(3),
            ArrayLen::Named(ident("N")),
            ArrayLen::Param(cp(1)),
            ArrayLen::Infer,
        ];
        for len in lens {
            assert_eq!(
                ConstTerm::from_array_len(len).to_array_len_no_infer(),
                Some(len)
            );
        }
    }

    #[test]
    fn fixed_len_matches_int_arg() {
        assert_eq!(
            ConstTerm::from_arg(&ConstArg::Value(ConstValue::Int(3))),
            ConstTerm::from_array_len(ArrayLen::Fixed(3)),
        );
    }

    #[test]
    fn public_conversions_reject_inference_states() {
        let infer = ConstTerm::Infer(ConstInferVarId(0));
        assert_eq!(ConstTerm::ArrayInfer.to_arg_no_infer(), None);
        assert_eq!(infer.to_arg_no_infer(), None);
        assert_eq!(infer.to_array_len_no_infer(), None);
        assert_eq!(
            ConstTerm::Value(ConstValue::Bool(true)).to_array_len_no_infer(),
            None
        );
        assert_eq!(
            ConstTerm::Value(ConstValue::Int(-1)).to_array_len_no_infer(),
            None
        );
    }

    #[test]
    fn diagnostics() {
        assert_eq!(
            ConstTerm::Value(ConstValue::Int(1)).diagnostic(),
            ConstDiagnostic::Value(ConstValue::Int(1)),
        );
        assert_eq!(
            ConstTerm::Name(ident("N")).diagnostic(),
            ConstDiagnostic::Name(ident("N")),
        );
        assert_eq!(
            ConstTerm::Param(cp(0)).diagnostic(),
            ConstDiagnostic::Unknown
        );
        assert_eq!(ConstTerm::ArrayInfer.diagnostic(), ConstDiagnostic::Unknown);
        assert_eq!(
            ConstTerm::Infer(ConstInferVarId(0)).diagnostic(),
            ConstDiagnostic::Unknown,
        );
    }
}
