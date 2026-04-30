use crate::{ExternOperator, ModulePath, ProviderId};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProviderDescriptor {
    pub provider: ProviderId,
    pub modules: Vec<ExternModuleDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternModuleDescriptor {
    pub path: ModulePath,
    pub types: Vec<ExternTypeDescriptor>,
    pub functions: Vec<ExternFunctionDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternTypeDescriptor {
    pub name: String,
    pub doc: Option<String>,
    pub rep: ExternRep,
    pub fields: Vec<ExternFieldDescriptor>,
    pub init: Option<ExternInitDescriptor>,
    pub methods: Vec<ExternMethodDescriptor>,
    pub statics: Vec<ExternStaticDescriptor>,
    pub operators: Vec<ExternOperatorDescriptor>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternFunctionDescriptor {
    pub name: String,
    pub doc: Option<String>,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternFieldDescriptor {
    pub name: String,
    pub ty: ExternTypeExpr,
    pub access: FieldAccess,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternInitDescriptor {
    pub params: Vec<ExternParam>,
    pub field_init: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternMethodDescriptor {
    pub name: String,
    pub doc: Option<String>,
    pub receiver: ReceiverMode,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternStaticDescriptor {
    pub name: String,
    pub doc: Option<String>,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternOperatorDescriptor {
    pub op: ExternOperator,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternSignature {
    pub params: Vec<ExternParam>,
    pub ret: ExternTypeExpr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternParam {
    pub name: Option<String>,
    pub ty: ExternTypeExpr,
    pub flow: ParamFlow,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternTypeExpr {
    Void,
    Bool,
    Int,
    Float,
    String,
    Any,
    List(Box<ExternTypeExpr>),
    Map(Box<ExternTypeExpr>, Box<ExternTypeExpr>),
    Option(Box<ExternTypeExpr>),
    Named {
        module: Option<ModulePath>,
        name: String,
        args: Vec<ExternTypeExpr>,
    },
    Callback(ExternCallbackSignature),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExternRep {
    Shared,
    Inline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldAccess {
    ReadOnly { computed: bool },
    ReadWrite { computed: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReceiverMode {
    Value,
    Shared,
    Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamFlow {
    Value,
    Borrow,
    MutBorrow,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct ExternEffects {
    pub fallible: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternCallbackSignature {
    pub params: Vec<ExternTypeExpr>,
    pub ret: Box<ExternTypeExpr>,
    pub policy: CallbackPolicy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallbackPolicy {
    pub escape: CallbackEscape,
    pub thread: CallbackThread,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallbackEscape {
    NonEscaping,
    Escaping,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallbackThread {
    SameThread,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BinaryOp, ExternOperator};

    fn module(segments: &[&str]) -> ModulePath {
        ModulePath {
            segments: segments
                .iter()
                .map(|segment| (*segment).to_string())
                .collect(),
        }
    }

    #[test]
    fn receiver_is_not_param_flow() {
        let method = ExternMethodDescriptor {
            name: "move".to_string(),
            doc: None,
            receiver: ReceiverMode::Mutable,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: Some("dx".to_string()),
                    ty: ExternTypeExpr::Float,
                    flow: ParamFlow::Value,
                }],
                ret: ExternTypeExpr::Void,
            },
            effects: ExternEffects::default(),
        };

        assert_eq!(method.receiver, ReceiverMode::Mutable);
        assert_eq!(method.signature.params[0].flow, ParamFlow::Value);
    }

    #[test]
    fn params_have_flow() {
        let function = ExternFunctionDescriptor {
            name: "mix".to_string(),
            doc: None,
            signature: ExternSignature {
                params: vec![
                    ExternParam {
                        name: Some("a".to_string()),
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Value,
                    },
                    ExternParam {
                        name: Some("b".to_string()),
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Borrow,
                    },
                    ExternParam {
                        name: Some("out".to_string()),
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::MutBorrow,
                    },
                ],
                ret: ExternTypeExpr::Void,
            },
            effects: ExternEffects::default(),
        };
        let flows = function
            .signature
            .params
            .iter()
            .map(|param| param.flow)
            .collect::<Vec<_>>();

        assert_eq!(
            flows,
            [ParamFlow::Value, ParamFlow::Borrow, ParamFlow::MutBorrow]
        );
    }

    #[test]
    fn operator_flow_is_operand_flow() {
        let operator = ExternOperatorDescriptor {
            op: ExternOperator::Binary {
                op: BinaryOp::Add,
                self_on_right: true,
            },
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Float,
                    flow: ParamFlow::Borrow,
                }],
                ret: ExternTypeExpr::Float,
            },
            effects: ExternEffects::default(),
        };

        assert_eq!(operator.signature.params[0].flow, ParamFlow::Borrow);
        assert_eq!(
            operator.op,
            ExternOperator::Binary {
                op: BinaryOp::Add,
                self_on_right: true,
            }
        );
    }
}
