use serde::{Deserialize, Serialize};

use crate::{ExternOperator, ModulePath, ProviderId};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ProviderDescriptor {
    pub provider: ProviderId,
    pub modules: Vec<ExternModuleDescriptor>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternModuleDescriptor {
    pub path: ModulePath,
    pub types: Vec<ExternTypeDescriptor>,
    pub functions: Vec<ExternFunctionDescriptor>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternFunctionDescriptor {
    pub name: String,
    pub doc: Option<String>,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternFieldDescriptor {
    pub name: String,
    pub ty: ExternTypeExpr,
    pub computed: bool,
    #[serde(default = "default_true")]
    pub readable: bool,
    #[serde(default = "default_true")]
    pub writable: bool,
    #[serde(default = "default_shared_receiver")]
    pub get_receiver: ReceiverMode,
    #[serde(default = "default_mutable_receiver")]
    pub set_receiver: ReceiverMode,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternInitDescriptor {
    pub params: Vec<ExternParam>,
    pub field_init: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternMethodDescriptor {
    pub name: String,
    pub doc: Option<String>,
    pub receiver: ReceiverMode,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternStaticDescriptor {
    pub name: String,
    pub doc: Option<String>,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternOperatorDescriptor {
    pub op: ExternOperator,
    #[serde(default = "default_shared_receiver")]
    pub receiver: ReceiverMode,
    pub signature: ExternSignature,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternSignature {
    pub params: Vec<ExternParam>,
    pub ret: ExternTypeExpr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternParam {
    pub name: Option<String>,
    pub ty: ExternTypeExpr,
    pub flow: ParamFlow,
    pub escape: CallbackEscape,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ExternRep {
    Shared,
    Inline,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ReceiverMode {
    Value,
    Shared,
    Mutable,
}

fn default_true() -> bool {
    true
}

fn default_shared_receiver() -> ReceiverMode {
    ReceiverMode::Shared
}

fn default_mutable_receiver() -> ReceiverMode {
    ReceiverMode::Mutable
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ParamFlow {
    Value,
    Borrow,
    MutBorrow,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternEffects {
    pub fallible: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternCallbackSignature {
    pub params: Vec<ExternCallbackParam>,
    pub ret: Box<ExternTypeExpr>,
    pub policy: CallbackPolicy,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternCallbackParam {
    pub ty: ExternTypeExpr,
    pub escape: CallbackEscape,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct CallbackPolicy {
    pub escape: CallbackEscape,
    pub thread: CallbackThread,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CallbackEscape {
    NonEscaping,
    Escaping,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
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
                    escape: CallbackEscape::NonEscaping,
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
                        escape: CallbackEscape::NonEscaping,
                    },
                    ExternParam {
                        name: Some("b".to_string()),
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::Borrow,
                        escape: CallbackEscape::NonEscaping,
                    },
                    ExternParam {
                        name: Some("out".to_string()),
                        ty: ExternTypeExpr::Float,
                        flow: ParamFlow::MutBorrow,
                        escape: CallbackEscape::NonEscaping,
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
            receiver: ReceiverMode::Shared,
            signature: ExternSignature {
                params: vec![ExternParam {
                    name: None,
                    ty: ExternTypeExpr::Float,
                    flow: ParamFlow::Borrow,
                    escape: CallbackEscape::NonEscaping,
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
