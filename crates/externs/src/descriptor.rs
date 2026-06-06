use std::fmt;

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

impl fmt::Display for ExternTypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Void => f.write_str("void"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
            Self::Any => f.write_str("any"),
            Self::List(inner) => write!(f, "[{inner}]"),
            Self::Map(key, value) => write!(f, "[{key}: {value}]"),
            Self::Option(inner) => write!(f, "{inner}?"),
            Self::Named { module, name, args } => {
                if let Some(module) = module {
                    write!(f, "{module}.{name}")?;
                } else {
                    f.write_str(name)?;
                }
                if !args.is_empty() {
                    let args = args
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "<{args}>")?;
                }
                Ok(())
            }
            Self::Callback(_) => f.write_str("callback"),
        }
    }
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
