use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{ExternOperator, ModulePath, ProviderId};

pub const CALLBACK_WRAPPER_MAX_ARITY: usize = 8;

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
    #[serde(default)]
    pub variants: Vec<ExternEnumVariantDescriptor>,
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
pub struct ExternEnumVariantDescriptor {
    pub name: String,
    pub fields: Vec<ExternEnumVariantFieldDescriptor>,
    pub doc: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternEnumVariantFieldDescriptor {
    pub name: Option<String>,
    pub ty: ExternTypeExpr,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternInitDescriptor {
    pub params: Vec<ExternParam>,
    pub field_init: Vec<String>,
    #[serde(default)]
    pub presence_init: Vec<String>,
    #[serde(default)]
    pub ret: ExternTypeExpr,
    #[serde(default)]
    pub effects: ExternEffects,
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

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ExternTypeExpr {
    #[default]
    Void,
    Unit,
    Bool,
    Int,
    Float,
    String,
    Char,
    Any,
    Option(Box<ExternTypeExpr>),
    Result(Box<ExternTypeExpr>, Box<ExternTypeExpr>),
    Tuple(Vec<ExternTypeExpr>),
    Array {
        elem: Box<ExternTypeExpr>,
        len: u64,
    },
    List(Box<ExternTypeExpr>),
    Map(Box<ExternTypeExpr>, Box<ExternTypeExpr>),
    Slice(Box<ExternTypeExpr>),
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
            Self::Unit => f.write_str("()"),
            Self::Bool => f.write_str("bool"),
            Self::Int => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::String => f.write_str("string"),
            Self::Char => f.write_str("char"),
            Self::Any => f.write_str("any"),
            Self::Option(inner) => write!(f, "{inner}?"),
            Self::Result(ok, err) => write!(f, "Result<{ok}, {err}>"),
            Self::Tuple(fields) => {
                f.write_str("(")?;
                for (index, field) in fields.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{field}")?;
                }
                if fields.len() == 1 {
                    f.write_str(",")?;
                }
                f.write_str(")")
            }
            Self::Array { elem, len } => write!(f, "[{elem}; {len}]"),
            Self::List(inner) => write!(f, "[{inner}]"),
            Self::Map(key, value) => write!(f, "[{key}: {value}]"),
            Self::Slice(inner) => write!(f, "slice[{inner}]"),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiPosition {
    Return,
    ParamValue,
    ParamBorrow,
    ParamMutBorrow,
    CallbackParam,
    CallbackReturn,
    Field,
    Nested,
}

impl AbiPosition {
    fn allows_direct_capability(self) -> bool {
        matches!(
            self,
            Self::ParamValue | Self::ParamBorrow | Self::ParamMutBorrow
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiTypeClass {
    Value,
    ParamOnly,
    ReturnOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AbiTypeError {
    VoidOutsideReturn,
    SliceOutsideParam,
    SliceNested,
    CallbackOutsideParam,
    CallbackNested,
    CallbackReturnUnsupported,
    CallbackThreadUnsupported,
    GenericNamedArgsUnsupported,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AbiTypeViolation {
    pub position: AbiPosition,
    pub reason: AbiTypeError,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallbackEscapeMismatch {
    pub param_escape: CallbackEscape,
    pub policy_escape: CallbackEscape,
}

pub fn effective_callback_escape(
    param_escape: CallbackEscape,
    callback: &ExternCallbackSignature,
) -> Result<CallbackEscape, CallbackEscapeMismatch> {
    if callback.policy.escape == param_escape {
        Ok(param_escape)
    } else {
        Err(CallbackEscapeMismatch {
            param_escape,
            policy_escape: callback.policy.escape,
        })
    }
}

impl ExternCallbackSignature {
    pub fn scoped_lambda_policy_supported(&self) -> bool {
        self.policy.escape == CallbackEscape::NonEscaping
            && self.policy.thread == CallbackThread::SameThread
    }

    pub fn callback_wrapper_signature_supported(&self) -> bool {
        self.params.len() <= CALLBACK_WRAPPER_MAX_ARITY
            && self
                .params
                .iter()
                .all(ExternCallbackParam::callback_wrapper_supported)
            && self.ret.callback_wrapper_return_supported()
    }

    pub fn scoped_lambda_supported(&self) -> bool {
        self.scoped_lambda_policy_supported() && self.callback_wrapper_signature_supported()
    }
}

impl ExternCallbackParam {
    pub fn callback_wrapper_supported(&self) -> bool {
        self.escape == CallbackEscape::NonEscaping && self.ty.callback_wrapper_param_supported()
    }
}

impl ExternTypeExpr {
    pub fn classify_abi(
        &self,
        position: AbiPosition,
    ) -> Result<AbiTypeClass, Vec<AbiTypeViolation>> {
        let mut violations = vec![];
        let class = self.classify_abi_inner(position, &mut violations);
        if violations.is_empty() {
            Ok(class)
        } else {
            Err(violations)
        }
    }

    fn classify_abi_inner(
        &self,
        position: AbiPosition,
        violations: &mut Vec<AbiTypeViolation>,
    ) -> AbiTypeClass {
        match self {
            Self::Void => {
                if position != AbiPosition::Return && position != AbiPosition::CallbackReturn {
                    violations.push(AbiTypeViolation {
                        position,
                        reason: AbiTypeError::VoidOutsideReturn,
                    });
                }
                AbiTypeClass::ReturnOnly
            }
            Self::Unit
            | Self::Bool
            | Self::Int
            | Self::Float
            | Self::String
            | Self::Char
            | Self::Any => AbiTypeClass::Value,
            Self::Option(inner) | Self::Array { elem: inner, .. } | Self::List(inner) => {
                inner.classify_nested(violations);
                AbiTypeClass::Value
            }
            Self::Result(ok, err) | Self::Map(ok, err) => {
                ok.classify_nested(violations);
                err.classify_nested(violations);
                AbiTypeClass::Value
            }
            Self::Tuple(fields) => {
                for field in fields {
                    field.classify_nested(violations);
                }
                AbiTypeClass::Value
            }
            Self::Slice(inner) => {
                let reason = if position == AbiPosition::Nested {
                    AbiTypeError::SliceNested
                } else {
                    AbiTypeError::SliceOutsideParam
                };
                if !position.allows_direct_capability() {
                    violations.push(AbiTypeViolation { position, reason });
                }
                inner.classify_nested(violations);
                AbiTypeClass::ParamOnly
            }
            Self::Named { args, .. } => {
                if !args.is_empty() {
                    violations.push(AbiTypeViolation {
                        position,
                        reason: AbiTypeError::GenericNamedArgsUnsupported,
                    });
                }
                for arg in args {
                    arg.classify_nested(violations);
                }
                AbiTypeClass::Value
            }
            Self::Callback(callback) => {
                let reason = match position {
                    AbiPosition::Nested | AbiPosition::CallbackParam => {
                        AbiTypeError::CallbackNested
                    }
                    AbiPosition::CallbackReturn => AbiTypeError::CallbackReturnUnsupported,
                    _ => AbiTypeError::CallbackOutsideParam,
                };
                if !position.allows_direct_capability() {
                    violations.push(AbiTypeViolation { position, reason });
                }
                if callback.policy.thread != CallbackThread::SameThread {
                    violations.push(AbiTypeViolation {
                        position,
                        reason: AbiTypeError::CallbackThreadUnsupported,
                    });
                }
                for param in &callback.params {
                    param
                        .ty
                        .classify_abi_inner(AbiPosition::CallbackParam, violations);
                }
                callback
                    .ret
                    .classify_abi_inner(AbiPosition::CallbackReturn, violations);
                AbiTypeClass::ParamOnly
            }
        }
    }

    fn classify_nested(&self, violations: &mut Vec<AbiTypeViolation>) {
        self.classify_abi_inner(AbiPosition::Nested, violations);
    }

    pub fn option(inner: Self) -> Self {
        Self::Option(Box::new(inner))
    }

    pub fn result(ok: Self, err: Self) -> Self {
        Self::Result(Box::new(ok), Box::new(err))
    }

    pub fn list(inner: Self) -> Self {
        Self::List(Box::new(inner))
    }

    pub fn map(key: Self, value: Self) -> Self {
        Self::Map(Box::new(key), Box::new(value))
    }

    pub fn array(elem: Self, len: u64) -> Self {
        Self::Array {
            elem: Box::new(elem),
            len,
        }
    }

    pub fn slice(inner: Self) -> Self {
        Self::Slice(Box::new(inner))
    }

    pub fn named(module: Option<ModulePath>, name: impl Into<String>) -> Self {
        Self::Named {
            module,
            name: name.into(),
            args: vec![],
        }
    }

    pub fn callback_wrapper_param_supported(&self) -> bool {
        matches!(self, Self::Bool | Self::Int | Self::Float | Self::Char)
    }

    pub fn callback_wrapper_return_supported(&self) -> bool {
        matches!(self, Self::Void) || self.callback_wrapper_param_supported()
    }
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct CallbackPolicy {
    pub escape: CallbackEscape,
    pub thread: CallbackThread,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CallbackEscape {
    #[default]
    NonEscaping,
    Escaping,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum CallbackThread {
    #[default]
    SameThread,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn callback(escape: CallbackEscape) -> ExternCallbackSignature {
        ExternCallbackSignature {
            params: vec![],
            ret: Box::new(ExternTypeExpr::Void),
            policy: CallbackPolicy {
                escape,
                thread: CallbackThread::SameThread,
            },
        }
    }

    #[test]
    fn displays_final_abi_shapes() {
        let cases = [
            (ExternTypeExpr::Void, "void"),
            (ExternTypeExpr::Unit, "()"),
            (ExternTypeExpr::Bool, "bool"),
            (ExternTypeExpr::Int, "int"),
            (ExternTypeExpr::Float, "float"),
            (ExternTypeExpr::String, "string"),
            (ExternTypeExpr::Char, "char"),
            (ExternTypeExpr::Any, "any"),
            (ExternTypeExpr::option(ExternTypeExpr::Int), "int?"),
            (
                ExternTypeExpr::result(
                    ExternTypeExpr::String,
                    ExternTypeExpr::named(None, "LoadError"),
                ),
                "Result<string, LoadError>",
            ),
            (
                ExternTypeExpr::Tuple(vec![ExternTypeExpr::Int, ExternTypeExpr::Float]),
                "(int, float)",
            ),
            (ExternTypeExpr::Tuple(vec![ExternTypeExpr::Bool]), "(bool,)"),
            (
                ExternTypeExpr::array(ExternTypeExpr::Float, 4),
                "[float; 4]",
            ),
            (ExternTypeExpr::list(ExternTypeExpr::Int), "[int]"),
            (
                ExternTypeExpr::map(ExternTypeExpr::String, ExternTypeExpr::Bool),
                "[string: bool]",
            ),
            (ExternTypeExpr::slice(ExternTypeExpr::Int), "slice[int]"),
            (
                ExternTypeExpr::Named {
                    module: Some(ModulePath {
                        segments: vec!["math".to_string()],
                    }),
                    name: "Vec2".to_string(),
                    args: vec![ExternTypeExpr::Float],
                },
                "math.Vec2<float>",
            ),
            (
                ExternTypeExpr::Callback(callback(CallbackEscape::NonEscaping)),
                "callback",
            ),
        ];

        for (ty, expected) in cases {
            assert_eq!(ty.to_string(), expected);
        }
    }
}
