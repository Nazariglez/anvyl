use std::fmt;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ProviderId {
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ModulePath {
    pub segments: Vec<String>,
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.segments.is_empty() {
            f.write_str("<empty>")
        } else {
            f.write_str(&self.segments.join("."))
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternTypeKey {
    pub module: ModulePath,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternFunctionKey {
    pub module: ModulePath,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternMemberKey {
    pub owner: ExternTypeKey,
    pub selector: ExternMemberSelector,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ExternMemberSelector {
    Field(String),
    Method(String),
    Static(String),
    Init,
    Operator(ExternOperator),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ExternBindingKey {
    pub target: ExternBindingTarget,
    pub operation: ExternBindingOp,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ExternBindingTarget {
    Function(ExternFunctionKey),
    Member(ExternMemberKey),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ExternBindingOp {
    Call,
    Get,
    Set,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    NotEq,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ExternOperator {
    Unary(UnaryOp),
    Binary { op: BinaryOp, self_on_right: bool },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum OperatorReturn {
    Bool,
    NonVoid,
}

impl fmt::Display for ExternOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unary(op) => write!(f, "unary {op}"),
            Self::Binary { op, self_on_right } => {
                if *self_on_right {
                    write!(f, "right {op}")
                } else {
                    write!(f, "{op}")
                }
            }
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg => f.write_str("-"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::LessThan => "<",
            Self::GreaterThan => ">",
            Self::LessThanEq => "<=",
            Self::GreaterThanEq => ">=",
        })
    }
}

impl fmt::Display for OperatorReturn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::NonVoid => "non-void",
        })
    }
}

impl ExternOperator {
    pub fn return_requirement(self) -> OperatorReturn {
        match self {
            ExternOperator::Binary { op, .. } if op.returns_bool() => OperatorReturn::Bool,
            _ => OperatorReturn::NonVoid,
        }
    }
}

impl BinaryOp {
    fn returns_bool(self) -> bool {
        matches!(
            self,
            BinaryOp::Eq
                | BinaryOp::NotEq
                | BinaryOp::LessThan
                | BinaryOp::GreaterThan
                | BinaryOp::LessThanEq
                | BinaryOp::GreaterThanEq
        )
    }
}
