use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ProviderId {
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ModulePath {
    pub segments: Vec<String>,
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

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    fn module(segments: &[&str]) -> ModulePath {
        ModulePath {
            segments: segments
                .iter()
                .map(|segment| (*segment).to_string())
                .collect(),
        }
    }

    fn ty(module: ModulePath, name: &str) -> ExternTypeKey {
        ExternTypeKey {
            module,
            name: name.to_string(),
        }
    }

    #[test]
    fn module_path_is_identity() {
        let a = ty(module(&["math"]), "Vec2");
        let b = ty(module(&["gfx"]), "Vec2");

        assert_ne!(a, b);
        assert_eq!(HashSet::from([a.clone(), b]).len(), 2);
        assert!(HashSet::from([a]).contains(&ty(module(&["math"]), "Vec2")));
    }

    #[test]
    fn operator_side_is_identity() {
        let owner = ty(module(&["math"]), "Vec2");
        let left = ExternMemberKey {
            owner: owner.clone(),
            selector: ExternMemberSelector::Operator(ExternOperator::Binary {
                op: BinaryOp::Mul,
                self_on_right: false,
            }),
        };
        let right = ExternMemberKey {
            owner,
            selector: ExternMemberSelector::Operator(ExternOperator::Binary {
                op: BinaryOp::Mul,
                self_on_right: true,
            }),
        };

        assert_ne!(left, right);
    }

    #[test]
    fn selectors_cover_operators() {
        let owner = ty(module(&["math"]), "Vec2");
        let unary = [ExternMemberSelector::Operator(ExternOperator::Unary(
            UnaryOp::Neg,
        ))];
        let binary = [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Rem,
            BinaryOp::Eq,
            BinaryOp::NotEq,
            BinaryOp::LessThan,
            BinaryOp::GreaterThan,
            BinaryOp::LessThanEq,
            BinaryOp::GreaterThanEq,
        ]
        .map(|op| {
            ExternMemberSelector::Operator(ExternOperator::Binary {
                op,
                self_on_right: false,
            })
        });
        let keys = unary
            .into_iter()
            .chain(binary)
            .map(|selector| ExternMemberKey {
                owner: owner.clone(),
                selector,
            });

        assert_eq!(keys.collect::<HashSet<_>>().len(), 12);
    }

    #[test]
    fn operators_report_return_requirements() {
        for op in [
            BinaryOp::Eq,
            BinaryOp::NotEq,
            BinaryOp::LessThan,
            BinaryOp::GreaterThan,
            BinaryOp::LessThanEq,
            BinaryOp::GreaterThanEq,
        ] {
            assert_eq!(
                ExternOperator::Binary {
                    op,
                    self_on_right: false,
                }
                .return_requirement(),
                OperatorReturn::Bool
            );
        }

        for op in [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Rem,
        ] {
            assert_eq!(
                ExternOperator::Binary {
                    op,
                    self_on_right: false,
                }
                .return_requirement(),
                OperatorReturn::NonVoid
            );
        }

        assert_eq!(
            ExternOperator::Unary(UnaryOp::Neg).return_requirement(),
            OperatorReturn::NonVoid
        );
    }

    #[test]
    fn binding_op_is_identity() {
        let field = ExternMemberKey {
            owner: ty(module(&["gfx"]), "Sprite"),
            selector: ExternMemberSelector::Field("x".to_string()),
        };
        let get = ExternBindingKey {
            target: ExternBindingTarget::Member(field.clone()),
            operation: ExternBindingOp::Get,
        };
        let set = ExternBindingKey {
            target: ExternBindingTarget::Member(field),
            operation: ExternBindingOp::Set,
        };

        assert_ne!(get, set);
    }
}
