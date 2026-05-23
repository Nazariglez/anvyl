use anvyx_frontend::air::{Operand, Place};

use super::vir::{VirCall, VirCallArg, VirParam, VirParamMode};

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub bindings: Vec<ArgBinding>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgBinding {
    Value(Operand),
    SharedBorrow(Place),
    MutBorrow(Place),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindError {
    Arity,
    Mode,
}

impl CallFrame {
    pub fn bind(params: &[VirParam], call: &VirCall) -> Result<Self, BindError> {
        if params.len() != call.args.len() {
            return Err(BindError::Arity);
        }
        let bindings = params
            .iter()
            .zip(&call.args)
            .map(bind_arg)
            .collect::<Result<_, _>>()?;
        Ok(Self { bindings })
    }

    pub fn mut_borrows(&self) -> impl Iterator<Item = &Place> {
        self.bindings.iter().filter_map(|binding| match binding {
            ArgBinding::MutBorrow(place) => Some(place),
            ArgBinding::Value(_) | ArgBinding::SharedBorrow(_) => None,
        })
    }
}

fn bind_arg((param, arg): (&VirParam, &VirCallArg)) -> Result<ArgBinding, BindError> {
    match (param.mode, arg) {
        (VirParamMode::Value, VirCallArg::Value(operand)) => Ok(ArgBinding::Value(operand.clone())),
        (VirParamMode::SharedBorrow, VirCallArg::SharedBorrow(place)) => {
            Ok(ArgBinding::SharedBorrow(place.clone()))
        }
        (VirParamMode::MutBorrow, VirCallArg::MutBorrow(place)) => {
            Ok(ArgBinding::MutBorrow(place.clone()))
        }
        _ => Err(BindError::Mode),
    }
}
