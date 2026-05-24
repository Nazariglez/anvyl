use anvyx_frontend::air::{ParamMode, Place};

use super::vir::{VirCall, VirCallArg, VirParam};

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub bindings: Vec<VirCallArg>,
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
            VirCallArg::MutBorrow(place) => Some(place),
            VirCallArg::Value(_)
            | VirCallArg::SharedBorrow(_)
            | VirCallArg::SharedStringConst(_) => None,
        })
    }
}

fn bind_arg((param, arg): (&VirParam, &VirCallArg)) -> Result<VirCallArg, BindError> {
    match (param.mode, arg) {
        (ParamMode::Value, VirCallArg::Value(_))
        | (
            ParamMode::SharedBorrow,
            VirCallArg::SharedBorrow(_) | VirCallArg::SharedStringConst(_),
        )
        | (ParamMode::MutBorrow, VirCallArg::MutBorrow(_)) => Ok(arg.clone()),
        _ => Err(BindError::Mode),
    }
}
