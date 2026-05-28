use anvyx_frontend::air::ExternId;
use anvyx_runtime::{Ctx, RuntimeError};

use super::vir::VirCallArg;

pub trait ExternDispatcher {
    fn call(
        &mut self,
        ctx: &mut Ctx<'_, '_>,
        id: ExternId,
        args: &[VirCallArg],
    ) -> Result<(), RuntimeError>;
}

pub struct NoExterns;

impl ExternDispatcher for NoExterns {
    fn call(
        &mut self,
        _ctx: &mut Ctx<'_, '_>,
        id: ExternId,
        _args: &[VirCallArg],
    ) -> Result<(), RuntimeError> {
        Err(RuntimeError::new(format!(
            "missing VM extern dispatch for extern #{}",
            id.index()
        )))
    }
}

pub fn unsupported_callback(id: ExternId) -> RuntimeError {
    RuntimeError::new(format!(
        "VM callback extern #{} is not supported",
        id.index()
    ))
}
