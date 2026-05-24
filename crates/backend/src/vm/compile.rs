use anvyx_frontend::air::{
    CallArg, FunctionId, Operand, ParamMode, Program, RValue, TypeId, TypePassClass,
    TypePassClasses, VerifiedProgram,
};

use super::vir::{VirCall, VirCallArg, VirFunction, VirParam, VirProgram};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VmCompiler;

impl VmCompiler {
    pub fn compile(program: VerifiedProgram<'_>) -> Result<VirProgram, Vec<VmCompileError>> {
        let mut cx = CompileCx {
            program: program.program(),
            classes: TypePassClasses::analyze(program.program()),
            errors: vec![],
        };
        let vir = cx.compile();
        if cx.errors.is_empty() {
            Ok(vir)
        } else {
            Err(cx.errors)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VmCompileError {
    pub function: FunctionId,
    pub kind: VmCompileErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmCompileErrorKind {
    NonCheapValueParam,
    NonCheapValueArg,
}

struct CompileCx<'a> {
    program: &'a Program,
    classes: TypePassClasses,
    errors: Vec<VmCompileError>,
}

impl CompileCx<'_> {
    fn compile(&mut self) -> VirProgram {
        let functions = self
            .program
            .functions
            .iter()
            .enumerate()
            .map(|(index, function)| {
                let id = FunctionId::from_index(index);
                let params = function
                    .signature
                    .params
                    .iter()
                    .map(|param| {
                        if param.mode == ParamMode::Value && !self.is_cheap(param.ty) {
                            self.push(id, VmCompileErrorKind::NonCheapValueParam);
                        }
                        VirParam {
                            local: param.local_id,
                            ty: param.ty,
                            mode: param.mode,
                        }
                    })
                    .collect();
                let mut calls = vec![];
                function.body.for_each_rvalue(&mut |value| {
                    if let Some(call) = self.compile_rvalue_call(id, value) {
                        calls.push(call);
                    }
                });
                VirFunction {
                    source: id,
                    params,
                    calls,
                }
            })
            .collect();
        VirProgram { functions }
    }

    fn compile_rvalue_call(&mut self, function: FunctionId, value: &RValue) -> Option<VirCall> {
        let RValue::Call { callee, args } = value else {
            return None;
        };
        let args = args
            .iter()
            .map(|arg| {
                if let CallArg::Value(operand) = arg
                    && let Some(ty) = self.operand_ty(operand)
                    && !self.is_cheap(ty)
                {
                    self.push(function, VmCompileErrorKind::NonCheapValueArg);
                }
                VirCallArg::from(arg)
            })
            .collect();
        Some(VirCall {
            callee: callee.clone(),
            args,
        })
    }

    fn operand_ty(&self, operand: &Operand) -> Option<TypeId> {
        match operand {
            Operand::Place(place) => Some(place.ty),
            Operand::Const(id) => self
                .program
                .const_arena
                .get_checked(*id)
                .map(|data| data.ty),
        }
    }

    fn is_cheap(&self, ty: TypeId) -> bool {
        self.classes
            .get(ty)
            .is_some_and(TypePassClass::is_cheap_value)
    }

    fn push(&mut self, function: FunctionId, kind: VmCompileErrorKind) {
        self.errors.push(VmCompileError { function, kind });
    }
}
