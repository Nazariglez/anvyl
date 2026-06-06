use anvyx_frontend::air::{
    CallArg, ExternId, FunctionId, ParamMode, Program, RValue, TypeId, TypePassClass,
    TypePassClasses, VerifiedProgram,
};

use super::vir::{VirCall, VirExtern, VirFunction, VirParam, VirProgram};

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
                            param: param.param_type(),
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
        let externs = self
            .program
            .externs
            .iter()
            .enumerate()
            .map(|(index, decl)| VirExtern {
                source: ExternId::from_index(index),
                params: decl.call_params().collect(),
                ret: decl.return_type,
            })
            .collect();
        VirProgram { functions, externs }
    }

    fn compile_rvalue_call(&mut self, function: FunctionId, value: &RValue) -> Option<VirCall> {
        let RValue::Call { callee, args } = value else {
            return None;
        };
        let args = args
            .iter()
            .map(|arg| {
                if let CallArg::Value(operand) = arg
                    && let Some(ty) = self.program.operand_ty(operand)
                    && !self.is_cheap(ty)
                {
                    self.push(function, VmCompileErrorKind::NonCheapValueArg);
                }
                arg.clone()
            })
            .collect();
        Some(VirCall {
            callee: callee.clone(),
            args,
        })
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
