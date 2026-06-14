use anvyx_frontend::air::{
    AirBlock, AirStmt, AirTail, CallArg, Callee, ExternId, FunctionId, FunctionKind, GlobalId,
    LambdaCaptureArg, Operand, ParamMode, Place, PlaceRoot, Program, RValue, TypeData, TypeId,
    TypePassClass, TypePassClasses, VerifiedProgram,
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
    pub site: VmCompileErrorSite,
    pub kind: VmCompileErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmCompileErrorSite {
    Function(FunctionId),
    Global(GlobalId),
    Extern(ExternId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmCompileErrorKind {
    UnsupportedLambdaType,
    UnsupportedLambdaValue,
    UnsupportedLambdaCall,
    UnsupportedLambdaCapture,
    UnsupportedLambdaCell,
    UnsupportedLambdaExternBoundary,
    UnsupportedCollectionLoan,
    UnsupportedGlobal,
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
        for index in 0..self.program.globals.len() {
            self.push_global(
                GlobalId::from_index(index),
                VmCompileErrorKind::UnsupportedGlobal,
            );
        }
        let functions = self
            .program
            .functions
            .iter()
            .enumerate()
            .map(|(index, function)| {
                let id = FunctionId::from_index(index);
                match function.kind {
                    FunctionKind::Lambda(_) => {
                        self.push_function(id, VmCompileErrorKind::UnsupportedLambdaValue);
                    }
                    FunctionKind::GlobalInit(_) => {
                        self.push_function(id, VmCompileErrorKind::UnsupportedGlobal);
                    }
                    FunctionKind::Normal
                    | FunctionKind::Method
                    | FunctionKind::ExtendMethod
                    | FunctionKind::Helper => {}
                }
                if self.type_contains_function(function.signature.return_type()) {
                    self.push_function(id, VmCompileErrorKind::UnsupportedLambdaType);
                }
                let params = function
                    .signature
                    .params
                    .iter()
                    .map(|param| {
                        if self.type_contains_function(param.ty) {
                            self.push_function(id, VmCompileErrorKind::UnsupportedLambdaType);
                        }
                        if param.mode == ParamMode::Value && !self.is_cheap(param.ty) {
                            self.push_function(id, VmCompileErrorKind::NonCheapValueParam);
                        }
                        VirParam {
                            local: param.local_id,
                            param: param.param_type(),
                        }
                    })
                    .collect();
                for local in &function.locals {
                    if self.type_contains_function(local.ty) {
                        self.push_function(id, VmCompileErrorKind::UnsupportedLambdaType);
                    }
                }
                let mut calls = vec![];
                self.check_block(id, &function.body.block, &mut calls);
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
            .map(|(index, decl)| {
                let id = ExternId::from_index(index);
                if decl
                    .call_params()
                    .any(|param| self.type_contains_function(param.ty))
                    || self.type_contains_function(decl.return_type)
                {
                    self.push_extern(id, VmCompileErrorKind::UnsupportedLambdaExternBoundary);
                }
                VirExtern {
                    source: id,
                    params: decl.call_params().collect(),
                    ret: decl.return_type,
                }
            })
            .collect();
        VirProgram { functions, externs }
    }

    fn check_block(&mut self, function: FunctionId, block: &AirBlock, calls: &mut Vec<VirCall>) {
        for stmt in &block.stmts {
            match stmt {
                AirStmt::Init { value, .. } | AirStmt::Eval(value) => {
                    self.check_rvalue(function, value);
                    if let Some(call) = self.compile_rvalue_call(function, value) {
                        calls.push(call);
                    }
                }
                AirStmt::Assign { dst, value } => {
                    self.check_place(function, dst);
                    self.check_rvalue(function, value);
                    if let Some(call) = self.compile_rvalue_call(function, value) {
                        calls.push(call);
                    }
                }
                AirStmt::GlobalEnsure { .. } => {
                    self.push_function(function, VmCompileErrorKind::UnsupportedGlobal);
                }
                AirStmt::GlobalSetRoot { value, .. } => {
                    self.push_function(function, VmCompileErrorKind::UnsupportedGlobal);
                    self.check_rvalue(function, value);
                    if let Some(call) = self.compile_rvalue_call(function, value) {
                        calls.push(call);
                    }
                }
                AirStmt::If(branch) => {
                    self.check_operand(function, &branch.cond);
                    self.check_block(function, &branch.then_block, calls);
                    if let Some(block) = &branch.else_block {
                        self.check_block(function, block, calls);
                    }
                }
                AirStmt::Loop(loop_) => self.check_block(function, &loop_.body, calls),
                AirStmt::CollectionLoan(loan) => {
                    self.push_function(function, VmCompileErrorKind::UnsupportedCollectionLoan);
                    self.check_place(function, &loan.root);
                    self.check_block(function, &loan.body, calls);
                }
                AirStmt::CollectionSlotScope(scope) => {
                    self.push_function(function, VmCompileErrorKind::UnsupportedCollectionLoan);
                    self.check_place(function, &scope.root);
                    self.check_block(function, &scope.body, calls);
                }
                AirStmt::EnumMatch(match_) => {
                    self.check_place(function, &match_.discr);
                    for arm in &match_.arms {
                        self.check_block(function, &arm.block, calls);
                    }
                    if let Some(block) = &match_.else_block {
                        self.check_block(function, block, calls);
                    }
                }
                AirStmt::OptionalMatch(match_) => {
                    self.check_place(function, &match_.discr);
                    self.check_block(function, &match_.some_block, calls);
                    self.check_block(function, &match_.none_block, calls);
                }
            }
        }
        if let AirTail::Return(Some(value)) = &block.tail {
            self.check_operand(function, value);
        }
    }

    fn check_operand(&mut self, function: FunctionId, operand: &Operand) {
        if let Operand::Place(place) = operand {
            self.check_place(function, place);
        }
    }

    fn check_place(&mut self, function: FunctionId, place: &Place) {
        match place.root {
            PlaceRoot::CaptureCell(_) => {
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCell);
            }
            PlaceRoot::LambdaCapture(_)
                if self
                    .program
                    .capture_cell_root(function, place.root)
                    .is_some() =>
            {
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCell);
            }
            PlaceRoot::LambdaCapture(_) | PlaceRoot::ScopedBorrow(_) => {
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCapture);
            }
            PlaceRoot::Global(_) => {
                self.push_function(function, VmCompileErrorKind::UnsupportedGlobal);
            }
            PlaceRoot::Local(_) => {}
        }
    }

    fn check_rvalue(&mut self, function: FunctionId, value: &RValue) {
        match value {
            RValue::Use(operand)
            | RValue::Unary { value: operand, .. }
            | RValue::OptionalSome { value: operand, .. }
            | RValue::Cast { value: operand, .. }
            | RValue::Stringify { value: operand, .. }
            | RValue::Format { value: operand, .. } => self.check_operand(function, operand),
            RValue::Binary { lhs, rhs, .. } | RValue::SharedRefEq { lhs, rhs, .. } => {
                self.check_operand(function, lhs);
                self.check_operand(function, rhs);
            }
            RValue::Aggregate { fields, .. } | RValue::StringConcat { parts: fields } => {
                for field in fields {
                    self.check_operand(function, field);
                }
            }
            RValue::Call { callee, args } => {
                self.check_callee(function, callee);
                for arg in args {
                    self.check_call_arg(function, arg);
                }
            }
            RValue::Len { source } | RValue::ListPop { list: source, .. } => {
                self.check_place(function, source);
            }
            RValue::ListPush { list, value } => {
                self.check_place(function, list);
                self.check_operand(function, value);
            }
            RValue::RangeListCopy { source, .. } | RValue::SliceView { source, .. } => {
                self.check_place(function, source);
            }
            RValue::MapGet { map, key, .. } | RValue::MapRemove { map, key, .. } => {
                self.check_place(function, map);
                self.check_operand(function, key);
            }
            RValue::MapInsert {
                map,
                key,
                value,
                kind: _,
            } => {
                self.check_place(function, map);
                self.check_operand(function, key);
                self.check_operand(function, value);
            }
            RValue::MapEntryAt { map, .. } => self.check_place(function, map),
            RValue::FunctionRef { .. } => {
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaValue);
            }
            RValue::MakeLambda { captures, .. } => {
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaValue);
                for capture in captures {
                    self.check_lambda_capture(function, capture);
                }
            }
        }
    }

    fn check_callee(&mut self, function: FunctionId, callee: &Callee) {
        if let Callee::Lambda(operand) = callee {
            self.check_operand(function, operand);
            self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCall);
        }
    }

    fn check_call_arg(&mut self, function: FunctionId, arg: &CallArg) {
        match arg {
            CallArg::Value(operand) => self.check_operand(function, operand),
            CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
                self.check_place(function, place);
            }
            CallArg::SharedStringConst(_) => {}
        }
    }

    fn check_lambda_capture(&mut self, function: FunctionId, capture: &LambdaCaptureArg) {
        match capture {
            LambdaCaptureArg::ReadonlyLocal { value } => {
                self.check_operand(function, value);
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCapture);
            }
            LambdaCaptureArg::ScopedLocal { place } | LambdaCaptureArg::ScopedBorrow { place } => {
                self.check_place(function, place);
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCapture);
            }
            LambdaCaptureArg::CaptureCell { .. } => {
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCell);
            }
            LambdaCaptureArg::NoRuntime => {}
        }
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
                {
                    if self.type_contains_function(ty) {
                        self.push_function(function, VmCompileErrorKind::UnsupportedLambdaValue);
                    }
                    if !self.is_cheap(ty) {
                        self.push_function(function, VmCompileErrorKind::NonCheapValueArg);
                    }
                }
                arg.clone()
            })
            .collect();
        Some(VirCall {
            callee: callee.clone(),
            args,
        })
    }

    fn type_contains_function(&self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::Function(_) => true,
            TypeData::Optional(inner)
            | TypeData::Array { elem: inner, .. }
            | TypeData::Slice(inner) => self.type_contains_function(*inner),
            TypeData::List(elem) => self.type_contains_function(*elem),
            TypeData::Map { key, value, .. } => {
                self.type_contains_function(*key) || self.type_contains_function(*value)
            }
            TypeData::Tuple(elems) => elems.iter().any(|elem| self.type_contains_function(*elem)),
            _ => false,
        }
    }

    fn is_cheap(&self, ty: TypeId) -> bool {
        self.classes
            .get(ty)
            .is_some_and(TypePassClass::is_cheap_value)
    }

    fn push_function(&mut self, function: FunctionId, kind: VmCompileErrorKind) {
        self.errors.push(VmCompileError {
            site: VmCompileErrorSite::Function(function),
            kind,
        });
    }

    fn push_extern(&mut self, ext: ExternId, kind: VmCompileErrorKind) {
        self.errors.push(VmCompileError {
            site: VmCompileErrorSite::Extern(ext),
            kind,
        });
    }

    fn push_global(&mut self, global: GlobalId, kind: VmCompileErrorKind) {
        self.errors.push(VmCompileError {
            site: VmCompileErrorSite::Global(global),
            kind,
        });
    }
}
