use std::collections::HashSet;

use anvyx_frontend::air::{
    AirBlock, AirStmt, AirTail, CallArg, Callee, ConstId, ExternId, FunctionId, FunctionKind,
    GlobalId, LambdaCaptureArg, Operand, ParamMode, Place, PlaceRoot, Program, RValue, TypeData,
    TypeId, TypePassClass, TypePassClasses, VariantShape, VerifiedProgram,
};

use super::vir::{
    VirCall, VirCallArg, VirCallTarget, VirExtern, VirFunction, VirParam, VirProgram,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VmCompiler;

impl VmCompiler {
    pub fn compile(program: VerifiedProgram<'_>) -> Result<VirProgram, Vec<VmCompileError>> {
        let air = program.program();
        let mut cx = CompileCx {
            program: air,
            classes: TypePassClasses::analyze(air),
            errors: vec![],
        };
        let vir = cx.compile();
        let errors = std::mem::take(&mut cx.errors);
        drop(cx);
        std::hint::black_box(program);
        if errors.is_empty() {
            Ok(vir)
        } else {
            Err(errors)
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
    UnsupportedRangeFor,
    UnsupportedGlobal,
    UnsupportedNativeInitField,
    NonCheapValueParam,
    NonCheapValueArg,
}

struct CompileCx<'a> {
    program: &'a Program,
    classes: TypePassClasses,
    errors: Vec<VmCompileError>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VmPlaceRootStatus {
    Local,
    LambdaCapture,
    LambdaCellCapture,
    ScopedBorrow,
    CaptureCell,
    Global,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VmCaptureStatus {
    NoRuntime,
    Readonly,
    ScopedLocal,
    ScopedBorrow,
    CaptureCell,
}

impl CompileCx<'_> {
    fn compile(&mut self) -> VirProgram {
        for index in 0..self.program.globals.len() {
            self.push_global_gap(GlobalId::from_index(index));
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
                    FunctionKind::GlobalInit(_) => self.push_global_function_gap(id),
                    FunctionKind::Normal
                    | FunctionKind::Method
                    | FunctionKind::ExtendMethod
                    | FunctionKind::Helper => {}
                }
                if self.contains_function_payload(function.signature.return_type()) {
                    self.push_function(id, VmCompileErrorKind::UnsupportedLambdaType);
                }
                let params = function
                    .signature
                    .params
                    .iter()
                    .map(|param| {
                        if self.contains_function_payload(param.ty) {
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
                    if self.contains_function_payload(local.ty) {
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
                    .any(|param| self.contains_function_payload(param.ty))
                    || self.contains_function_payload(decl.return_type)
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
                    self.inspect_rvalue(function, value, calls);
                }
                AirStmt::Assign { dst, value } => {
                    self.check_place(function, dst);
                    self.inspect_rvalue(function, value, calls);
                }
                AirStmt::GlobalEnsure { .. } => self.push_global_function_gap(function),
                AirStmt::GlobalSetRoot { value, .. } | AirStmt::GlobalUpdateRoot { value, .. } => {
                    self.push_global_function_gap(function);
                    self.inspect_rvalue(function, value, calls);
                }
                AirStmt::If(branch) => {
                    self.check_operand(function, &branch.cond);
                    self.check_block(function, &branch.then_block, calls);
                    if let Some(block) = &branch.else_block {
                        self.check_block(function, block, calls);
                    }
                }
                AirStmt::Loop(loop_) => self.check_block(function, &loop_.body, calls),
                AirStmt::RangeFor(range) => {
                    self.push_function(function, VmCompileErrorKind::UnsupportedRangeFor);
                    self.check_operand(function, &range.start);
                    self.check_operand(function, &range.end);
                    self.check_operand(function, &range.step);
                    self.check_block(function, &range.body, calls);
                }
                AirStmt::CollectionLoan(loan) => {
                    self.push_collection_gap(function);
                    self.check_place(function, &loan.root);
                    self.check_block(function, &loan.body, calls);
                }
                AirStmt::CollectionSlotScope(scope) => {
                    self.push_collection_gap(function);
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
                AirStmt::MapEntryMatch(match_) => {
                    self.push_collection_gap(function);
                    self.check_place(function, &match_.map);
                    self.check_operand(function, &match_.key);
                    self.check_block(function, &match_.some_block, calls);
                    self.check_block(function, &match_.none_block, calls);
                }
            }
        }
        match &block.tail {
            AirTail::None
            | AirTail::Return(None)
            | AirTail::Break(_)
            | AirTail::Continue(_)
            | AirTail::Unreachable => {}
            AirTail::Return(Some(value)) => self.check_operand(function, value),
        }
    }

    fn check_operand(&mut self, function: FunctionId, operand: &Operand) {
        match operand {
            Operand::Place(place) => self.check_place(function, place),
            Operand::Const(_) => {}
        }
    }

    fn check_place(&mut self, function: FunctionId, place: &Place) {
        self.push_place_root_gap(function, self.place_root_status(function, place.root));
    }

    fn inspect_rvalue(&mut self, function: FunctionId, value: &RValue, calls: &mut Vec<VirCall>) {
        match value {
            RValue::Call { .. } => {
                if let Some(call) = self.compile_rvalue_call(function, value) {
                    calls.push(call);
                }
            }
            _ => self.check_rvalue(function, value),
        }
    }

    fn check_rvalue(&mut self, function: FunctionId, value: &RValue) {
        match value {
            RValue::Use(operand)
            | RValue::FunctionValue { value: operand, .. }
            | RValue::Unary { value: operand, .. }
            | RValue::OptionalSome { value: operand, .. }
            | RValue::Cast { value: operand, .. }
            | RValue::Stringify { value: operand, .. }
            | RValue::Format { value: operand, .. }
            | RValue::CheckedForStep { step: operand } => self.check_operand(function, operand),
            RValue::Binary { lhs, rhs, .. } | RValue::SharedRefEq { lhs, rhs, .. } => {
                self.check_operand(function, lhs);
                self.check_operand(function, rhs);
            }
            RValue::Aggregate { fields, .. } | RValue::StringConcat { parts: fields } => {
                for field in fields {
                    self.check_operand(function, field);
                }
            }
            RValue::Call { .. } => {}
            RValue::Len { source }
            | RValue::ListPop { list: source, .. }
            | RValue::RangeListCopy { source, .. }
            | RValue::SliceView { source, .. } => {
                self.check_place(function, source);
            }
            RValue::ListPush { list, value } => {
                self.check_place(function, list);
                self.check_operand(function, value);
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

    fn check_lambda_capture(&mut self, function: FunctionId, capture: &LambdaCaptureArg) {
        match capture {
            LambdaCaptureArg::NoRuntime
            | LambdaCaptureArg::ScopedLocal { .. }
            | LambdaCaptureArg::ScopedBorrow { .. }
            | LambdaCaptureArg::CaptureCell { .. } => {}
            LambdaCaptureArg::ReadonlyLocal { value } => self.check_operand(function, value),
        }
        self.push_capture_gap(function, Self::capture_status(capture));
    }

    fn place_root_status(&self, function: FunctionId, root: PlaceRoot) -> VmPlaceRootStatus {
        match root {
            PlaceRoot::Local(_) => VmPlaceRootStatus::Local,
            PlaceRoot::LambdaCapture(_) => {
                if self.program.capture_cell_root(function, root).is_some() {
                    VmPlaceRootStatus::LambdaCellCapture
                } else {
                    VmPlaceRootStatus::LambdaCapture
                }
            }
            PlaceRoot::ScopedBorrow(_) => VmPlaceRootStatus::ScopedBorrow,
            PlaceRoot::CaptureCell(_) => VmPlaceRootStatus::CaptureCell,
            PlaceRoot::Global(_) => VmPlaceRootStatus::Global,
        }
    }

    fn push_place_root_gap(&mut self, function: FunctionId, status: VmPlaceRootStatus) {
        let kind = match status {
            VmPlaceRootStatus::Local => return,
            VmPlaceRootStatus::LambdaCapture | VmPlaceRootStatus::ScopedBorrow => {
                VmCompileErrorKind::UnsupportedLambdaCapture
            }
            VmPlaceRootStatus::LambdaCellCapture | VmPlaceRootStatus::CaptureCell => {
                VmCompileErrorKind::UnsupportedLambdaCell
            }
            VmPlaceRootStatus::Global => return self.push_global_function_gap(function),
        };
        self.push_function(function, kind);
    }

    fn push_global_function_gap(&mut self, function: FunctionId) {
        self.push_function(function, VmCompileErrorKind::UnsupportedGlobal);
    }

    fn push_global_gap(&mut self, global: GlobalId) {
        self.push_global(global, VmCompileErrorKind::UnsupportedGlobal);
    }

    fn push_collection_gap(&mut self, function: FunctionId) {
        self.push_function(function, VmCompileErrorKind::UnsupportedCollectionLoan);
    }

    fn capture_status(capture: &LambdaCaptureArg) -> VmCaptureStatus {
        match capture {
            LambdaCaptureArg::NoRuntime => VmCaptureStatus::NoRuntime,
            LambdaCaptureArg::ReadonlyLocal { .. } => VmCaptureStatus::Readonly,
            LambdaCaptureArg::ScopedLocal { .. } => VmCaptureStatus::ScopedLocal,
            LambdaCaptureArg::ScopedBorrow { .. } => VmCaptureStatus::ScopedBorrow,
            LambdaCaptureArg::CaptureCell { .. } => VmCaptureStatus::CaptureCell,
        }
    }

    fn push_capture_gap(&mut self, function: FunctionId, status: VmCaptureStatus) {
        let kind = match status {
            VmCaptureStatus::NoRuntime => return,
            VmCaptureStatus::Readonly
            | VmCaptureStatus::ScopedLocal
            | VmCaptureStatus::ScopedBorrow => VmCompileErrorKind::UnsupportedLambdaCapture,
            VmCaptureStatus::CaptureCell => VmCompileErrorKind::UnsupportedLambdaCell,
        };
        self.push_function(function, kind);
    }

    fn compile_rvalue_call(&mut self, function: FunctionId, value: &RValue) -> Option<VirCall> {
        match value {
            RValue::Call { callee, args } => {
                let args = args
                    .iter()
                    .map(|arg| self.compile_call_arg(function, arg))
                    .collect();
                let target = self.supported_call_target(function, callee)?;
                Some(VirCall { target, args })
            }
            _ => None,
        }
    }

    fn compile_call_arg(&mut self, function: FunctionId, arg: &CallArg) -> VirCallArg {
        match arg {
            CallArg::Value(operand) => {
                self.check_operand(function, operand);
                if let Some(ty) = self.program.operand_ty(operand) {
                    if self.contains_function_payload(ty) {
                        self.push_function(function, VmCompileErrorKind::UnsupportedLambdaValue);
                    }
                    if !self.is_cheap(ty) {
                        self.push_function(function, VmCompileErrorKind::NonCheapValueArg);
                    }
                }
                VirCallArg::Value(operand.clone())
            }
            CallArg::InitFieldProvided(operand) => {
                self.check_operand(function, operand);
                self.unsupported_init_field(function)
            }
            CallArg::InitFieldOmitted => self.unsupported_init_field(function),
            CallArg::SharedBorrow(place) => {
                self.check_place(function, place);
                VirCallArg::SharedBorrow(place.clone())
            }
            CallArg::SharedStringConst(id) => VirCallArg::SharedStringConst(*id),
            CallArg::MutBorrow(place) => {
                self.check_place(function, place);
                VirCallArg::MutBorrow(place.clone())
            }
        }
    }

    fn unsupported_init_field(&mut self, function: FunctionId) -> VirCallArg {
        self.push_function(function, VmCompileErrorKind::UnsupportedNativeInitField);
        VirCallArg::Value(Operand::Const(ConstId::from_index(usize::MAX)))
    }

    fn supported_call_target(
        &mut self,
        function: FunctionId,
        callee: &Callee,
    ) -> Option<VirCallTarget> {
        match callee {
            Callee::Function(id) => Some(VirCallTarget::Function(*id)),
            Callee::Extern(id) => Some(VirCallTarget::Extern(*id)),
            Callee::Lambda(operand) => {
                self.check_operand(function, operand);
                self.push_function(function, VmCompileErrorKind::UnsupportedLambdaCall);
                None
            }
        }
    }

    fn contains_function_payload(&self, ty: TypeId) -> bool {
        self.contains_function_payload_inner(ty, &mut HashSet::new())
    }

    fn contains_function_payload_inner(&self, ty: TypeId, visited: &mut HashSet<TypeId>) -> bool {
        if !visited.insert(ty) {
            return false;
        }
        match self.program.type_arena.data(ty) {
            TypeData::Function(_) => true,
            TypeData::Optional(inner)
            | TypeData::Array { elem: inner, .. }
            | TypeData::Slice(inner)
            | TypeData::List(inner) => self.contains_function_payload_inner(*inner, visited),
            TypeData::Map { key, value, .. } => {
                self.contains_function_payload_inner(*key, visited)
                    || self.contains_function_payload_inner(*value, visited)
            }
            TypeData::Tuple(elems) => elems
                .iter()
                .any(|elem| self.contains_function_payload_inner(*elem, visited)),
            TypeData::Aggregate(id) | TypeData::DataRef(id) => self
                .program
                .aggregate(*id)
                .fields
                .iter()
                .any(|field| self.contains_function_payload_inner(field.ty, visited)),
            TypeData::Enum(id) => {
                self.program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .any(|variant| match &variant.shape {
                        VariantShape::Unit => false,
                        VariantShape::Tuple(fields) => fields
                            .iter()
                            .any(|field| self.contains_function_payload_inner(*field, visited)),
                        VariantShape::Struct(fields) => fields
                            .iter()
                            .any(|field| self.contains_function_payload_inner(field.ty, visited)),
                    })
            }
            TypeData::Extern(id) => self
                .program
                .extern_type(*id)
                .fields
                .iter()
                .any(|field| self.contains_function_payload_inner(field.ty, visited)),
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::String
            | TypeData::Void
            | TypeData::Any
            | TypeData::Dyn(_) => false,
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
