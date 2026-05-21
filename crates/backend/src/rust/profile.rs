use anvyx_frontend::air::{
    Callee, ConstId, ConstValue, ExternDecl, ExternId, ExternMember, Function, FunctionId,
    FunctionKind, Local, LocalId, LocalKind, Module, Operand, Param, ParamRole, Place, Program,
    RValue, Statement, Terminator, TypeData, TypeId, VerifiedProgram,
};

#[derive(Debug, Clone, Copy, Default)]
pub struct RustBackendProfile;

impl RustBackendProfile {
    pub fn check(program: VerifiedProgram<'_>) -> Result<(), Vec<RustBackendProfileError>> {
        let mut cx = ProfileCx {
            program: program.program(),
            errors: vec![],
        };
        cx.check();
        if cx.errors.is_empty() {
            Ok(())
        } else {
            Err(cx.errors)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RustBackendProfileError {
    pub site: ProfileSite,
    pub kind: ProfileErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProfileSite {
    Entry,
    Type(TypeId),
    Const(ConstId),
    Module(usize),
    Function(FunctionId),
    Extern(ExternId),
    Local(FunctionId, LocalId),
    Param(FunctionId, usize),
    Statement(FunctionId, usize, usize),
    Terminator(FunctionId, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProfileErrorKind {
    UnsupportedType,
    UnsupportedConst,
    UnsupportedModuleItem,
    UnsupportedFunctionKind,
    UnsupportedParamRole,
    UnsupportedLocalKind,
    UnsupportedPlaceProjection,
    UnsupportedTerminator,
    UnsupportedRValue,
    UnsupportedCallee,
    UnsupportedExtern,
    UnsupportedExternMember,
    UnsupportedEntry,
}

struct ProfileCx<'a> {
    program: &'a Program,
    errors: Vec<RustBackendProfileError>,
}

impl ProfileCx<'_> {
    fn check(&mut self) {
        self.check_entry();
        for index in 0..self.program.type_arena.len() {
            self.check_type(TypeId::from_index(index));
        }
        for index in 0..self.program.const_arena.len() {
            self.check_const(ConstId::from_index(index));
        }
        for (index, module) in self.program.modules.iter().enumerate() {
            self.check_module(index, module);
        }
        for index in 0..self.program.externs.len() {
            let id = ExternId::from_index(index);
            self.check_extern(id, self.program.extern_decl(id));
        }
        for index in 0..self.program.functions.len() {
            let id = FunctionId::from_index(index);
            self.check_function(id, self.program.function(id));
        }
    }

    fn check_entry(&mut self) {
        let Some(entry) = self.program.entry() else {
            return;
        };
        let function = self.program.function(entry);
        if !function.signature.params.is_empty() {
            self.push(ProfileSite::Entry, ProfileErrorKind::UnsupportedEntry);
        }
    }

    fn check_type(&mut self, id: TypeId) {
        if !type_is_slice1(self.program.type_arena.data(id)) {
            self.push(ProfileSite::Type(id), ProfileErrorKind::UnsupportedType);
        }
    }

    fn check_const(&mut self, id: ConstId) {
        if !const_is_slice1(self.program, id) {
            self.push(ProfileSite::Const(id), ProfileErrorKind::UnsupportedConst);
        }
    }

    fn check_module(&mut self, index: usize, module: &Module) {
        if !module.aggregates.is_empty()
            || !module.enums.is_empty()
            || !module.extern_types.is_empty()
        {
            self.push(
                ProfileSite::Module(index),
                ProfileErrorKind::UnsupportedModuleItem,
            );
        }
    }

    fn check_function(&mut self, id: FunctionId, function: &Function) {
        if function.kind != FunctionKind::Normal {
            self.push(
                ProfileSite::Function(id),
                ProfileErrorKind::UnsupportedFunctionKind,
            );
        }
        self.check_type_ref(ProfileSite::Function(id), function.signature.return_type);
        for (index, param) in function.signature.params.iter().enumerate() {
            self.check_param(id, index, param);
        }
        for index in 0..function.locals.len() {
            let local = LocalId::from_index(index);
            self.check_local(id, local, &function.locals[index]);
        }
        for (block_index, block) in function.body.iter().enumerate() {
            for (statement_index, statement) in block.statements.iter().enumerate() {
                self.check_statement(id, block_index, statement_index, statement);
            }
            self.check_terminator(id, block_index, &block.terminator);
        }
    }

    fn check_param(&mut self, function: FunctionId, index: usize, param: &Param) {
        if param.role != ParamRole::Normal {
            self.push(
                ProfileSite::Param(function, index),
                ProfileErrorKind::UnsupportedParamRole,
            );
        }
        self.check_type_ref(ProfileSite::Param(function, index), param.ty);
    }

    fn check_local(&mut self, function: FunctionId, local: LocalId, data: &Local) {
        if !matches!(
            data.kind,
            LocalKind::Arg | LocalKind::Temp | LocalKind::User
        ) {
            self.push(
                ProfileSite::Local(function, local),
                ProfileErrorKind::UnsupportedLocalKind,
            );
        }
        self.check_type_ref(ProfileSite::Local(function, local), data.ty);
    }

    fn check_statement(
        &mut self,
        function: FunctionId,
        block: usize,
        statement: usize,
        data: &Statement,
    ) {
        let site = ProfileSite::Statement(function, block, statement);
        match data {
            Statement::Init { value, .. } | Statement::Eval(value) => {
                self.check_rvalue(site, value)
            }
            Statement::Assign { dst, value } => {
                self.check_rvalue(site, value);
                self.check_place(site, dst);
            }
        }
    }

    fn check_terminator(&mut self, function: FunctionId, block: usize, terminator: &Terminator) {
        match terminator {
            Terminator::Return(value) => {
                if let Some(value) = value {
                    self.check_operand(ProfileSite::Terminator(function, block), value);
                }
            }
            Terminator::Goto(_)
            | Terminator::If { .. }
            | Terminator::SwitchEnum { .. }
            | Terminator::Unreachable => {
                self.push(
                    ProfileSite::Terminator(function, block),
                    ProfileErrorKind::UnsupportedTerminator,
                );
            }
        }
    }

    fn check_rvalue(&mut self, site: ProfileSite, value: &RValue) {
        match value {
            RValue::Use(operand) => self.check_operand(site, operand),
            RValue::Unary { value, ty, .. } => {
                self.check_operand(site, value);
                self.check_type_ref(site, *ty);
            }
            RValue::Binary { lhs, rhs, ty, .. } => {
                self.check_operand(site, lhs);
                self.check_operand(site, rhs);
                self.check_type_ref(site, *ty);
            }
            RValue::Call { callee, args } => {
                self.check_callee(site, callee);
                for arg in args {
                    self.check_operand(site, arg);
                }
            }
            RValue::Cast { value, target } => {
                self.check_operand(site, value);
                self.check_type_ref(site, *target);
            }
            RValue::Stringify { value, source_ty } => {
                self.check_operand(site, value);
                if !stringify_source_is_slice1(self.program.type_arena.data(*source_ty)) {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                self.check_type_ref(site, *source_ty);
            }
            RValue::SharedRefEq { .. }
            | RValue::Aggregate { .. }
            | RValue::Format { .. }
            | RValue::Len { .. }
            | RValue::ListPush { .. }
            | RValue::ListPop { .. }
            | RValue::ListSlice { .. }
            | RValue::MapGet { .. }
            | RValue::MapInsert { .. }
            | RValue::MapRemove { .. }
            | RValue::MapEntryAt { .. }
            | RValue::SliceView { .. }
            | RValue::MakeClosure { .. } => {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
            }
        }
    }

    fn check_callee(&mut self, site: ProfileSite, callee: &Callee) {
        match callee {
            Callee::Function(_) => {}
            Callee::Extern(id) if extern_is_slice1_runtime(self.program, *id) => {}
            Callee::Extern(_) | Callee::Closure(_) => {
                self.push(site, ProfileErrorKind::UnsupportedCallee);
            }
        }
    }

    fn check_operand(&mut self, site: ProfileSite, operand: &Operand) {
        match operand {
            Operand::Place(place) => self.check_place(site, place),
            Operand::Const(id) => self.check_const_ref(site, *id),
        }
    }

    fn check_place(&mut self, site: ProfileSite, place: &Place) {
        if !place.projection.is_empty() {
            self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
        }
        self.check_type_ref(site, place.ty);
    }

    fn check_extern(&mut self, id: ExternId, decl: &ExternDecl) {
        if decl.member != ExternMember::FreeFunction {
            self.push(
                ProfileSite::Extern(id),
                ProfileErrorKind::UnsupportedExternMember,
            );
        }
        if !extern_is_slice1_runtime(self.program, id) {
            self.push(ProfileSite::Extern(id), ProfileErrorKind::UnsupportedExtern);
        }
    }

    fn check_type_ref(&mut self, site: ProfileSite, ty: TypeId) {
        if !type_is_slice1(self.program.type_arena.data(ty)) {
            self.push(site, ProfileErrorKind::UnsupportedType);
        }
    }

    fn check_const_ref(&mut self, site: ProfileSite, id: ConstId) {
        if !const_is_slice1(self.program, id) {
            self.push(site, ProfileErrorKind::UnsupportedConst);
        }
    }

    fn push(&mut self, site: ProfileSite, kind: ProfileErrorKind) {
        self.errors.push(RustBackendProfileError { site, kind });
    }
}

fn type_is_slice1(ty: &TypeData) -> bool {
    scalar_type_is_slice1(ty) || matches!(ty, TypeData::Void)
}

fn stringify_source_is_slice1(ty: &TypeData) -> bool {
    scalar_type_is_slice1(ty)
}

fn scalar_type_is_slice1(ty: &TypeData) -> bool {
    matches!(
        ty,
        TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String
    )
}

fn const_is_slice1(program: &Program, id: ConstId) -> bool {
    let konst = program.const_arena.get(id);
    matches!(
        (program.type_arena.data(konst.ty), &konst.value),
        (TypeData::Int, ConstValue::Int(_))
            | (TypeData::Float, ConstValue::Float(_))
            | (TypeData::Bool, ConstValue::Bool(_))
            | (TypeData::String, ConstValue::String(_))
    )
}

fn extern_is_slice1_runtime(program: &Program, id: ExternId) -> bool {
    let decl = program.extern_decl(id);
    if decl.member != ExternMember::FreeFunction {
        return false;
    }
    let module = program.module(decl.module);
    let [segment] = module.path.as_slice() else {
        return false;
    };
    if segment.as_str() != "core_runtime" {
        return false;
    }
    match decl.name.as_str() {
        "_println" => extern_signature_is(program, decl, &[TypeData::String], TypeData::Void),
        "_assert" => extern_signature_is(
            program,
            decl,
            &[TypeData::Bool, TypeData::String],
            TypeData::Void,
        ),
        _ => false,
    }
}

fn extern_signature_is(
    program: &Program,
    decl: &ExternDecl,
    params: &[TypeData],
    ret: TypeData,
) -> bool {
    decl.params.len() == params.len()
        && decl
            .params
            .iter()
            .map(|ty| program.type_arena.data(*ty))
            .eq(params)
        && program.type_arena.data(decl.return_type) == &ret
}
